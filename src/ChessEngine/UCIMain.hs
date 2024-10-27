module Main where

import ChessEngine.Board
import ChessEngine.PositionEval
import ChessEngine.TimeManager
import ChessEngine.UCI
import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception (SomeException, try)
import Control.Exception.Base (catch)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM
import Control.Monad.Trans.Maybe (hoistMaybe, runMaybeT)
import Data.IORef
import Data.List (intercalate)
import Data.Maybe
import Data.Time.Clock
import Debug.Trace
import System.Exit (exitSuccess)
import System.IO

-- TODO cleanup structure, separate persistent state (engineStateShowDebug) from others
data EngineState = EngineState
  { board :: !(Maybe ChessBoard),
    evalTimeLimit :: !(Maybe UTCTime),
    evalNodeLimit :: !(Maybe Int),
    result :: !(Maybe (IORef EvaluationContext)),
    workerThreadId :: Maybe ThreadId,
    killerThreadId :: Maybe ThreadId,
    engineStateShowDebug :: !Bool,
    engineStateWorkerThreads :: !Int
  }

blank :: EngineState
blank =
  EngineState
    { board = Nothing,
      evalTimeLimit = Nothing,
      evalNodeLimit = Nothing,
      result = Nothing,
      workerThreadId = Nothing,
      killerThreadId = Nothing,
      engineStateShowDebug = False,
      engineStateWorkerThreads = 1
    }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  commandsBuffer <- newTChanIO
  forkIO $ bufferCommands commandsBuffer
  stateRef <- newIORef blank
  handleCommands commandsBuffer stateRef

handleCommands :: TChan UCICommand -> IORef EngineState -> IO ()
handleCommands commandBuffer stateRef = do
  now <- getCurrentTime
  cmd <- atomically $ readTChan commandBuffer
  case cmd of
    Quit -> do
      hFlush stdout
      exitSuccess
    _ -> doHandleCommand cmd stateRef now
  hFlush stdout
  handleCommands commandBuffer stateRef

bufferCommands :: TChan UCICommand -> IO ()
bufferCommands commandsBuffer = do
  end <- isEOF
  if not end
    then do
      line <- getLine
      let cmd = parseUCICommand line
      case cmd of
        Just c -> do
          atomically $ writeTChan commandsBuffer c
          bufferCommands commandsBuffer
        Nothing -> bufferCommands commandsBuffer
    else forever yield

doHandleCommand :: UCICommand -> IORef EngineState -> UTCTime -> IO ()
doHandleCommand UCI stateRef _ = do
  putStrLn "id name ArvyyChessEngine"
  putStrLn "id author github.com/arvyy"
  putStrLn "option name threads type spin default 1 min 1 max 8"
  putStrLn ""
  putStrLn "uciok"
doHandleCommand IsReady state _ = do
  putStrLn "readyok"
doHandleCommand (Debug enable) stateRef _ = do
  state <- readIORef stateRef
  let state' = state {engineStateShowDebug = enable}
  writeIORef stateRef state'
doHandleCommand (SetOption "threads" (Just n)) stateRef _ = do
  let threadCount = read n :: Int
  state <- readIORef stateRef
  let state' = state {engineStateWorkerThreads = threadCount}
  writeIORef stateRef state'
doHandleCommand (Position board') stateRef _ = do
  state <- readIORef stateRef
  let state' = state {board = Just board'}
  writeIORef stateRef state'
doHandleCommand (Go props) stateRef now = do
  state <- readIORef stateRef
  let depth' = case (depth props) of
        Just d -> d
        Nothing -> 9999
  let explicitDeadline = (moveTime props)
      implicitDeadline = do
        whiteTime' <- whiteTime props
        blackTime' <- blackTime props
        board' <- board state
        return $ computeDeadline (turn board') (fullMoves board') whiteTime' blackTime' (whiteIncrement props) (blackIncrement props)
      deadline =
        if infinite props
          then Nothing
          else explicitDeadline <|> implicitDeadline
  evalResultRef <- newIORef EvaluationContext 
                                { nodesParsed = 0
                                , finished = False
                                , evaluation = PositionEval 0
                                , moves = []
                                , showDebug = (engineStateShowDebug state)
                                , workerThreadCount = (engineStateWorkerThreads state)
                                , latestEvaluationInfo = []}

  -- worker thread doing calculation
  workerThreadId' <- forkIO $ do
    let board' = fromJust $ board state
    result <- evaluate evalResultRef board' depth'
    showBestMoveAndClear stateRef result

  -- killer thread killing on deadline
  maybeKillerThreadId <- case deadline of
    Just time ->
      Just
        <$> ( forkIO $ do
                -- time is in miliseconds; threadDelay takes arg in microseconds
                threadDelay (time * 1000)
                state <- readIORef stateRef
                case workerThreadId state of
                  Just threadId -> killThread threadId
                  _ -> return ()
                case result state of
                  Just resultRef -> do
                    res <- readIORef resultRef
                    showBestMoveAndClear stateRef res
                  _ -> return ()
            )
    Nothing -> return Nothing

  let newState =
        state
          { result = Just evalResultRef,
            evalTimeLimit = Nothing,
            evalNodeLimit = (nodes props),
            workerThreadId = Just workerThreadId',
            killerThreadId = maybeKillerThreadId
          }
  writeIORef stateRef newState
  where
    showBestMoveAndClear stateRef result = do
      let EvaluationContext {moves = moves, latestEvaluationInfo = latestEvaluationInfo} = result
      liftIO $ forM_ latestEvaluationInfo putStrLn
      case moves of
        [] -> return ()
        (m : _) -> case moveToString m of
          Just str ->
            putStrLn $ "bestmove " ++ str
          Nothing -> return ()
      hFlush stdout
doHandleCommand _ state _ = return ()

chanEmpty :: TChan a -> IO Bool
chanEmpty chan = atomically $ do
  content <- tryPeekTChan chan
  let isEmpty = case content of
        Nothing -> True
        _ -> False
  return isEmpty
