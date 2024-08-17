module Main where

import ChessEngine.Board
import ChessEngine.PositionEval
import ChessEngine.UCI
import ChessEngine.TimeManager
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Data.Maybe
import Data.Time.Clock
import Debug.Trace
import System.IO
import System.Exit (exitSuccess)
import Data.List (intercalate)
import Data.IORef
import Control.Monad.Trans.Maybe (runMaybeT, hoistMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (Alternative((<|>)))
import Control.Exception (try, SomeException)
import Control.Exception.Base (catch)

data EngineState = EngineState
  { board :: !(Maybe ChessBoard),
    evalTimeLimit :: !(Maybe UTCTime),
    evalNodeLimit :: !(Maybe Int),
    result :: !(Maybe (IORef EvaluateResult)),
    workerThreadId :: Maybe ThreadId,
    killerThreadId :: Maybe ThreadId }

blank :: EngineState
blank = EngineState {board = Nothing, evalTimeLimit = Nothing, evalNodeLimit = Nothing, result = Nothing, workerThreadId = Nothing, killerThreadId = Nothing }

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
    putStrLn ""
    putStrLn "uciok"
doHandleCommand IsReady state _ = do
    putStrLn "readyok"
doHandleCommand (Position board') stateRef _ = do
    state <- readIORef stateRef
    let state' = state { board = Just board'}
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
      deadline = if infinite props
                 then Nothing
                 else explicitDeadline <|> implicitDeadline
  evalResultRef <- newIORef EvaluateResult { nodesParsed = 0, finished = False, evaluation = PositionEval 0, moves = [] }

  -- worker thread doing calculation
  workerThreadId' <- forkIO $ catch
          (do
            let board' = fromJust $ board state
            result <- evaluate evalResultRef board' depth'
            showBestMoveAndClear stateRef result)
          (\e -> putStrLn ("Error: " ++ (show (e :: SomeException))))


  -- killer thread killing on deadline
  maybeKillerThreadId <- case deadline of 
    Just time -> Just <$> (forkIO $ do
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
                    writeIORef stateRef blank)
    Nothing -> return Nothing

  let newState = state 
        { result = Just evalResultRef
        , evalTimeLimit = Nothing
        , evalNodeLimit = (nodes props)
        , workerThreadId = Just workerThreadId'
        , killerThreadId = maybeKillerThreadId
        }
  writeIORef stateRef newState

  where 
    showBestMoveAndClear stateRef result = do
        let EvaluateResult {moves = moves, evaluation = (PositionEval value), nodesParsed = nodesParsed} = result
        case moves of
          [] -> return ()
          (m : _) -> case moveToString m of
            Just str ->
              putStrLn $ "bestmove " ++ str
            Nothing -> return ()
        writeIORef stateRef blank
        

doHandleCommand _ state _ = return ()

chanEmpty :: TChan a -> IO Bool
chanEmpty chan = atomically $ do
  content <- tryPeekTChan chan
  let isEmpty = case content of
        Nothing -> True
        _ -> False
  return isEmpty
