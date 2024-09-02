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
import System.IO
import System.Exit (exitSuccess)
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (Alternative((<|>)))
import ChessEngine.EvaluatorData
import Control.Exception (catch)
import Control.Exception.Base (SomeException)
import System.Mem

-- TODO cleanup structure, separate persistent state (engineStateShowDebug) from others
data EngineState = EngineState
  { board :: !(Maybe ChessBoard),
    cache :: ChessCache,
    evalTimeLimit :: !(Maybe UTCTime),
    evalNodeLimit :: !(Maybe Int),
    result :: !(Maybe (IORef EvaluateResult)),
    workerThreadId :: Maybe ThreadId,
    killerThreadId :: Maybe ThreadId,
    engineStateShowDebug :: !Bool }

blank :: IO EngineState
blank = do 
    cache <- create
    return EngineState 
        { board = Nothing
        , evalTimeLimit = Nothing
        , evalNodeLimit = Nothing
        , result = Nothing
        , workerThreadId = Nothing
        , killerThreadId = Nothing
        , engineStateShowDebug = False
        , cache = cache
        }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  commandsBuffer <- newTChanIO
  forkIO $ bufferCommands commandsBuffer
  engineState <- blank
  stateRef <- newIORef engineState
  handleCommands commandsBuffer stateRef

handleCommands :: TChan UCICommand -> IORef EngineState -> IO ()
handleCommands commandBuffer stateRef = do
  cmd <- atomically $ readTChan commandBuffer
  case cmd of 
      Quit -> do 
          hFlush stdout
          exitSuccess
      _ -> doHandleCommand cmd stateRef
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

doHandleCommand :: UCICommand -> IORef EngineState -> IO ()
doHandleCommand UCI _ = do 
    putStrLn "id name ArvyyChessEngine"
    putStrLn "id author github.com/arvyy"
    putStrLn ""
    putStrLn "uciok"
doHandleCommand UCINewGame stateRef = do
    EngineState { cache = c } <- readIORef stateRef
    clear c
doHandleCommand IsReady _ = do
    putStrLn "readyok"
doHandleCommand (Debug enable) stateRef = do
    state <- readIORef stateRef
    let state' = state { engineStateShowDebug = enable }
    writeIORef stateRef state'
doHandleCommand (Position board') stateRef = do
    state <- readIORef stateRef
    let state' = state { board = Just board'}
    writeIORef stateRef state'
doHandleCommand Stop stateRef = do
    stopSearch stateRef True
    
doHandleCommand (Go props) stateRef = do
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
  evalResultRef <- newIORef EvaluateResult { nodesParsed = 0, finished = False, evaluation = PositionEval 0, moves = [], showDebug = (engineStateShowDebug state), latestEvaluationInfo = [] }

  -- worker thread doing calculation
  workerThreadId' <- forkIO $ catch (do
                                        let board' = fromJust $ board state
                                        result <- evaluate evalResultRef (cache state) board' depth'
                                        case killerThreadId state of
                                            Just threadId -> killThread threadId
                                            _ -> return ()
                                        let state' = state { killerThreadId = Nothing, workerThreadId = Nothing }
                                        writeIORef stateRef state'
                                        showBestMoveAndClear stateRef result)
                              (\err -> putStrLn $ "info string " ++ (show (err :: SomeException)))

  hFlush stdout
  -- killer thread killing on deadline
  maybeKillerThreadId <- case deadline of 
    Just time -> Just <$> (forkIO $ do
                    -- time is in miliseconds; threadDelay takes arg in microseconds
                    threadDelay (time * 1000)
                    stopSearch stateRef False)
    Nothing -> return Nothing

  let newState = state 
        { result = Just evalResultRef
        , evalTimeLimit = Nothing
        , evalNodeLimit = (nodes props)
        , workerThreadId = Just workerThreadId'
        , killerThreadId = maybeKillerThreadId
        }
  writeIORef stateRef newState

doHandleCommand _ _ = return ()

showBestMoveAndClear :: IORef EngineState -> EvaluateResult -> IO ()
showBestMoveAndClear stateRef result = do
    let EvaluateResult {moves = moves, latestEvaluationInfo = latestEvaluationInfo } = result
    liftIO $ forM_ latestEvaluationInfo putStrLn 
    case moves of
      [] -> putStrLn $ "info string no best move found; state: " ++ show result
      (m : _) -> case moveToString m of
        Just str ->
          putStrLn $ "bestmove " ++ str
        Nothing -> return ()
    hFlush stdout
    performMajorGC

stopSearch :: IORef EngineState -> Bool -> IO ()
stopSearch stateRef killTheKiller = do
    state <- readIORef stateRef
    case workerThreadId state of
        Just threadId -> killThread threadId
        _ -> return ()
    case killerThreadId state of
        Just threadId -> when killTheKiller $ killThread threadId
        _ -> return ()
    case result state of
        Just resultRef -> do
                res <- readIORef resultRef
                showBestMoveAndClear stateRef res
        _ -> return ()
    let state' = state { killerThreadId = Nothing, workerThreadId = Nothing }
    writeIORef stateRef state'

chanEmpty :: TChan a -> IO Bool
chanEmpty chan = atomically $ do
  content <- tryPeekTChan chan
  let isEmpty = case content of
        Nothing -> True
        _ -> False
  return isEmpty
