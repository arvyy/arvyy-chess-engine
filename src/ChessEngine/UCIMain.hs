module Main where

import ChessEngine.Board
import ChessEngine.PositionEval
import ChessEngine.UCI
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

data EngineState = EngineState
  { board :: !(Maybe ChessBoard),
    evalTimeLimit :: !(Maybe UTCTime),
    evalNodeLimit :: !(Maybe Int),
    result :: !(Maybe EvaluateResult)
  }
  deriving (Show)

blank :: EngineState
blank = EngineState {board = Nothing, evalTimeLimit = Nothing, evalNodeLimit = Nothing, result = Nothing}

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  commandsBuffer <- newTChanIO
  forkIO $ bufferCommands commandsBuffer
  handleCommands commandsBuffer blank

handleCommands :: TChan UCICommand -> EngineState -> IO ()
handleCommands commandBuffer state = do
  empty <- chanEmpty commandBuffer
  let suspended = case (result state) of
        Just _ -> True
        Nothing -> False
  (output, newState) <-
    if (empty && suspended)
      then do
        now <- getCurrentTime
        return $ resumeThinking state now
      else do
        now <- getCurrentTime
        cmd <- atomically $ readTChan commandBuffer
        case cmd of 
            Quit -> do 
                hFlush stdout
                exitSuccess
            _ -> return $ doHandleCommand cmd state now
  forM_ output putStrLn
  hFlush stdout
  handleCommands commandBuffer newState

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

doHandleCommand :: UCICommand -> EngineState -> UTCTime -> ([String], EngineState)
doHandleCommand UCI state _ = (["id name ArvyyChessEngine", "id author Arvyy", "", "uciok"], state)
doHandleCommand IsReady state _ = (["readyok"], state)
doHandleCommand Stop state _ = yieldThinkResult state
doHandleCommand (Position board) state _ = ([], state {board = Just board})
doHandleCommand (Go props) state now =
  let depth' = case (depth props) of
        Just d -> d
        Nothing -> 9999
      deadline = fmap (\ms -> addUTCTime (realToFrac (fromIntegral ms / 1000.0)) now) (moveTime props)
      initialResult = fmap (\b -> evaluate b depth') (board state)
      newState = state {result = initialResult, evalTimeLimit = deadline, evalNodeLimit = (nodes props)}
   -- in ([], newState)
   in resumeThinking newState now
doHandleCommand _ state _ = ([], state)

chanEmpty :: TChan a -> IO Bool
chanEmpty chan = atomically $ do
  content <- tryPeekTChan chan
  let isEmpty = case content of
        Nothing -> True
        _ -> False
  return isEmpty

resumeThinking :: EngineState -> UTCTime -> ([String], EngineState)
resumeThinking EngineState {result = Nothing} _ = ([], EngineState {board = Nothing, evalTimeLimit = Nothing, evalNodeLimit = Nothing, result = Nothing})
resumeThinking state now =
  let outOfTime = case evalTimeLimit of
        Just time -> now > time
        Nothing -> False
      outOfNodes = case evalNodeLimit of
        Just n -> nodesParsed > n
        Nothing -> False
      yieldResult = finished || outOfTime || outOfNodes
   in if yieldResult
        then yieldThinkResult state
        else ([], state {result = Just continuation})
  where
    EngineState {result = Just evalResult, evalTimeLimit = evalTimeLimit, evalNodeLimit = evalNodeLimit} = state
    EvaluateResult {nodesParsed = nodesParsed, finished = finished, continuation = continuation} = evalResult

yieldThinkResult :: EngineState -> ([String], EngineState)
yieldThinkResult state = (infoCp ++ infoNodes ++ infoCurrLine ++ bestMove, blank)
  where
    EngineState {result = Just evalResult} = state
    EvaluateResult {moves = moves, evaluation = (PositionEval value), nodesParsed = nodesParsed} = evalResult
    infoCp = ["info cp " ++ show (floor (value * 100))]
    infoNodes = ["info nodes " ++ show nodesParsed]
    infoCurrLine = ["info currline 1 " ++ (intercalate " " (mapMaybe moveToString moves))]
    bestMove = case moves of
      [] -> []
      (m : _) -> case moveToString m of
        Just str ->
          ["bestmove " ++ str]
        Nothing -> []
