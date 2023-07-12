module Main where

import Data.Maybe
import ChessEngine.Board
import ChessEngine.PositionEval
import Control.Monad
import Data.Time.Clock
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent

data EngineState = EngineState
    { board :: !(Maybe ChessBoard)
    , evalTimeLimit :: !(Maybe UTCTime)
    , evalNodeLimit :: !(Maybe Int)
    , result :: !(Maybe EvaluateResult)
    }

blank :: EngineState
blank = EngineState{ board = Nothing, evalTimeLimit = Nothing, evalNodeLimit = Nothing, result = Nothing }

main :: IO ()
main = do
    commandsBuffer <- newTChanIO
    forkIO $ handleCommands commandsBuffer blank
    bufferCommands commandsBuffer

handleCommands :: TChan String -> EngineState -> IO ()
handleCommands commandBuffer state = do
    empty <- chanEmpty commandBuffer
    (output, newState) <- if (empty && (board state) /= Nothing)
                          then do
                              now <- getCurrentTime
                              return $ resumeThinking state now
                          else do
                              line <- atomically $ readTChan commandBuffer
                              return $ doHandleCommand line state
    forM_ output putStrLn
    handleCommands commandBuffer newState

bufferCommands :: TChan String -> IO ()
bufferCommands commandsBuffer = do
    line <- getLine 
    atomically $ writeTChan commandsBuffer line
    if line == "quit"
    then return ()
    else bufferCommands commandsBuffer

doHandleCommand :: String -> EngineState -> ([String], EngineState)
doHandleCommand "uci" state = 
    ([ "id name ArvyyChessEngine", "uciok"], state)

doHandleCommand "isready" state = (["readyok"], state)

doHandleCommand _ state = ([], state)

chanEmpty :: TChan a -> IO Bool
chanEmpty chan = atomically $ do
    content <- tryPeekTChan chan
    let isEmpty = case content of
                    Nothing -> True
                    _ -> False
    return isEmpty

resumeThinking :: EngineState -> UTCTime -> ([String], EngineState)
resumeThinking EngineState { result = Nothing } _ = ([], EngineState{ board = Nothing, evalTimeLimit = Nothing, evalNodeLimit = Nothing, result = Nothing })
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
       else ([], state { result = Just continuation })
    where
        EngineState { result = Just evalResult, evalTimeLimit = evalTimeLimit, evalNodeLimit = evalNodeLimit } = state
        EvaluateResult { nodesParsed = nodesParsed, finished = finished, continuation = continuation } = evalResult

yieldThinkResult :: EngineState -> ([String], EngineState)
yieldThinkResult state = (bestMove, blank)
    where
        EngineState { result = Just evalResult } = state
        EvaluateResult { moves = moves } = evalResult
        bestMove = case moves of
                    [] -> [] -- should never happen
                    (m:_) -> case moveToString m of
                                Just str -> ["bestmove " ++ str]
                                Nothing -> []
