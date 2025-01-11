module ChessEngine.ReplUtil where

import ChessEngine.Board
import ChessEngine.HeuristicEvaluator (finalDepthEvalExplained)
import Control.Monad (forM_, foldM)
import Data.Foldable (foldl')
import Data.Maybe (fromJust, isJust)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Bits (testBit)
import ChessEngine.PositionEval
import Data.IORef (newIORef)

heuristicEvalExplainPosition :: String -> IO ()
heuristicEvalExplainPosition fen = do
  let (board, _) = case loadFen fen of
        Just v -> v
        _ -> error "Invalid fen"
  (eval, explanation) <- finalDepthEvalExplained board
  putStrLn $ "Evalation: " ++ show eval
  forM_ explanation putStrLn

dividePosition :: String -> Integer -> IO ()
dividePosition fen depth = do
  let (board, _) = case loadFen fen of
        Just v -> v
        _ -> error "Invalid fen"
  printNodes board depth
  where
    printNodes :: ChessBoard -> Integer -> IO ()
    printNodes board depth =
      let candidates = pseudoLegalCandidateMoves board
          validCandidates = filter (\candidate -> isJust $ candidateMoveLegal board candidate) candidates
          candidatesInfo = (\candidate -> (fromJust (moveToString candidate), countNodes (fromJust $ candidateMoveLegal board candidate) (depth - 1))) <$> validCandidates
          totalCount = foldl' (+) 0 (snd <$> candidatesInfo)
          printCandidateInfo (move, count) = putStrLn $ move ++ ": " ++ (show count)
       in do
            forM_ candidatesInfo printCandidateInfo
            putStrLn "========="
            putStrLn $ "Total: " ++ (show totalCount)

    countNodes board depth
      | depth == 0 = 1
      | otherwise =
          let candidates = pseudoLegalCandidateMoves board
              validCandidates = filter (\candidate -> isJust $ candidateMoveLegal board candidate) candidates
              countCandidates count move = count + countNodes (applyMoveUnsafe board move) (depth - 1)
           in foldl' countCandidates 0 validCandidates

debugRenderBitmap :: Int64 -> IO ()
debugRenderBitmap bitmap =
  putStrLn $ intercalate "\n" $ makeLine <$> [1..8]
  where
    makeLine y = intercalate " " $ (\x -> if testBit bitmap (coordsToBitIndex x (9 - y)) then "X" else "O") <$> [1..8]

evalPosition :: String -> IO ()
evalPosition fen = forM_ [3..10] printEvalForDepth
  where
    (board, _) = fromJust $ loadFen fen
    printEvalForDepth depth = do
        contextRef <- newIORef
                  EvaluationContext
                    { nodesParsed = 0,
                      finished = False,
                      evaluation = PositionEval 0,
                      moves = [],
                      showDebug = False,
                      workerThreadCount = 1,
                      latestEvaluationInfo = []
                    }
        EvaluationContext { moves = move : _, evaluation = evaluation } <- evaluate contextRef board depth
        putStrLn $ "Depth " ++ (show depth) ++ ", best move " ++ (show move) ++ ", eval " ++ (show evaluation)
