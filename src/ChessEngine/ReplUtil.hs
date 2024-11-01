module ChessEngine.ReplUtil where

import ChessEngine.Board
import ChessEngine.CandidateMoves
import ChessEngine.HeuristicEvaluator (finalDepthEvalExplained)
import Control.Monad (forM_)
import Data.Maybe (isJust, fromJust)
import Data.Foldable (foldl')

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
