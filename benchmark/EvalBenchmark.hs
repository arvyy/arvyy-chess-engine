{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

import ChessEngine.Board
import ChessEngine.PositionEval
import Criterion.Main
import Data.Foldable
import Data.Maybe
import Debug.Trace
import Data.IORef (newIORef)
import ChessEngine.EvaluatorData (create)
import ChessEngine.EvaluatorData

evalPosition :: ChessCache -> String -> Int -> IO PositionEval
evalPosition cache fen depth = do
  clear cache
  let (board, _) = fromJust $ loadFen fen
  resultRef <- newIORef EvaluateResult { nodesParsed = 0, finished = False, evaluation = PositionEval 0, moves = [], showDebug = False, latestEvaluationInfo = [] }
  result <- evaluate resultRef cache board depth
  return $ evaluation result

countNodes fen depth =
  let (board, _) = fromJust $ loadFen fen
      count = countNodes' board depth
   in trace ("Count for depth " ++ (show depth) ++ ": " ++ (show count)) count

countNodes' board depth
  | depth == 0 = 1
  | otherwise =
      let candidates = pseudoLegalCandidateMoves board
          validCandidates = filter (\candidate -> isJust $ candidateMoveLegal board candidate) candidates
          countCandidates count move = count + countNodes' (applyMoveUnsafe board move) (depth - 1)
       in foldl' countCandidates 0 validCandidates

main = do
  cache <- create
  defaultMain
    [ bgroup
        "position eval"
        [ bench "Initial pos 5 depth" $ whnfIO (evalPosition cache "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 5),
          bench "Midgame pos 5 depth" $ whnfIO (evalPosition cache "r1bq1rk1/2p1bppp/pp2pn2/n7/P2P4/5NP1/1PQ1PPBP/RNB2RK1 w - - 0 11" 5)
        ],
      bgroup
        "move generator"
        [ bench "Initial pos 5 deep" $ whnf (countNodes "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") 5
        ]
    ]
