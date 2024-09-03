{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

import ChessEngine.Board
import ChessEngine.PositionEval
import Criterion.Main
import Data.Foldable
import Data.IORef (newIORef)
import Data.Maybe
import Debug.Trace

evalPosition :: String -> Int -> IO PositionEval
evalPosition fen depth = do
  let (board, _) = fromJust $ loadFen fen
  resultRef <- newIORef EvaluateResult {nodesParsed = 0, finished = False, evaluation = PositionEval 0, moves = [], showDebug = False, latestEvaluationInfo = []}
  result <- evaluate resultRef board depth
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

main =
  defaultMain
    [ bgroup
        "position eval"
        [ bench "Initial pos 5 depth" $ whnfIO (evalPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 5),
          bench "Midgame pos 5 depth" $ whnfIO (evalPosition "r1bq1rk1/2p1bppp/pp2pn2/n7/P2P4/5NP1/1PQ1PPBP/RNB2RK1 w - - 0 11" 5)
        ],
      bgroup
        "move generator"
        [ bench "Initial pos 5 deep" $ whnf (countNodes "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") 5
        ]
    ]
