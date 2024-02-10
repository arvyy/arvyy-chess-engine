{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

import Criterion.Main
import Data.Maybe
import Data.Foldable
import Debug.Trace
import ChessEngine.Board
import ChessEngine.PositionEval

evalPosition :: String -> Int -> PositionEval
evalPosition fen depth =
  let (board, _) = fromJust $ loadFen fen
      result = evaluate board depth
  in loopTilCompletion result
  where
    loopTilCompletion EvaluateResult { finished, evaluation, continuation } = 
        if finished
        then evaluation
        else loopTilCompletion continuation

countNodes fen depth = 
    let (board, _) = fromJust $ loadFen fen
        count = countNodes' board depth
    in trace ("Count for depth " ++ (show depth) ++ ": " ++ (show count)) count

countNodes' board depth
    | depth == 0 = 1
    | otherwise = 
        let candidates = candidateMoves board
            countCandidates count (_, board') = count + countNodes' board' (depth - 1)
        in foldl' countCandidates 0 candidates

main = defaultMain [
  bgroup "position eval"
    [
      bench "Initial pos 5 depth" $ whnf (evalPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") 5
    -- , bench "Midgame pos 4 depth" $ whnf (evalPosition "r1bq1rk1/2p1bppp/pp2pn2/n7/P2P4/5NP1/1PQ1PPBP/RNB2RK1 w - - 0 11") 4
    ],
  bgroup "move generator"
    [
     -- bench "Initial pos 4 deep" $ whnf (countNodes "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") 4
    ]
    ]
