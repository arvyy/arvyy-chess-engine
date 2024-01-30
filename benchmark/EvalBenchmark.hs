{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

import Criterion.Main
import Data.Maybe
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

main = defaultMain [
  bgroup "position eval"
    [ bench "Initial pos 4 depth" $ whnf (evalPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") 4
    , bench "Midgame pos 4 depth" $ whnf (evalPosition "r1bq1rk1/2p1bppp/pp2pn2/n7/P2P4/5NP1/1PQ1PPBP/RNB2RK1 w - - 0 11") 4]
  ]
