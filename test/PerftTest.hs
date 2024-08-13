module Main where

import ChessEngine.Board
import ChessEngine.PositionEval
import ChessEngine.UCI
import Data.Maybe
import Debug.Trace
import Test.QuickCheck (allProperties)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import ChessEngine.PrecomputedCandidateMoves
import Control.Monad
import Data.Foldable

-- Data taken from https://www.chessprogramming.org/Perft_Results
-- FEN + expected result for depth 5
perftTestData :: [(String, Integer)]
perftTestData = 
  [ ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 4865609)
  , ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ", 193690690)
  , ("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ", 674624)
  , ("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 15833292)
  , ("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8  ", 89941194)
  , ("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10 ", 164075551)
  ]

countNodes board depth
  | depth == 0 = 1
  | otherwise =
      let candidates = pseudoLegalCandidateMoves board
          validCandidates = filter (\candidate -> isJust $ candidateMoveLegal board candidate) candidates
          countCandidates count move = count + countNodes (applyMoveUnsafe board move) (depth - 1)
       in foldl' countCandidates 0 validCandidates

runPerftTest :: (String, Integer) -> Assertion
runPerftTest (fen, expectedCount) = do
    let (board, _) = fromJust $ loadFen fen
    let count = countNodes board 5
    count @?= expectedCount

main = defaultMain $
    testCase "Perft tests" $
        forM_ perftTestData runPerftTest
