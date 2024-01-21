{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck(allProperties)
import Test.Tasty.HUnit

import Data.Maybe

import ChessEngine.Board
import ChessEngine.UCI

-- makes random valid move or returns itself if no moves available
applyRandomMove :: ChessBoard -> Gen ChessBoard
applyRandomMove board = 
    let moves :: [(Move, ChessBoard)]
        moves = candidateMoves board
        boards = map snd moves
    in if (null boards)
       then return board
       else fmap (\index -> boards !! index) $ chooseInt (0, (length boards) - 1)
        

instance Arbitrary ChessBoard where
    arbitrary = do
        moveCount <- chooseInt (0, 100)
        iterate (>>= applyRandomMove) (pure initialBoard) !! moveCount

-- PROPERTY TESTS -------------------------------------

prop_changeturn = 
    \board -> all (\candidate -> case candidate of 
                                    (move, board') -> (turn board) /= (turn board'))
                  $ candidateMoves board

-- EXAMPLE TESTS --------------------------------------

unitTests = testGroup "Unit tests"
  [ testCase "Parse UCI command" $ do
      parseUCICommand "uci" @?= Just UCI
      parseUCICommand "quit" @?= Just Quit
      parseUCICommand "position startpos moves" @?= Just (Position initialBoard)
      parseUCICommand "position startpos moves e2e4" @?= fmap Position (applyMove initialBoard (Move 5 2 5 4 Nothing))
      assertEqual "" True $ isJust $ parseUCICommand "position startpos moves e2e4 a7a5 b2b3 a5a4 b3a4 a8a4 b1c3 a4a5 g1f3 a5a7 d2d3 a7a6 c1e3 a6a5 f1e2 f7f5 e1g1" 
  ]



return []
main = defaultMain $ testGroup "Tests"
    [testProperties "Property tests" $allProperties, unitTests]
