{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck(allProperties)

import ChessEngine.Board(ChessBoard (..), initialBoard, candidateMoves, Move)

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

-- TESTS -------------------------------------

prop_changeturn = 
    \board -> all (\candidate -> case candidate of 
                                    (move, board') -> (turn board) /= (turn board'))
                  $ candidateMoves board

-- TESTS -------------------------------------

return []
tests :: TestTree
tests = testProperties "Tests" $allProperties
main = defaultMain tests
