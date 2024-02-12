{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck(allProperties)
import Test.Tasty.HUnit

import Data.Maybe

import ChessEngine.Board
import ChessEngine.PositionEval
import ChessEngine.UCI

import Debug.Trace

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

candidateExists :: String -> Move -> Bool
candidateExists uci move =
    let Just (Position board) = parseUCICommand uci
        candidates :: [Move]
        candidates = map fst $ candidateMoves board
    in elem move candidates


unitTests = testGroup "Unit tests"
  [ testCase "Parse UCI command" $ do
      parseUCICommand "uci" @?= Just UCI
      parseUCICommand "quit" @?= Just Quit
      parseUCICommand "position startpos" @?= Just (Position initialBoard)
      parseUCICommand "position startpos moves" @?= Just (Position initialBoard)
      parseUCICommand "position startpos moves e2e4" @?= fmap Position (applyMove initialBoard (Move 5 2 5 4 Nothing))
      parseUCICommand "go depth 6" @?= Just (Go emptyGoProps{ depth = Just 6 })
  , testCase "Candidates moves" $ do
      -- test en pessant works
      candidateExists "position startpos moves e2e4 a7a5 e4e5 d7d5" (Move 5 5 4 6 Nothing) @?= True
      -- regression test, king trying to capture protected pieces
      candidateExists "position fen 8/8/8/7p/1P2pB2/4Kb2/2k5/8 w - - 0 46" (Move 5 3 6 3 Nothing) @?= False
  ]



return []
main = defaultMain $ testGroup "Tests"
    [testProperties "Property tests" $allProperties, unitTests]
