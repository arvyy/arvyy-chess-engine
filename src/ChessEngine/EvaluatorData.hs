-- common data / type definitions and their util functions
module ChessEngine.EvaluatorData
( PositionEval(..)
, negateEval
, evalAdd
, TranspositionValue(..)
, ChessCache(..)
, putValue
, getValue
, putPawnEvaluation
, getPawnEvaluation
, putKillerMove
, isKillerMove
, create )
where

import ChessEngine.Board
import qualified Data.HashTable.ST.Basic as Map
import Control.Monad.ST
import Data.Int (Int64)
import Control.Monad
import Data.Maybe (fromMaybe)

newtype PositionEval = PositionEval Float
  deriving (Eq, Show, Ord)

negateEval :: PositionEval -> PositionEval
negateEval (PositionEval v) = PositionEval (-v)

evalAdd :: PositionEval -> Float -> PositionEval
evalAdd (PositionEval v) added = (PositionEval $ v + added)

data TranspositionValue = TranspositionValue PositionEval Int deriving (Show)
type TranspositionTable s = Map.HashTable s ChessBoard TranspositionValue
type PawnTable s = Map.HashTable s Int64 Float
type KillerMoveTable s = Map.HashTable s Int [Move]

data ChessCache s = ChessCache (TranspositionTable s) (PawnTable s) (KillerMoveTable s)

putValue :: ChessCache s -> ChessBoard -> Int -> PositionEval -> ST s ()
putValue (ChessCache table _ _) board depth value = do
  existingValue <- Map.lookup table board
  case existingValue of
    Just (TranspositionValue _ prevDepth) ->
      when (depth > prevDepth) $ Map.insert table board (TranspositionValue value depth)
    Nothing -> Map.insert table board (TranspositionValue value depth)

getValue :: ChessCache s -> ChessBoard -> ST s (Maybe TranspositionValue)
getValue (ChessCache table _ _) board = Map.lookup table board

putPawnEvaluation :: ChessCache s -> Int64 -> Float -> ST s ()
putPawnEvaluation (ChessCache _ pawns _) pawnPosition value = Map.insert pawns pawnPosition value

getPawnEvaluation :: ChessCache s -> Int64 -> ST s (Maybe Float)
getPawnEvaluation (ChessCache _ pawns _) position = Map.lookup pawns position

putKillerMove :: ChessCache s -> Int -> Move -> ST s ()
putKillerMove (ChessCache _ _ killerMoves) depth move = 
    do 
        existing' <- Map.lookup killerMoves depth
        let new = case existing' of
                    Just lst -> take 2 (move : lst)
                    Nothing -> [move]
        Map.insert killerMoves depth new

isKillerMove :: ChessCache s -> Int -> Move -> ST s Bool
isKillerMove (ChessCache _ _ killerMoves) depth move =
    do
        existing' <- Map.lookup killerMoves depth
        let existing = fromMaybe [] existing'
        let result = elem move existing
        return result

create :: ST s (ChessCache s)
create = do
  table <- Map.new
  pawns <- Map.new
  killerMoves <- Map.new
  return (ChessCache table pawns killerMoves)
