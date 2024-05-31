-- common data / type definitions and their util functions
module ChessEngine.EvaluatorData
( PositionEval(..)
, negateEval
, TranspositionValue(..)
, ChessCache(..)
, putValue
, getValue
, putPawnEvaluation
, getPawnEvaluation
, create )
where

import ChessEngine.Board
import qualified Data.HashTable.ST.Basic as Map
import Control.Monad.ST
import Data.Int (Int64)
import Control.Monad

newtype PositionEval = PositionEval Float
  deriving (Eq, Show, Ord)

negateEval :: PositionEval -> PositionEval
negateEval (PositionEval v) = PositionEval (-v)

data TranspositionValue = TranspositionValue PositionEval Int deriving (Show)
type TranspositionTable s = Map.HashTable s ChessBoard TranspositionValue
type PawnTable s = Map.HashTable s Int64 Float
data ChessCache s = ChessCache (TranspositionTable s) (PawnTable s)

putValue :: ChessCache s -> ChessBoard -> Int -> PositionEval -> ST s ()
putValue (ChessCache table _) board depth value = do
  existingValue <- Map.lookup table board
  case existingValue of
    Just (TranspositionValue _ prevDepth) ->
      when (depth > prevDepth) $ Map.insert table board (TranspositionValue value depth)
    Nothing -> Map.insert table board (TranspositionValue value depth)

getValue :: ChessCache s -> ChessBoard -> ST s (Maybe TranspositionValue)
getValue (ChessCache table _) board = Map.lookup table board

putPawnEvaluation :: ChessCache s -> Int64 -> Float -> ST s ()
putPawnEvaluation (ChessCache _ pawns) pawnPosition value = Map.insert pawns pawnPosition value

getPawnEvaluation :: ChessCache s -> Int64 -> ST s (Maybe Float)
getPawnEvaluation (ChessCache _ pawns) position = Map.lookup pawns position

create :: ST s (ChessCache s)
create = do
  table <- Map.new
  pawns <- Map.new
  return (ChessCache table pawns)
