-- common data / type definitions and their util functions
module ChessEngine.EvaluatorData
( PositionEval(..)
, negateEval
, evalAdd
, TableValueBound(..)
, TranspositionValue(..)
, ChessCache(..)
, putValue
, getValue
, putPawnEvaluation
, getPawnEvaluation
, putKillerMove
, getKillerMoves
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

data TableValueBound = Exact | UpperBound | LowerBound deriving (Show, Eq)

data TranspositionValue = TranspositionValue TableValueBound PositionEval Int [Move] deriving (Show)
type TranspositionTable s = Map.HashTable s ChessBoard TranspositionValue
type PawnTable s = Map.HashTable s Int64 Float
type KillerMoveTable s = Map.HashTable s Int [Move]

data ChessCache s = ChessCache (TranspositionTable s) (PawnTable s) (KillerMoveTable s)

putValue :: ChessCache s -> ChessBoard -> Int -> PositionEval -> TableValueBound -> [Move] -> ST s ()
putValue (ChessCache table _ _) board depth value bound move = do
  existingValue <- Map.lookup table board
  case existingValue of
    Just (TranspositionValue _ _ prevDepth _) ->
      when (depth >= prevDepth) $ Map.insert table board (TranspositionValue bound value depth move)
    Nothing -> Map.insert table board (TranspositionValue bound value depth move)

getValue :: ChessCache s -> ChessBoard -> ST s (Maybe TranspositionValue)
getValue (ChessCache table _ _) board = Map.lookup table board

putPawnEvaluation :: ChessCache s -> Int64 -> Float -> ST s ()
putPawnEvaluation (ChessCache _ pawns' _) pawnPosition value = Map.insert pawns' pawnPosition value

getPawnEvaluation :: ChessCache s -> Int64 -> ST s (Maybe Float)
getPawnEvaluation (ChessCache _ pawns' _) position = Map.lookup pawns' position

putKillerMove :: ChessCache s -> Int -> Move -> ST s ()
putKillerMove (ChessCache _ _ killerMoves) ply move = 
    do 
        existing' <- Map.lookup killerMoves ply
        let new = case existing' of
                    Just lst -> take 2 (move : lst)
                    Nothing -> [move]
        Map.insert killerMoves ply new

getKillerMoves :: ChessCache s -> Int -> ST s [Move]
getKillerMoves (ChessCache _ _ killerMoves) ply =
    do
        existing' <- Map.lookup killerMoves ply
        let existing = fromMaybe [] existing'
        return existing

create :: ST s (ChessCache s)
create = do
  table <- Map.new
  pawns' <- Map.new
  killerMoves <- Map.new
  return (ChessCache table pawns' killerMoves)
