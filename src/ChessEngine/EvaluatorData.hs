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
import qualified Data.HashTable.IO as Map
import Control.Monad.ST
import Data.Int (Int64)
import Control.Monad
import Data.Maybe (fromMaybe)

newtype PositionEval = PositionEval Int
  deriving (Eq, Show, Ord)

negateEval :: PositionEval -> PositionEval
negateEval (PositionEval v) = PositionEval (-v)

evalAdd :: PositionEval -> Int -> PositionEval
evalAdd (PositionEval v) added = (PositionEval $ v + added)

data TableValueBound = Exact | UpperBound | LowerBound deriving (Show, Eq)

data TranspositionValue = TranspositionValue TableValueBound PositionEval Int [Move] deriving (Show)
type TranspositionTable = Map.CuckooHashTable ChessBoard TranspositionValue
type PawnTable = Map.CuckooHashTable Int64 Int
type KillerMoveTable = Map.CuckooHashTable Int [Move]

data ChessCache = ChessCache (TranspositionTable) (PawnTable) (KillerMoveTable)

putValue :: ChessCache -> ChessBoard -> Int -> PositionEval -> TableValueBound -> [Move] -> IO ()
putValue (ChessCache table _ _) board depth value bound move = do
  existingValue <- Map.lookup table board
  case existingValue of
    Just (TranspositionValue _ _ prevDepth _) ->
      when (depth >= prevDepth) $ Map.insert table board (TranspositionValue bound value depth move)
    Nothing -> Map.insert table board (TranspositionValue bound value depth move)

getValue :: ChessCache -> ChessBoard -> IO (Maybe TranspositionValue)
getValue (ChessCache table _ _) board = Map.lookup table board

putPawnEvaluation :: ChessCache -> Int64 -> Int -> IO ()
putPawnEvaluation (ChessCache _ pawns' _) pawnPosition value = Map.insert pawns' pawnPosition value

getPawnEvaluation :: ChessCache -> Int64 -> IO (Maybe Int)
getPawnEvaluation (ChessCache _ pawns' _) position = Map.lookup pawns' position

putKillerMove :: ChessCache -> Int -> Move -> IO ()
putKillerMove (ChessCache _ _ killerMoves) ply move = 
    do 
        existing' <- Map.lookup killerMoves ply
        let new = case existing' of
                    Just lst -> take 2 (move : lst)
                    Nothing -> [move]
        Map.insert killerMoves ply new

getKillerMoves :: ChessCache -> Int -> IO [Move]
getKillerMoves (ChessCache _ _ killerMoves) ply =
    do
        existing' <- Map.lookup killerMoves ply
        let existing = fromMaybe [] existing'
        return existing

create :: IO ChessCache
create = do
  table <- Map.new
  pawns' <- Map.new
  killerMoves <- Map.new
  return (ChessCache table pawns' killerMoves)
