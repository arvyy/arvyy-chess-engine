{-# LANGUAGE NamedFieldPuns #-}

-- common data / type definitions and their util functions
module ChessEngine.EvaluatorData
  ( PositionEval (..),
    negateEval,
    evalAdd,
    TableValueBound (..),
    TranspositionValue (..),
    ChessCache (..),
    putValue,
    getValue,
    putPawnEvaluation,
    getPawnEvaluation,
    putKillerMove,
    getKillerMoves,
    create,
  )
where

import ChessEngine.Board
import Control.Monad
import qualified Data.HashTable.IO as Map
import Data.Hashable
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

newtype PositionEval = PositionEval Int
  deriving (Eq, Show, Ord)

negateEval :: PositionEval -> PositionEval
negateEval (PositionEval v) = PositionEval (-v)

evalAdd :: PositionEval -> Int -> PositionEval
evalAdd (PositionEval v) added = (PositionEval $ v + added)

newtype ChessBoardKey = ChessBoardKey ChessBoard

instance Eq ChessBoardKey where
  (==) (ChessBoardKey b1) (ChessBoardKey b2) =
    (pieces b1) == (pieces b2)
      && (turn b1) == (turn b2)
      && (enPassant b1) == (enPassant b2)
      && (blackQueenCastle b1) == (blackQueenCastle b2)
      && (blackKingCastle b1) == (blackKingCastle b2)
      && (whiteQueenCastle b1) == (whiteQueenCastle b2)
      && (whiteKingCastle b1) == (whiteKingCastle b2)

instance Hashable ChessBoardKey where
  hashWithSalt salt (ChessBoardKey ChessBoard {turn, pieces, enPassant, whiteKingCastle, whiteQueenCastle, blackKingCastle, blackQueenCastle}) =
    hashWithSalt salt (turn, pieces, enPassant, whiteKingCastle, whiteQueenCastle, blackKingCastle, blackQueenCastle)

data TableValueBound = Exact | UpperBound | LowerBound deriving (Show, Eq)

data TranspositionValue = TranspositionValue TableValueBound PositionEval Int [Move] deriving (Show)

type TranspositionTable = Map.CuckooHashTable ChessBoardKey TranspositionValue

type PawnTable = Map.CuckooHashTable Int64 Int

type KillerMoveTable = Map.CuckooHashTable Int [Move]

data ChessCache = ChessCache (TranspositionTable) (PawnTable) (KillerMoveTable)

putValue :: ChessCache -> ChessBoard -> Int -> PositionEval -> TableValueBound -> [Move] -> IO ()
putValue (ChessCache table _ _) board depth value bound move = do
  let key = ChessBoardKey board
  existingValue <- Map.lookup table key
  case existingValue of
    Just (TranspositionValue prevBound prevValue prevDepth _) -> do
      when (depth == prevDepth) $ Map.insert table key (TranspositionValue bound value depth move)
      when (depth > prevDepth) $ Map.insert table key (TranspositionValue bound value depth move)
    Nothing -> Map.insert table key (TranspositionValue bound value depth move)

getValue :: ChessCache -> ChessBoard -> IO (Maybe TranspositionValue)
getValue (ChessCache table _ _) board = Map.lookup table (ChessBoardKey board)

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
