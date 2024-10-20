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
-- import qualified Data.HashTable.IO as Map
import qualified StmContainers.Map as Map
import Data.Hashable
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Control.Concurrent.STM (atomically)

newtype PositionEval = PositionEval Int
  deriving (Eq, Show, Ord)

negateEval :: PositionEval -> PositionEval
negateEval (PositionEval v) = PositionEval (-v)

evalAdd :: PositionEval -> Int -> PositionEval
evalAdd (PositionEval v) added = (PositionEval $ v + added)

newtype ChessBoardKey = ChessBoardKey ChessBoard

instance Eq ChessBoardKey where
  (==) (ChessBoardKey b1) (ChessBoardKey b2) =
    (zebraHash b1) == (zebraHash b2)
      && (pieces b1) == (pieces b2)
      && (turn b1) == (turn b2)
      && (enPassant b1) == (enPassant b2)
      && (blackQueenCastle b1) == (blackQueenCastle b2)
      && (blackKingCastle b1) == (blackKingCastle b2)
      && (whiteQueenCastle b1) == (whiteQueenCastle b2)
      && (whiteKingCastle b1) == (whiteKingCastle b2)

instance Hashable ChessBoardKey where
  hashWithSalt salt (ChessBoardKey ChessBoard { zebraHash }) = fromIntegral zebraHash

data TableValueBound = Exact | UpperBound | LowerBound deriving (Show, Eq)

data TranspositionValue = TranspositionValue TableValueBound PositionEval Int [Move] deriving (Show)

type TranspositionTable = Map.Map ChessBoardKey TranspositionValue

type PawnTable = Map.Map Int64 Int

type KillerMoveTable = Map.Map Int [Move]

data ChessCache = ChessCache (TranspositionTable) (PawnTable) (KillerMoveTable)

putValue :: ChessCache -> ChessBoard -> Int -> PositionEval -> TableValueBound -> [Move] -> IO ()
putValue (ChessCache table _ _) board depth value bound move = atomically $ do
  let key = ChessBoardKey board
  existingValue <- Map.lookup key table
  case existingValue of
    Just (TranspositionValue prevBound prevValue prevDepth _) -> do
      when (depth == prevDepth) $ Map.insert (TranspositionValue bound value depth move) key table 
      when (depth > prevDepth) $ Map.insert (TranspositionValue bound value depth move) key table
    Nothing -> Map.insert (TranspositionValue bound value depth move) key table

getValue :: ChessCache -> ChessBoard -> IO (Maybe TranspositionValue)
getValue (ChessCache table _ _) board = atomically $ Map.lookup (ChessBoardKey board) table

putPawnEvaluation :: ChessCache -> Int64 -> Int -> IO ()
putPawnEvaluation (ChessCache _ pawns' _) pawnPosition value = atomically $ Map.insert value pawnPosition pawns'

getPawnEvaluation :: ChessCache -> Int64 -> IO (Maybe Int)
getPawnEvaluation (ChessCache _ pawns' _) position = atomically $ Map.lookup position pawns'

putKillerMove :: ChessCache -> Int -> Move -> IO ()
putKillerMove (ChessCache _ _ killerMoves) ply move = atomically $
  do
    existing' <- Map.lookup ply killerMoves 
    let new = case existing' of
          Just lst -> take 2 (move : lst)
          Nothing -> [move]
    Map.insert new ply  killerMoves 

getKillerMoves :: ChessCache -> Int -> IO [Move]
getKillerMoves (ChessCache _ _ killerMoves) ply = atomically $
  do
    existing' <- Map.lookup ply killerMoves 
    let existing = fromMaybe [] existing'
    return existing

create :: IO ChessCache
create = do
  table <- Map.newIO
  pawns' <- Map.newIO
  killerMoves <- Map.newIO
  return (ChessCache table pawns' killerMoves)
