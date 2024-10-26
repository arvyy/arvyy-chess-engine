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
import qualified StmContainers.Map as Map
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Control.Concurrent.STM (atomically)
import qualified Data.Array.IO as Array
import Data.Word

newtype PositionEval = PositionEval Int
  deriving (Eq, Show, Ord)

ttSize :: Word64
ttSize = 2 ^ 18

negateEval :: PositionEval -> PositionEval
negateEval (PositionEval v) = PositionEval (-v)

evalAdd :: PositionEval -> Int -> PositionEval
evalAdd (PositionEval v) added = (PositionEval $ v + added)

data TableValueBound = Exact | UpperBound | LowerBound deriving (Show, Eq)

data TranspositionValue = TranspositionValue TableValueBound PositionEval Int [Move] deriving (Show)

type TranspositionTable = Array.IOArray Word64 (Bool, Word64, TranspositionValue)

type PawnTable = Map.Map Int64 Int

type KillerMoveTable = Map.Map (Int, Int) [Move]

data ChessCache = ChessCache (TranspositionTable) (PawnTable) (KillerMoveTable)

putValue :: ChessCache -> ChessBoard -> Int -> PositionEval -> TableValueBound -> [Move] -> IO ()
putValue (ChessCache table _ _) board depth value bound move = do
  let key = (zebraHash board) `mod` ttSize
  Array.writeArray table key (True, (zebraHash board), (TranspositionValue bound value depth move))

getValue :: ChessCache -> ChessBoard -> IO (Maybe TranspositionValue)
getValue (ChessCache table _ _) board = do
    let key = (zebraHash board) `mod` ttSize
    (present, hash, value@(TranspositionValue _ _ _ _)) <- Array.readArray table key
    return $ 
        if present && hash == (zebraHash board) 
        then Just value 
        else Nothing

putPawnEvaluation :: ChessCache -> Int64 -> Int -> IO ()
putPawnEvaluation (ChessCache _ pawns' _) pawnPosition value = atomically $ Map.insert value pawnPosition pawns'

getPawnEvaluation :: ChessCache -> Int64 -> IO (Maybe Int)
getPawnEvaluation (ChessCache _ pawns' _) position = atomically $ Map.lookup position pawns'

putKillerMove :: ChessCache -> (Int, Int) -> Move -> IO ()
putKillerMove (ChessCache _ _ killerMoves) plyAndThreadIndex move = atomically $
  do
    existing' <- Map.lookup plyAndThreadIndex killerMoves 
    let new = case existing' of
          Just lst -> take 2 (move : lst)
          Nothing -> [move]
    Map.insert new plyAndThreadIndex killerMoves 

getKillerMoves :: ChessCache -> (Int, Int) -> IO [Move]
getKillerMoves (ChessCache _ _ killerMoves) plyAndThreadIndex = atomically $
  do
    existing' <- Map.lookup plyAndThreadIndex killerMoves 
    let existing = fromMaybe [] existing'
    return existing

create :: IO ChessCache
create = do
  table <- Array.newArray (0, ttSize) (False, 0, (TranspositionValue UpperBound (PositionEval 0) 0 [undefined]))
  pawns' <- Map.newIO
  killerMoves <- Map.newIO
  return (ChessCache table pawns' killerMoves)
