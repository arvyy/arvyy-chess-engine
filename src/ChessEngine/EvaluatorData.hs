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
import Data.Int (Int64)
import qualified Data.Array.IO as Array
import Data.Word
import Data.Foldable (foldl')
import Data.Bits

newtype PositionEval = PositionEval Int
  deriving (Eq, Show, Ord)

ttSize :: Word64
ttSize = 2 ^ 18

pawnTableSize :: Int64
pawnTableSize = 2 ^ 16

negateEval :: PositionEval -> PositionEval
negateEval (PositionEval v) = PositionEval (-v)

evalAdd :: PositionEval -> Int -> PositionEval
evalAdd (PositionEval v) added = (PositionEval $ v + added)

data TableValueBound = Exact | UpperBound | LowerBound deriving (Show, Eq)

data TranspositionValue = TranspositionValue TableValueBound PositionEval Int [Move] deriving (Show)

type TranspositionTable = Array.IOArray Word64 (Bool, Word64, TranspositionValue)

type PawnTable = Array.IOArray Int64 (Bool, Int64, Int)

type KillerMoveTable = Array.IOArray (Int, Int) [Move]

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

-- maps a bitboard representation
-- to key to be used as an array. Direct `mod` isn't suitable
-- because it will clump alot of positions into same slot
-- use xor with stride 7 (to sqew across files)
pawnEvaluationKey :: Int64 -> Int64
pawnEvaluationKey bitmap =
    let (result, _) = foldl' iteration (0, bitmap) [1..9]
    in result `mod` pawnTableSize
  where
    iteration (key, bitmap) _ = (key `xor` (bitmap .&. 0x7f), bitmap `shiftR` 7)

putPawnEvaluation :: ChessCache -> Int64 -> Int -> IO ()
putPawnEvaluation (ChessCache _ pawns' _) pawnBitboard evaluation =
    Array.writeArray pawns' (pawnEvaluationKey pawnBitboard) (True, pawnBitboard, evaluation)

getPawnEvaluation :: ChessCache -> Int64 -> IO (Maybe Int)
getPawnEvaluation (ChessCache _ pawns' _) pawnBitboard = do
    (present, bitboard, value) <- Array.readArray pawns' (pawnEvaluationKey pawnBitboard)
    return $ if present && bitboard == pawnBitboard
             then Just value
             else Nothing

putKillerMove :: ChessCache -> (Int, Int) -> Move -> IO ()
putKillerMove (ChessCache _ _ killerMoves) plyAndThreadIndex move =
  do
    existing' <- Array.readArray killerMoves plyAndThreadIndex 
    let new = take 2 (move : existing')
    Array.writeArray killerMoves plyAndThreadIndex new

getKillerMoves :: ChessCache -> (Int, Int) -> IO [Move]
getKillerMoves (ChessCache _ _ killerMoves) plyAndThreadIndex = 
    Array.readArray killerMoves plyAndThreadIndex 
                

create :: IO ChessCache
create = do
  table <- Array.newArray (0, ttSize) (False, 0, (TranspositionValue UpperBound (PositionEval 0) 0 [undefined]))
  pawns' <- Array.newArray (0, pawnTableSize) (False, 0, 0)
  killerMoves <- Array.newArray ((0, 0), (30, 8)) []
  return (ChessCache table pawns' killerMoves)
