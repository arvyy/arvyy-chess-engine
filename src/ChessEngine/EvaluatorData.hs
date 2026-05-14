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
    putHistoryHeuristicMove,
    getHistoryHeuristicMoveValue,
    create,
    resetBetweenSearch
  )
where

import ChessEngine.Board
import qualified Data.Array.IO as Array
import Data.Bits
import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.Word
import qualified Data.Vector.Unboxed.Mutable as VU

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

data TableValueBound = Exact | UpperBound | LowerBound deriving (Show, Eq, Enum)

data TranspositionValue = TranspositionValue TableValueBound PositionEval Int Move deriving (Show)
type BoxedTranspositionValue = (Int, Int, Int, Int64)

transpositionValueToBoxed :: TranspositionValue -> BoxedTranspositionValue
transpositionValueToBoxed (TranspositionValue bound (PositionEval v) depth m) = (fromEnum bound, v, depth, moveToInt m)

transpositionValueFromBoxed :: BoxedTranspositionValue -> TranspositionValue
transpositionValueFromBoxed (bound, v, depth, m) = TranspositionValue (toEnum bound) (PositionEval v) depth (moveFromInt m)

type TranspositionTable = VU.IOVector (Bool, Word64, BoxedTranspositionValue)

type PawnTable = Array.IOArray Int64 (Bool, Int64, Int)

type HistoryHeuristicTable = Array.IOArray (Int, Int, Int, Int, Int) Int

data ChessCache = ChessCache TranspositionTable PawnTable HistoryHeuristicTable

putValue :: ChessCache -> ChessBoard -> Int -> PositionEval -> TableValueBound -> Move -> IO ()
putValue (ChessCache table _ _) board depth value bound move = do
  let key = fromIntegral $ (zebraHash board) `mod` ttSize
  VU.write table key (True, (zebraHash board), transpositionValueToBoxed (TranspositionValue bound value depth move))

getValue :: ChessCache -> ChessBoard -> IO (Maybe TranspositionValue)
getValue (ChessCache table _ _) board = do
  let key = fromIntegral $ (zebraHash board) `mod` ttSize
  (present, hash, boxedValue) <- VU.read table key
  return $
    if present && hash == (zebraHash board)
      then Just $ transpositionValueFromBoxed boxedValue
      else Nothing

-- maps a bitboard representation
-- to key to be used as an array. Direct `mod` isn't suitable
-- because it will clump alot of positions into same slot
-- use xor with stride 7 (to sqew across files)
pawnEvaluationKey :: Int64 -> Int64
pawnEvaluationKey bitmap =
  let (result, _) = foldl' iteration (0, bitmap) [1 .. 9]
   in result `mod` pawnTableSize
  where
    iteration (key, bitmap) _ = (key `xor` (bitmap .&. 0x7f), bitmap `shiftR` 7)

putPawnEvaluation :: ChessCache -> Int64 -> Int -> IO ()
putPawnEvaluation (ChessCache _ pawns' _) pawnBitboard evaluation =
  Array.writeArray pawns' (pawnEvaluationKey pawnBitboard) (True, pawnBitboard, evaluation)

getPawnEvaluation :: ChessCache -> Int64 -> IO (Maybe Int)
getPawnEvaluation (ChessCache _ pawns' _) pawnBitboard = do
  (present, bitboard, value) <- Array.readArray pawns' (pawnEvaluationKey pawnBitboard)
  return $
    if present && bitboard == pawnBitboard
      then Just value
      else Nothing


historyHeuristicTableKey :: PlayerColor -> Move -> (Int, Int, Int, Int, Int)
historyHeuristicTableKey player move = 
    (fromCol move, fromRow move, toCol move, toRow move, if player == White then 1 else 0)
      
putHistoryHeuristicMove :: ChessCache -> Int -> PlayerColor -> Move -> IO ()
putHistoryHeuristicMove (ChessCache _ _ history) depth player move =
    do
        let key = historyHeuristicTableKey player move
        existing <- Array.readArray history key
        let new = existing + depth * depth
        Array.writeArray history key new

getHistoryHeuristicMoveValue :: ChessCache -> PlayerColor -> Move -> IO Int
getHistoryHeuristicMoveValue (ChessCache _ _ history) player move =
    do
        let key = historyHeuristicTableKey player move
        Array.readArray history key

create :: IO ChessCache
create = do
  table <- VU.replicate (fromIntegral ttSize) (False, 0, transpositionValueToBoxed (TranspositionValue UpperBound (PositionEval 0) 0 (moveFromInt 0)))
  pawns' <- Array.newArray (0, pawnTableSize) (False, 0, 0)
  history <- Array.newArray ((1, 1, 1, 1, 0), (8, 8, 8, 8, 1)) 0
  return (ChessCache table pawns' history)

resetBetweenSearch :: ChessCache -> IO ChessCache
resetBetweenSearch (ChessCache tt pawns _) = do
    history <- Array.newArray ((1, 1, 1, 1, 0), (8, 8, 8, 8, 1)) 0
    return $ ChessCache tt pawns history
