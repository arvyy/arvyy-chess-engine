{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

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
    recordHistoricCut,
    getHistoricCut,
    create,
  )
where

import ChessEngine.Board
import qualified Data.Array.IO as Array
import Data.Bits
import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.Word
import Control.Monad (forM_)
import Control.DeepSeq (force, NFData (..), deepseq)

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

data TranspositionValue = TranspositionValue !TableValueBound !PositionEval !Int [Move] deriving (Show)
instance NFData TranspositionValue where
    rnf (TranspositionValue _ _ _ moves) = moves `deepseq` ()

type TranspositionTable = Array.IOArray Word64 (Bool, Word64, TranspositionValue)

type PawnTable = Array.IOArray Int64 (Bool, Int64, Int)

type KillerMoveTable = Array.IOArray (Int, Int) [Move]

type HistoryTable = Array.IOArray (PlayerColor, Int, Int, Int, Int) Int

data ChessCache = ChessCache (TranspositionTable) (PawnTable) (KillerMoveTable) HistoryTable

{-# INLINE putValue #-}
putValue :: ChessCache -> ChessBoard -> Int -> PositionEval -> TableValueBound -> [Move] -> IO ()
putValue (ChessCache table _ _ _) board depth value bound move = do
  let !key = (zebraHash board) `mod` ttSize
  let ttValue = force $ (True, (zebraHash board), (TranspositionValue bound value depth move))
  Array.writeArray table key ttValue

{-# INLINE getValue #-}
getValue :: ChessCache -> ChessBoard -> IO (Maybe TranspositionValue)
getValue (ChessCache table _ _ _) board = do
  let !key = (zebraHash board) `mod` ttSize
  (present, hash, value@(TranspositionValue _ _ _ _)) <- Array.readArray table key
  return $
    if present && hash == (zebraHash board)
      then Just value
      else Nothing

-- maps a bitboard representation
-- to key to be used as an array. Direct `mod` isn't suitable
-- because it will clump alot of positions into same slot
-- use xor with stride 7 (to sqew across files)
{-# INLINE pawnEvaluationKey #-}
pawnEvaluationKey :: Int64 -> Int64
pawnEvaluationKey bitmap =
  let (result, _) = foldl' iteration (0, bitmap) [1 .. 9]
   in result `mod` pawnTableSize
  where
    iteration (key, bitmap) _ = (key `xor` (bitmap .&. 0x7f), bitmap `shiftR` 7)

{-# INLINE putPawnEvaluation #-}
putPawnEvaluation :: ChessCache -> Int64 -> Int -> IO ()
putPawnEvaluation (ChessCache _ pawns' _ _) pawnBitboard evaluation =
  Array.writeArray pawns' (pawnEvaluationKey pawnBitboard) (force (True, pawnBitboard, evaluation))

{-# INLINE getPawnEvaluation #-}
getPawnEvaluation :: ChessCache -> Int64 -> IO (Maybe Int)
getPawnEvaluation (ChessCache _ pawns' _ _) pawnBitboard = do
  (present, bitboard, value) <- Array.readArray pawns' (pawnEvaluationKey pawnBitboard)
  return $
    if present && bitboard == pawnBitboard
      then Just value
      else Nothing

{-# INLINE putKillerMove #-}
putKillerMove :: ChessCache -> (Int, Int) -> Move -> IO ()
putKillerMove (ChessCache _ _ killerMoves _) plyAndThreadIndex move =
  do
    existing' <- Array.readArray killerMoves plyAndThreadIndex
    let new = force $ take 2 (move : existing')
    Array.writeArray killerMoves plyAndThreadIndex new

{-# INLINE getKillerMoves #-}
getKillerMoves :: ChessCache -> (Int, Int) -> IO [Move]
getKillerMoves (ChessCache _ _ killerMoves _) plyAndThreadIndex =
  Array.readArray killerMoves plyAndThreadIndex

{-# INLINE recordHistoricCut #-}
recordHistoricCut :: ChessCache -> Int -> ChessPiece -> Int -> Int -> Int -> Int -> IO ()
recordHistoricCut (ChessCache _ _ _ history) depth (ChessPiece color _) fromx fromy tox toy =
  do
    let key = (color, fromx, fromy, tox, toy)
    value <- Array.readArray history key
    Array.writeArray history key (force (value + (depth * depth)))
    if value >= 200
    then dampenHistory history
    else return ()

{-# INLINE dampenHistory #-}
dampenHistory :: HistoryTable -> IO ()
dampenHistory history = do
    bounds <- Array.getBounds history
    forM_ bounds (\key -> do
                        value <- Array.readArray history key
                        Array.writeArray history key (value `div` 2))

{-# INLINE getHistoricCut #-}
getHistoricCut :: ChessCache -> ChessPiece -> Int -> Int -> Int -> Int -> IO Int
getHistoricCut (ChessCache _ _ _ history) (ChessPiece color _) fromx fromy tox toy =
  do
    let key = (color, fromx, fromy, tox, toy)
    Array.readArray history key

create :: IO ChessCache
create = do
  let !ttDefaultValue = (False, 0, (TranspositionValue UpperBound (PositionEval 0) 0 [undefined]))
  table <- Array.newArray (0, ttSize) ttDefaultValue
  pawns' <- Array.newArray (0, pawnTableSize) (force (False, 0, 0))
  killerMoves <- Array.newArray ((0, 0), (30, 8)) []
  history <- Array.newArray ((Black, 1, 1, 1, 1), (White, 8, 8, 8, 8)) 0
  return (ChessCache table pawns' killerMoves history)
