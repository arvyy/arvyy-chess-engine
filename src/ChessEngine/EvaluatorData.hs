{-# LANGUAGE NamedFieldPuns #-}
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
, create
, clear )
where

import ChessEngine.Board
import qualified Data.HashTable.IO as Map
import Data.Int (Int64)
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Hashable
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.Bits

newtype PositionEval = PositionEval Int
  deriving (Eq, Show, Ord)

negateEval :: PositionEval -> PositionEval
negateEval (PositionEval v) = PositionEval (-v)

evalAdd :: PositionEval -> Int -> PositionEval
evalAdd (PositionEval v) added = (PositionEval $ v + added)

-- TT structure
data TranspositionTable k v = TranspositionTable 
    { present :: {-# UNPACK #-} !(UMV.IOVector Int64)
    , keys :: {-# UNPACK #-} !(MV.IOVector k)
    , values :: {-# UNPACK #-} !(MV.IOVector v)
    -- given old value (first arg) and new value, return True if new value should replace old
    , conflictResolver :: {-# UNPACK #-} !(v -> v -> Bool) }

createTranspositionTable :: (Eq k, Hashable k) =>  Int -> (v -> v -> Bool) -> IO (TranspositionTable k v)
createTranspositionTable size conflictResolver = do 
    present <- UMV.replicate (size `div` 64) 0
    keys <- MV.new size
    values <- MV.new size
    return $ TranspositionTable { present, keys, values, conflictResolver }

{-# INLINE keyToIndex #-}
{-# SPECIALIZE keyToIndex :: ChessBoardKey -> Int -> Int #-}
keyToIndex :: Hashable k => k -> Int -> Int
keyToIndex key tablesize = hash key .&. (tablesize - 1)

{-# INLINE lookupTranspositionTable #-}
{-# SPECIALIZE lookupTranspositionTable :: (TranspositionTable ChessBoardKey TranspositionValue) -> ChessBoardKey -> IO (Maybe TranspositionValue) #-}
lookupTranspositionTable :: (Eq k, Hashable k) => TranspositionTable k v -> k -> IO (Maybe v)
lookupTranspositionTable TranspositionTable { present, keys, values } key = do
    let index = keyToIndex key (UMV.length present)
    exists <- testPresentFlag present index
    if exists
    then do
        key' <- MV.unsafeRead keys index
        if key == key' 
        then Just <$> MV.unsafeRead values index 
        else return Nothing
    else return Nothing

{-# INLINE updateTranspositionTable #-}
{-# SPECIALIZE updateTranspositionTable :: (TranspositionTable ChessBoardKey TranspositionValue) -> ChessBoardKey -> TranspositionValue -> IO () #-}
updateTranspositionTable :: (Eq k, Hashable k) => TranspositionTable k v -> k -> v -> IO ()
updateTranspositionTable TranspositionTable { present, keys, values, conflictResolver } key value = do
    exists <- testPresentFlag present index
    if exists
    then do
        key' <- MV.unsafeRead keys index
        value' <- MV.unsafeRead values index
        when (key == key' || conflictResolver value' value) doWrite
    else doWrite
  where
    index :: Int
    index = keyToIndex key (UMV.length present)

    doWrite :: IO ()
    doWrite = do
        setPresentFlag present index
        MV.unsafeWrite keys index key
        MV.unsafeWrite values index value

testPresentFlag :: UMV.IOVector Int64 -> Int -> IO Bool
testPresentFlag present index = do
    let chunkIndex = shiftR index 6
    let bitIndex = index .&. 63
    chunk <- UMV.unsafeRead present chunkIndex
    return $ testBit chunk bitIndex

setPresentFlag :: UMV.IOVector Int64 -> Int -> IO ()
setPresentFlag present index = do
    let chunkIndex = shiftR index 6
    let bitIndex = index .&. 63
    chunk <- UMV.unsafeRead present chunkIndex
    let chunk' = setBit chunk bitIndex
    UMV.unsafeWrite present chunkIndex (fromIntegral chunk')

newtype ChessBoardKey = ChessBoardKey ChessBoard

instance Eq ChessBoardKey where
    {-# INLINE (==) #-}
    (==) (ChessBoardKey b1) (ChessBoardKey b2) =
        (pieces b1) == (pieces b2) &&
        (turn b1) == (turn b2) &&
        (enPassant b1) == (enPassant b2) &&
        (blackQueenCastle b1) == (blackQueenCastle b2) &&
        (blackKingCastle b1) == (blackKingCastle b2) &&
        (whiteQueenCastle b1) == (whiteQueenCastle b2) &&
        (whiteKingCastle b1) == (whiteKingCastle b2)

instance Hashable ChessBoardKey where
    {-# INLINE hashWithSalt #-}
    hashWithSalt salt (ChessBoardKey ChessBoard { turn, pieces, enPassant, whiteKingCastle, whiteQueenCastle, blackKingCastle, blackQueenCastle }) =
        hashWithSalt salt (turn, pieces, enPassant, whiteKingCastle, whiteQueenCastle, blackKingCastle, blackQueenCastle)


data TableValueBound = Exact | UpperBound | LowerBound deriving (Show, Eq)

data TranspositionValue = TranspositionValue TableValueBound PositionEval Int Int [Move] deriving (Show)
type PawnTable = Map.CuckooHashTable Int64 Int
type KillerMoveTable = Map.CuckooHashTable Int [Move]

data ChessCache = ChessCache (TranspositionTable ChessBoardKey TranspositionValue) (PawnTable) (KillerMoveTable)

putValue :: ChessCache -> ChessBoard -> Int -> Int -> PositionEval -> TableValueBound -> [Move] -> IO ()
putValue (ChessCache table _ _) board depth fullMoves value bound move = do
  let key = ChessBoardKey board
  existingValue <- lookupTranspositionTable table key
  case existingValue of
    Just (TranspositionValue prevBound prevValue prevDepth _ _) -> do
      when (depth == prevDepth) $ updateTranspositionTable table key (TranspositionValue bound value depth fullMoves move)
      when (depth > prevDepth) $ updateTranspositionTable table key (TranspositionValue bound value depth fullMoves move)
    Nothing -> updateTranspositionTable table key (TranspositionValue bound value depth fullMoves move)

getValue :: ChessCache -> ChessBoard -> IO (Maybe TranspositionValue)
getValue (ChessCache table _ _) board = lookupTranspositionTable table (ChessBoardKey board)

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

clear :: ChessCache -> IO ()
-- TODO clear pawns / killers
clear (ChessCache tt pawns killers) = do
    let TranspositionTable { present, keys, values } = tt
    UMV.set present 0
    MV.clear keys
    MV.clear values

create :: IO ChessCache
create = do
  table <- createTranspositionTable (1024 * 1024) conflictResolver
  pawns' <- Map.new
  killerMoves <- Map.new
  return (ChessCache table pawns' killerMoves)

  where
    {-# INLINE conflictResolver #-}
    conflictResolver (TranspositionValue boundOld _ depthOld fullMovesOld _) (TranspositionValue boundNew _ depthNew fullMovesNew _) = depthNew >= depthOld
        {-
        if fullMovesOld + 1 >= fullMovesNew && boundOld == Exact && boundNew /= Exact
        then False
        else depthNew >= depthOld
        -}
