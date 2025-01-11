{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module ChessEngine.Board
  ( ChessPieceType (..),
    PlayerColor (..),
    otherPlayer,
    ChessPiece (..),
    Move,
    createMove,
    fromCol,
    fromRow,
    toCol,
    toRow,
    promotion,
    PromoChessPieceType (..),
    ChessBoard (..),
    ChessBoardPositions (..),
    boardPositions,
    findPiecePositions,
    boardNonPawnPositions,
    boardPawnPositions,
    hasPieceOnSquare,
    pieceOnSquare,
    squareEmpty,
    squareThreatenedBy,
    pseudoLegalCandidateMoves,
    candidateMoveLegal,
    quickMaterialCount,
    quickPawnCount,
    is3foldRepetition,
    initialBoard,
    applyNullMove,
    applyMove,
    applyMoveUnsafe,
    getCaptureInfo,
    playerInCheck,
    playerKingPosition,
    parseMove,
    moveToString,
    loadFen,
    pieceThreats,
    boardToFen,
    fileState,
    FileState (..),
    isPassedPawn,
    isBackwardPawn,
    countPawnShield,
    initPrecomputation
  )
where

import ChessEngine.PrecomputedCandidateMoves
import Data.Array.IArray
import Data.Bits
import Data.Char
import Data.Foldable (find, foldl', foldlM)
import Data.Hashable
import Data.Int (Int64)
import Data.List (unfoldr, intercalate)
import Data.Maybe
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Random (randoms)
import System.Random.TF (mkTFGen)
import Text.Read (readMaybe)
import Control.Applicative
import qualified Data.Set as Set
import Control.Monad (foldM)
import Control.Monad (msum)
import Control.DeepSeq (NFData (..), deepseq, force)
import qualified Data.IntMap.Strict as IntMap

data ChessBoardPositions = ChessBoardPositions
  { black :: !Int64,
    white :: !Int64,
    bishops :: !Int64,
    horses :: !Int64,
    queens :: !Int64,
    kings :: !Int64,
    pawns :: !Int64,
    rocks :: !Int64
  }
  deriving (Eq, Ord, Generic)

instance Show ChessBoardPositions where
  show positions = show $ positionsToList positions [Pawn, Bishop, Horse, Rock, Queen, King]

positionsToList :: ChessBoardPositions -> [ChessPieceType] -> [(Int, Int, ChessPiece)]
positionsToList positions types =
  concatMap (\color -> playerPositionsToList positions color types) [White, Black]

{-# INLINE playerPositionsToList #-}
playerPositionsToList :: ChessBoardPositions -> PlayerColor -> [ChessPieceType] -> [(Int, Int, ChessPiece)]
playerPositionsToList ChessBoardPositions {black, white, bishops, horses, queens, kings, pawns, rocks} color types =
  concatMap getTypeValues types
  where
    playerbitmap = if color == White then white else black

    getTypeValues :: ChessPieceType -> [(Int, Int, ChessPiece)]
    getTypeValues t =
      let bitmap = case t of
            Pawn -> pawns
            Bishop -> bishops
            Horse -> horses
            Rock -> rocks
            Queen -> queens
            King -> kings
       in collectValues t bitmap

    collectValues pieceType bitmap = do
      let bitmap' = bitmap .&. playerbitmap
      (x, y) <- bitmapToCoords bitmap'
      return (x, y, ChessPiece color pieceType)

{-# INLINE playerKingPosition #-}
playerKingPosition :: ChessBoard -> PlayerColor -> (Int, Int)
playerKingPosition ChessBoard {pieces = ChessBoardPositions {black, white, kings}} color =
  let bitmap = (if color == White then white else black) .&. kings
   in bitIndexToCoords (countTrailingZeros bitmap)

{-# INLINE bitmapToCoords #-}
bitmapToCoords :: Int64 -> [(Int, Int)]
bitmapToCoords bitmap =
  unfoldr unfoldStep bitmap
  where
    unfoldStep bitmap =
      let index = countTrailingZeros bitmap
       in if index >= 64
            then Nothing
            else Just (bitIndexToCoords index, clearBit bitmap index)

{-# INLINE bitIndexToCoords #-}
bitIndexToCoords :: Int -> (Int, Int)
bitIndexToCoords index =
  let x' = mod index 8
      y' = div index 8
   in if index >= 64
      then error $ "Bitmax index out of bounds: " ++ (show index)
      else (x' + 1, y' + 1)

coordsToBitmap :: [(Int, Int)] -> Int64
coordsToBitmap coords =
  foldl' (\bitmap (x, y) -> bitmap .|. (1 `shiftL` (coordsToBitIndex x y))) 0 coords

{-# INLINE coordsToBitIndex #-}
coordsToBitIndex :: Int -> Int -> Int
coordsToBitIndex x y =
  let value = (y - 1) * 8 + x - 1
   in if value > 63 then error ("Coordinate out of bounds: " ++ show (x, y)) else value

{-# INLINE clearPosition #-}
clearPosition :: Word64 -> ChessBoardPositions -> Int -> Int -> (Word64, ChessBoardPositions)
clearPosition hash positions@(ChessBoardPositions black white bishops horses queens kings pawns rocks) x y =
  case pieceOnSquare' positions x y of
    Just piece@(ChessPiece color pieceType) ->
      let positions =
            ChessBoardPositions
              { black = if color == Black then (clearBit black bitIndex) else black,
                white = if color == White then (clearBit white bitIndex) else white,
                pawns = if pieceType == Pawn then (clearBit pawns bitIndex) else pawns,
                horses = if pieceType == Horse then (clearBit horses bitIndex) else horses,
                bishops = if pieceType == Bishop then (clearBit bishops bitIndex) else bishops,
                rocks = if pieceType == Rock then (clearBit rocks bitIndex) else rocks,
                queens = if pieceType == Queen then (clearBit queens bitIndex) else queens,
                kings = if pieceType == King then (clearBit kings bitIndex) else kings
              }
       in (hash `xor` zebraHashPiece x y piece, positions)
    Nothing -> (hash, positions)
  where
    bitIndex = coordsToBitIndex x y

{-# INLINE setPosition #-}
setPosition :: Word64 -> ChessBoardPositions -> Int -> Int -> ChessPiece -> (Word64, ChessBoardPositions)
setPosition hash positions x y piece@(ChessPiece color pieceType) =
  let (hash', (ChessBoardPositions black white bishops horses queens kings pawns rocks)) = clearPosition hash positions x y
      positions' =
        ChessBoardPositions
          { black = if color == Black then setBit black bitIndex else black,
            white = if color == White then setBit white bitIndex else white,
            bishops = if pieceType == Bishop then setBit bishops bitIndex else bishops,
            horses = if pieceType == Horse then setBit horses bitIndex else horses,
            queens = if pieceType == Queen then setBit queens bitIndex else queens,
            kings = if pieceType == King then setBit kings bitIndex else kings,
            pawns = if pieceType == Pawn then setBit pawns bitIndex else pawns,
            rocks = if pieceType == Rock then setBit rocks bitIndex else rocks
          }
   in (hash' `xor` zebraHashPiece x y piece, positions')
  where
    bitIndex = coordsToBitIndex x y

-- count minor + major pieces with standard evaluation
{-# INLINE quickMaterialCount #-}
quickMaterialCount :: ChessBoard -> PlayerColor -> Int
quickMaterialCount board player =
  let p = pieces board
      playerBits = if player == White then (white p) else (black p)
   in (popCount (bishops p .&. playerBits)) * 3
        + (popCount (horses p .&. playerBits)) * 3
        + (popCount (rocks p .&. playerBits)) * 5
        + (popCount (queens p .&. playerBits)) * 9

-- count pawns with standard evaluation
{-# INLINE quickPawnCount #-}
quickPawnCount :: ChessBoard -> PlayerColor -> Int
quickPawnCount board player =
  let p = pieces board
      playerBits = if player == White then (white p) else (black p)
   in (popCount (pawns p .&. playerBits))

{-# INLINE pieceOnSquare' #-}
pieceOnSquare' :: ChessBoardPositions -> Int -> Int -> Maybe ChessPiece
pieceOnSquare' (ChessBoardPositions black white bishops horses queens kings pawns rocks) x y = do
  _ <- if inBounds x y then return True else Nothing
  color' <- color
  ChessPiece color' <$> pieceType
  where
    bitIndex = coordsToBitIndex x y
    color
      | testBit black bitIndex = Just Black
      | testBit white bitIndex = Just White
      | otherwise = Nothing
    pieceType
      | not $ testBit (white .|. black) bitIndex = Nothing
      | testBit pawns bitIndex = Just Pawn
      | testBit bishops bitIndex = Just Bishop
      | testBit horses bitIndex = Just Horse
      | testBit queens bitIndex = Just Queen
      | testBit kings bitIndex = Just King
      | testBit rocks bitIndex = Just Rock
      | otherwise = Nothing

data ChessPieceType = Pawn | Horse | Bishop | Rock | Queen | King deriving (Show, Eq, Ord, Enum)

data PlayerColor = Black | White deriving (Eq, Show, Ord, Generic, Enum)

data ChessPiece = ChessPiece !PlayerColor !ChessPieceType deriving (Show, Eq)

-- TODO merge enPassant / castling / turn into pieces, to unify board state in one place
data ChessBoard = ChessBoard
  { turn :: !PlayerColor,
    fullMoves :: !Int,
    pieces :: !ChessBoardPositions,
    -- if last move was double pawn move, indicates the file for potential en passant followup
    enPassant :: !(Maybe Int),
    -- state for castling resolution
    whiteKingCastle :: !Bool,
    whiteQueenCastle :: !Bool,
    blackKingCastle :: !Bool,
    blackQueenCastle :: !Bool,
    -- zebra hash of the thing
    zebraHash :: !Word64,
    -- linked list of boards leading to this one for purposes of
    -- 3fold repetition detection; cut off when doing irreversible move like capture / pawn move
    prev :: Maybe ChessBoard
  }
  deriving (Eq, Show, Ord)

instance Hashable PlayerColor

instance Hashable ChessBoardPositions

data PromoChessPieceType = NoPromo | PromoHorse | PromoRock | PromoQueen | PromoBishop deriving (Show, Eq, Enum)

newtype Move = Move Int64 deriving (Eq)

instance Show Move where
  show move@(Move bitRepr) = case moveToString move of
    Just str -> str
    _ -> "<Invalid move " ++ (show bitRepr) ++ ">"

{-# INLINE createMove #-}
createMove :: Int -> Int -> Int -> Int -> PromoChessPieceType -> Move
createMove fromCol fromRow toCol toRow promotion =
  let promo = fromIntegral $ fromEnum promotion
      bitRepr = (shiftL promo 12) .|. (shiftL (fromCol - 1) 9) .|. (shiftL (fromRow - 1) 6) .|. (shiftL (toCol - 1) 3) .|. (toRow - 1)
   in Move (fromIntegral bitRepr)

{-# INLINE fromCol #-}
fromCol :: Move -> Int
fromCol (Move bitRepr) = (fromIntegral $ (shiftR bitRepr 9) .&. 7) + 1

{-# INLINE fromRow #-}
fromRow :: Move -> Int
fromRow (Move bitRepr) = (fromIntegral $ (shiftR bitRepr 6) .&. 7) + 1

{-# INLINE toCol #-}
toCol :: Move -> Int
toCol (Move bitRepr) = (fromIntegral $ (shiftR bitRepr 3) .&. 7) + 1

{-# INLINE toRow #-}
toRow :: Move -> Int
toRow (Move bitRepr) = (fromIntegral $ bitRepr .&. 7) + 1

promotion :: Move -> PromoChessPieceType
promotion (Move bitRepr) = toEnum $ fromIntegral (shiftR bitRepr 12)

moveToTuple :: Move -> (Int, Int, Int, Int, PromoChessPieceType)
moveToTuple move = (fromCol move, fromRow move, toCol move, toRow move, promotion move)

boardPositions :: ChessBoard -> [(Int, Int, ChessPiece)]
boardPositions ChessBoard {pieces = pieces} = positionsToList pieces [Pawn, Bishop, Horse, Rock, Queen, King]

boardNonPawnPositions :: ChessBoard -> [(Int, Int, ChessPiece)]
boardNonPawnPositions ChessBoard {pieces = pieces} = positionsToList pieces [Bishop, Horse, Rock, Queen, King]

boardPawnPositions :: ChessBoard -> [(Int, Int, ChessPiece)]
boardPawnPositions ChessBoard {pieces = pieces} = positionsToList pieces [Pawn]

parseSquareReference :: String -> Maybe (Int, Int)
parseSquareReference [x, y] = do
  x' <- case x of
    'a' -> Just 1
    'b' -> Just 2
    'c' -> Just 3
    'd' -> Just 4
    'e' -> Just 5
    'f' -> Just 6
    'g' -> Just 7
    'h' -> Just 8
    _ -> Nothing
  y' <- if isDigit y then Just (digitToInt y) else Nothing
  if inBounds x' y' then Just (x', y') else Nothing
parseSquareReference _ = Nothing

squareReferenceToString :: (Int, Int) -> Maybe String
squareReferenceToString (x, y) = do
  x' <- case x of
    1 -> Just "a"
    2 -> Just "b"
    3 -> Just "c"
    4 -> Just "d"
    5 -> Just "e"
    6 -> Just "f"
    7 -> Just "g"
    8 -> Just "h"
    _ -> Nothing
  y' <- if y >= 1 && y <= 8 then Just (show y) else Nothing
  return (x' ++ y')

parseMove :: String -> Maybe Move
parseMove (x : y : x' : y' : rest) = do
  fromSquare <- parseSquareReference [x, y]
  toSquare <- parseSquareReference [x', y']
  promotion <- case rest of
    [] -> Just NoPromo
    ['n'] -> Just PromoHorse
    ['b'] -> Just PromoBishop
    ['q'] -> Just PromoQueen
    ['r'] -> Just PromoRock
    _ -> Nothing
  return $ createMove (fst fromSquare) (snd fromSquare) (fst toSquare) (snd toSquare) promotion
parseMove _ = Nothing

moveToString :: Move -> Maybe String
moveToString move = do
  let fromx = fromCol move
  let fromy = fromRow move
  let tox = toCol move
  let toy = toRow move
  let promo = promotion move
  from <- squareReferenceToString (fromx, fromy)
  to <- squareReferenceToString (tox, toy)
  let promoStr = case promo of
        NoPromo -> ""
        PromoHorse -> "n"
        PromoRock -> "r"
        PromoQueen -> "q"
        PromoBishop -> "b"
  return (from ++ to ++ promoStr)

initialBoard :: ChessBoard
initialBoard =
  let (board, _) = fromJust $ loadFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
   in board

otherPlayer :: PlayerColor -> PlayerColor
otherPlayer Black = White
otherPlayer White = Black

isPlayerOnSquare :: ChessBoard -> PlayerColor -> Int -> Int -> Bool
isPlayerOnSquare ChessBoard {pieces = ChessBoardPositions {white}} White x y =
  let bitIndex = coordsToBitIndex x y
   in testBit white bitIndex
isPlayerOnSquare ChessBoard {pieces = ChessBoardPositions {black}} Black x y =
  let bitIndex = coordsToBitIndex x y
   in testBit black bitIndex

{-# INLINE squareEmpty #-}
squareEmpty :: ChessBoard -> Int -> Int -> Bool
squareEmpty board x y =
    not (isPlayerOnSquare board White x y || isPlayerOnSquare board Black x y)

{-# INLINE pieceOnSquare #-}
pieceOnSquare :: ChessBoard -> Int -> Int -> Maybe ChessPiece
pieceOnSquare board = pieceOnSquare' (pieces board)

{-# INLINE hasPieceOnSquare #-}
hasPieceOnSquare :: ChessBoard -> Int -> Int -> ChessPiece -> Bool
hasPieceOnSquare ChessBoard {pieces} x y (ChessPiece color pieceType) =
  testBit (playerBitboard .&. pieceBitboard) bitIndex
  where
    playerBitboard = if color == White then white pieces else black pieces
    pieceBitboard = case pieceType of
      Pawn -> pawns pieces
      Rock -> rocks pieces
      Queen -> queens pieces
      King -> kings pieces
      Horse -> horses pieces
      Bishop -> bishops pieces
    bitIndex = coordsToBitIndex x y

{-# INLINE findPiecePositions #-}
findPiecePositions :: ChessBoard -> ChessPiece -> [(Int, Int)]
findPiecePositions ChessBoard {pieces} (ChessPiece color pieceType) =
  bitmapToCoords $ playerBitboard .&. pieceBitboard
  where
    playerBitboard = if color == White then white pieces else black pieces
    pieceBitboard = case pieceType of
      Pawn -> pawns pieces
      Rock -> rocks pieces
      Queen -> queens pieces
      King -> kings pieces
      Horse -> horses pieces
      Bishop -> bishops pieces

-- applies move blindly without validation for checks or piece movement rules
-- partial function if reference position is empty
applyMoveUnsafe ::  ChessBoard -> Move -> ChessBoard
applyMoveUnsafe board move =
  let ChessPiece player pieceType = case pieceOnSquare board x y of
        Nothing -> error $ "Unsafe move tried to move unexisting piece; board: " ++ boardToFen board ++ " Square: " ++ show (x, y)
        Just f -> f
      hash = zebraHash board
      fullMoves' = (fullMoves board) + (if (turn board) == Black then 1 else 0)
      isEnPassantMove = pieceType == Pawn && enPassant board == Just x' && (if player == White then y == 5 else y == 4)
      isDoubleDipMove = pieceType == Pawn && abs (y - y') == 2
      isKingCastleMove = pieceType == King && x == 5 && x' == 7
      isQueenCastleMove = pieceType == King && x == 5 && x' == 3
      isCastleMove = isKingCastleMove || isQueenCastleMove
      oldPieces = pieces board
      (hash', newPieces)
        | isEnPassantMove = applyEnPassant hash oldPieces player
        | isKingCastleMove = applyKingCastle hash oldPieces player
        | isQueenCastleMove = applyQueenCastle hash oldPieces player
        | otherwise = applyNormalMove hash oldPieces player pieceType
      enPassant' = if isDoubleDipMove then Just x else Nothing

      whiteKingRockTooken = (player == Black && x' == 8 && y' == 1)
      whiteKingCastle' =
        whiteKingCastle board
          && not (isCastleMove && player == White)
          && case (player, pieceType) of
            (White, King) -> False
            (White, Rock) -> not (x == 8 && y == 1)
            _ -> True
          && not whiteKingRockTooken

      whiteQueenRockTooken = (player == Black && x' == 1 && y' == 1)
      whiteQueenCastle' =
        whiteQueenCastle board
          && not (isCastleMove && player == White)
          && case (player, pieceType) of
            (White, King) -> False
            (White, Rock) -> not (x == 1 && y == 1)
            _ -> True
          && not whiteQueenRockTooken

      blackKingRockTooken = (player == White && x' == 8 && y' == 8)
      blackKingCastle' =
        blackKingCastle board
          && not (isCastleMove && player == Black)
          && case (player, pieceType) of
            (Black, King) -> False
            (Black, Rock) -> not (x == 8 && y == 8)
            _ -> True
          && not blackKingRockTooken

      blackQueenRockTooken = (player == White && x' == 1 && y' == 8)
      blackQueenCastle' =
        blackQueenCastle board
          && not (isCastleMove && player == Black)
          && case (player, pieceType) of
            (Black, King) -> False
            (Black, Rock) -> not (x == 1 && y == 8)
            _ -> True
          && not blackQueenRockTooken
      hash'' =
        hash'
          `xor` (if blackQueenCastle' /= (blackQueenCastle board) then zebraHashQueenCastle Black else 0)
          `xor` (if blackKingCastle' /= (blackKingCastle board) then zebraHashKingCastle Black else 0)
          `xor` (if whiteQueenCastle' /= (whiteQueenCastle board) then zebraHashQueenCastle White else 0)
          `xor` (if whiteKingCastle' /= (whiteKingCastle board) then zebraHashKingCastle White else 0)
          `xor` case enPassant board of
            Just n -> zebraHashEnPeasent n
            Nothing -> 0
          `xor` case enPassant' of
            Just n -> zebraHashEnPeasent n
            Nothing -> 0
          `xor` zebraHashBlackTurn
      isIrreversible = pieceType == Pawn || isCastleMove || isJust (pieceOnSquare' oldPieces x' y')
      linkedBoard =
        if isIrreversible
          then Nothing
          else Just board
   in board
        { pieces = newPieces,
          enPassant = enPassant',
          turn = otherPlayer (turn board),
          whiteKingCastle = whiteKingCastle',
          blackKingCastle = blackKingCastle',
          whiteQueenCastle = whiteQueenCastle',
          blackQueenCastle = blackQueenCastle',
          fullMoves = fullMoves',
          prev = linkedBoard,
          zebraHash = hash''
        }
  where
    (x, y, x', y', promotion) = moveToTuple move
    applyKingCastle :: Word64 -> ChessBoardPositions -> PlayerColor -> (Word64, ChessBoardPositions)
    applyKingCastle hash positions color =
      let (hash', p1) = clearPosition hash positions 5 y
          (hash'', p2) = clearPosition hash' p1 8 y
          (hash''', p3) = setPosition hash'' p2 6 y (ChessPiece color Rock)
       in setPosition hash''' p3 7 y (ChessPiece color King)

    applyQueenCastle :: Word64 -> ChessBoardPositions -> PlayerColor -> (Word64, ChessBoardPositions)
    applyQueenCastle hash positions color =
      let (hash', p1) = clearPosition hash positions 1 y
          (hash'', p2) = clearPosition hash' p1 5 y
          (hash''', p3) = setPosition hash'' p2 4 y (ChessPiece color Rock)
       in setPosition hash''' p3 3 y (ChessPiece color King)

    applyEnPassant :: Word64 -> ChessBoardPositions -> PlayerColor -> (Word64, ChessBoardPositions)
    applyEnPassant hash positions color =
      let (hash', p1) = clearPosition hash positions x y
          (hash'', p2) = clearPosition hash' p1 x' y
       in setPosition hash'' p2 x' y' (ChessPiece color Pawn)

    applyNormalMove :: Word64 -> ChessBoardPositions -> PlayerColor -> ChessPieceType -> (Word64, ChessBoardPositions)
    applyNormalMove hash positions color pieceType =
      let (hash', p1) = clearPosition hash positions x y
          newMovedPiece = case promotion of
            NoPromo -> ChessPiece color pieceType
            PromoHorse -> ChessPiece color Horse
            PromoRock -> ChessPiece color Rock
            PromoQueen -> ChessPiece color Queen
            PromoBishop -> ChessPiece color Bishop
       in setPosition hash' p1 x' y' newMovedPiece

-- return if given board is in 3fold repetition state
is3foldRepetition :: ChessBoard -> Bool
is3foldRepetition board = repetitionCount >= 2
  where
    isSamePos board1 board2 =
        (pieces board1 == pieces board2)
        && (blackQueenCastle board1 == blackQueenCastle board2)
        && (blackKingCastle board1 == blackKingCastle board2)
        && (whiteQueenCastle board1 == whiteQueenCastle board2)
        && (whiteKingCastle board1 == whiteKingCastle board2)
        && (enPassant board1 == enPassant board2)

    {-# INLINE unfoldrBoards #-}
    unfoldrBoards :: Maybe ChessBoard -> Maybe (ChessBoard, Maybe ChessBoard)
    unfoldrBoards (Just b@ChessBoard { prev }) =
      case prev of
        Just (ChessBoard {prev = Just b'}) -> Just (b, Just b')
        _ -> Just (b, Nothing)
    unfoldrBoards Nothing = Nothing

    {-# INLINE otherBoards #-}
    otherBoards = drop 1 $ unfoldr unfoldrBoards $ Just board

    repetitionCount :: Integer
    repetitionCount = 
        let count' = foldlM (\count b -> if (isSamePos board b)
                            then if count == 1
                                 then Left 2
                                 else Right $ count + 1
                            else Right count)
                            0
                            otherBoards
        in case count' of
            Left n -> n
            Right n -> n


inBounds :: Int -> Int -> Bool
inBounds x y = x >= 1 && x <= 8 && y >= 1 && y <= 8

emptyOrOccupiedByOpponent :: ChessBoard -> PlayerColor -> (Int, Int) -> Bool
emptyOrOccupiedByOpponent ChessBoard {pieces = ChessBoardPositions {white = white}} White (x, y) =
  let bitIndex = coordsToBitIndex x y
   in inBounds x y && not (testBit white bitIndex)
emptyOrOccupiedByOpponent ChessBoard {pieces = ChessBoardPositions {black = black}} Black (x, y) =
  let bitIndex = coordsToBitIndex x y
   in inBounds x y && not (testBit black bitIndex)

-- given board and piece find threatened squares (for the purposes of check, castling, and eval for controlled squares)
-- ignores own pin status
{-# INLINE pieceThreats #-}
pieceThreats :: ChessBoard -> (Int, Int, ChessPiece) -> [(Int, Int)]
pieceThreats ChessBoard {pieces = ChessBoardPositions {white, black}} (x, y, ChessPiece color King) =
  {-# SCC "m_pieceThreats_King" #-}
  let baseHops = emptyBoardKingHops x y
      bitboardMask = complement (if color == White then white else black)
      hops = baseHops .&. bitboardMask
   in bitmapToCoords hops
pieceThreats board (x, y, ChessPiece color Queen) =
  {-# SCC "m_pieceThreats_Queen" #-}
  -- concatMap (\pieceType -> pieceThreats board (x, y, ChessPiece color pieceType)) [Rock, Bishop]
  queenCandidateMoves x y color board
pieceThreats board (x, y, ChessPiece color Pawn) =
  {-# SCC "m_pieceThreats_Pawn" #-}
  let nextRow = if color == Black then y - 1 else y + 1
      candidates = [(x - 1, nextRow), (x + 1, nextRow)]
   in filter
        (emptyOrOccupiedByOpponent board color)
        candidates
pieceThreats board (x, y, ChessPiece color Bishop) =
  {-# SCC "m_pieceThreats_Bishop" #-}
  {-
  let rays = emptyBoardBishopRays x y
   in concatMap (rayToValidMoves color board) rays
  -}
  bishopCandidateMoves x y color board
pieceThreats board (x, y, ChessPiece color Rock) =
  {-# SCC "m_pieceThreats_Rock" #-}
  {-
  let rays = emptyBoardRockRays x y
   in concatMap (rayToValidMoves color board) rays
   -}
  rockCandidateMoves x y color board
pieceThreats ChessBoard {pieces = ChessBoardPositions {white, black}} (x, y, ChessPiece color Horse) =
  {-# SCC "m_pieceThreats_Horse" #-}
  let baseHops = emptyBoardHorseHops x y
      bitboardMask = complement (if color == White then white else black)
      hops = baseHops .&. bitboardMask
   in bitmapToCoords hops

-- returns least valuable piece (so this function can be used in SEE) which threatens given square
{-# INLINE squareThreatenedBy #-}
squareThreatenedBy :: ChessBoard -> PlayerColor -> Int -> Int -> Maybe (Int, Int, ChessPiece)
squareThreatenedBy board@ChessBoard {pieces = ChessBoardPositions {horses, kings, bishops, rocks, queens, white, black}} player x y =
  threatenedByPawn 
      <|> threatenedByHorse
      <|> threatenedByBishop
      <|> threatenedByRock
      <|> threatenedByQueen
      <|> threatenedByKing

  where
    opponentColor = if player == White then Black else White
    opponentBits = if opponentColor == White then white else black

    targetBitmap :: Int64
    targetBitmap = force $ setBit 0 (coordsToBitIndex x y)

    threatenedByHorse = {-# SCC "m_threatenedByHorse" #-}
      let opponentHorses = (if opponentColor == White then white else black) .&. horses
          matchedHorses = opponentHorses .&. emptyBoardHorseHops x y
          horsesCoords = bitmapToCoords matchedHorses
       in case horsesCoords of
            (x, y):_ -> Just (x, y, ChessPiece opponentColor Horse)
            _ -> Nothing

    threatenedByBishop = {-# SCC "m_threatenedByBishop" #-} 
        let bishopsCoords = bitmapToCoords $ opponentBits .&. bishops
            checkMatch (bishopX, bishopY) =
                if (magicCandidateMovesBitboard bishopMagicBitboards bishopX bishopY opponentColor board) .&. targetBitmap /= 0
                then Just (bishopX, bishopY, (ChessPiece opponentColor Bishop))
                else Nothing
        in msum $ checkMatch <$> bishopsCoords

    threatenedByRock = {-# SCC "m_threatenedByRock" #-} 
        let rockCoords = bitmapToCoords $ opponentBits .&. rocks
            checkMatch (rockX, rockY) =
                if (magicCandidateMovesBitboard rockMagicBitboards rockX rockY opponentColor board) .&. targetBitmap /= 0
                then Just (rockX, rockY, (ChessPiece opponentColor Rock))
                else Nothing
        in msum $ checkMatch <$> rockCoords

    threatenedByQueen = {-# SCC "m_threatenedByQueen" #-} 
        let queenCoords = bitmapToCoords $ opponentBits .&. queens
            queenMagicBitboard queenX queenY = 
                (magicCandidateMovesBitboard bishopMagicBitboards queenX queenY opponentColor board)
                .|. (magicCandidateMovesBitboard rockMagicBitboards queenX queenY opponentColor board)
            checkMatch (queenX, queenY) =
                if (queenMagicBitboard queenX queenY) .&. targetBitmap /= 0
                then Just (queenX, queenY, (ChessPiece opponentColor Queen))
                else Nothing
        in msum $ checkMatch <$> queenCoords

    threatenedByPawn = {-# SCC "m_threatenedByPawn" #-}
      let y' = if player == White then y + 1 else y - 1
          pawnExists x' = inBounds x' y' && hasPieceOnSquare board x' y' (ChessPiece opponentColor Pawn)
          maybePawn x' = if pawnExists x' then Just (x', y', ChessPiece opponentColor Pawn) else Nothing
       in maybePawn (x - 1) <|> maybePawn (x + 1)

    -- we can't just use `playerKingPosition` because a king might not exist
    -- on the board in case this is called during SEE eval
    threatenedByKing = {-# SCC "m_threatenedByKing" #-}
      let opponentKings = (if opponentColor == White then white else black) .&. kings
          matchedKings = opponentKings .&. emptyBoardKingHops x y
          kingsCoords = bitmapToCoords matchedKings
       in case kingsCoords of
            (x, y):_ -> Just (x, y, ChessPiece opponentColor King)
            _ -> Nothing

playerPotentiallyPinned :: ChessBoard -> PlayerColor -> Bool
playerPotentiallyPinned board player =
  any (\ray -> checkRayPin ray False [Queen, Bishop]) (emptyBoardBishopRays x y)
    || any (\ray -> checkRayPin ray False [Queen, Rock]) (emptyBoardRockRays x y)
  where
    opponentColor = if player == White then Black else White
    (x, y) = playerKingPosition board player

    checkRayPin :: [(Int, Int)] -> Bool -> [ChessPieceType] -> Bool
    checkRayPin moves ownPieceSeen pinnerTypes =
      case foldlM foldStepper ownPieceSeen moves of
        Right _ -> False
        Left v -> v
      where
        foldStepper ownPieceSeen (x, y) =
          let ownPiece = isPlayerOnSquare board player x y
              opponentPiece = isPlayerOnSquare board opponentColor x y
           in if not ownPiece && not opponentPiece
                then Right ownPieceSeen
                else
                  if ownPieceSeen && opponentPiece && case pieceOnSquare board x y of
                    Just (ChessPiece color pieceType) -> color == opponentColor && elem pieceType pinnerTypes
                    _ -> False
                    then Left True
                    else
                      if not ownPieceSeen && ownPiece
                        then Right True
                        else Left False

{-# INLINE playerInCheck #-}
playerInCheck :: ChessBoard -> Bool
playerInCheck board = playerInCheck' board (turn board)

{-# INLINE playerInCheck' #-}
playerInCheck' :: ChessBoard -> PlayerColor -> Bool
playerInCheck' board player =
  let (x, y) = playerKingPosition board player
   in isJust $ squareThreatenedBy board player x y

{-# INLINE pawnCandidateMoves #-}
pawnCandidateMoves :: ChessBoard -> Int -> Int -> PlayerColor -> [Move]
pawnCandidateMoves board x y player =
  let (dir, inStartingPos, inEnPassantPos, promotesOnMove, opponent) =
        if player == White
          then (1, y == 2, y == 5, y == 7, Black)
          else (-1, y == 7, y == 4, y == 2, White)
      y' = y + dir
      aheadIsClear = squareEmpty board x y'
      normalCaptures = do
        x' <- [x - 1, x + 1]
        x' <- [x' | x' >= 1 && x' <= 8 && isPlayerOnSquare board opponent x' y']
        if promotesOnMove
          then do
            createMove x y x' y' <$> [PromoQueen, PromoRock, PromoBishop, PromoHorse]
          else return $ createMove x y x' y' NoPromo
      enPassantCaptures = 
        case enPassant board of
            Just col ->
                if inEnPassantPos
                then if col == x - 1 
                     then return $ createMove x y (x - 1) y' NoPromo
                     else if col == x + 1
                          then return $ createMove x y (x + 1) y' NoPromo
                          else []
                else []
            _ -> []
      doubleDipMove =
        let doubleAheadIsClear = squareEmpty board x (y + 2 * dir)
            canDoubleDip = inStartingPos && aheadIsClear && doubleAheadIsClear
         in ([createMove x y x (y + 2 * dir) NoPromo | canDoubleDip])
      singleMove
        | aheadIsClear && not promotesOnMove = [createMove x y x y' NoPromo]
        | aheadIsClear && promotesOnMove = map (\promotion -> createMove x y x (y + dir) promotion) [PromoQueen, PromoRock, PromoHorse, PromoBishop]
        | otherwise = []
   in normalCaptures ++ enPassantCaptures ++ doubleDipMove ++ singleMove

{-# INLINE canCastleKingSide #-}
canCastleKingSide :: ChessBoard -> PlayerColor -> Bool
canCastleKingSide board color =
  let hasRights = if color == White then whiteKingCastle board else blackKingCastle board
      y = if color == White then 1 else 8
      hasEmptySpaces = squareEmpty board 6 y && squareEmpty board 7 y
      travelsThroughCheck = case filter (\x' -> isJust $ squareThreatenedBy board color x' y) [5, 6, 7] of
        [] -> False
        _ -> True
   in hasRights && hasEmptySpaces && not travelsThroughCheck

{-# INLINE canCastleQueenSide #-}
canCastleQueenSide :: ChessBoard -> PlayerColor -> Bool
canCastleQueenSide board color =
  let hasRights = if color == White then whiteQueenCastle board else blackQueenCastle board
      y = if color == White then 1 else 8
      hasEmptySpaces = squareEmpty board 2 y && squareEmpty board 3 y && squareEmpty board 4 y
      travelsThroughCheck = case filter (\x' -> isJust $ squareThreatenedBy board color x' y) [3, 4, 5] of
        [] -> False
        _ -> True
   in hasRights && hasEmptySpaces && not travelsThroughCheck

{-# INLINE kingCandidateMoves #-}
kingCandidateMoves :: ChessBoard -> Int -> Int -> PlayerColor -> [Move]
kingCandidateMoves board x y player =
  let baseMoves =
        map (\(x', y') -> createMove x y x' y' NoPromo) $
          pieceThreats board (x, y, ChessPiece player King)
      castleKingSide = ([createMove x y 7 y NoPromo | canCastleKingSide board player])
      castleQueenSide = ([createMove x y 3 y NoPromo | canCastleQueenSide board player])
   in castleKingSide ++ (castleQueenSide ++ baseMoves)

{-# INLINE pieceCandidateMoves #-}
pieceCandidateMoves :: ChessBoard -> (Int, Int, ChessPiece) -> [Move]
pieceCandidateMoves board (x, y, ChessPiece color Pawn) = pawnCandidateMoves board x y color
pieceCandidateMoves board (x, y, ChessPiece color King) = kingCandidateMoves board x y color
pieceCandidateMoves board piece@(x, y, _) =
  map
    (\(x', y') -> createMove x y x' y' NoPromo)
    (pieceThreats board piece)

-- candidate moves before handling invalid ones (eg., not resolving being in check)
-- ie., pseudo legal
{-# INLINE pseudoLegalCandidateMoves #-}
pseudoLegalCandidateMoves :: ChessBoard -> [Move]
pseudoLegalCandidateMoves board =
  {-# SCC "m_pseudoLegalCandidateMoves" #-}
  let player = turn board
      playerPieces = playerPositionsToList (pieces board) player [Pawn, Bishop, Horse, Rock, Queen, King]
   in concatMap (\p -> pieceCandidateMoves board p) playerPieces

-- returns just if given candidate is legal, empty otherwise
-- (candidate can be illegal because pseudoLegalCandidateMoves returns pseudolegal moves)
{-# INLINE candidateMoveLegal #-}
candidateMoveLegal :: ChessBoard -> Move -> Maybe ChessBoard
candidateMoveLegal board candidate =
  let board' = applyMoveUnsafe board candidate
      inCheck = (wasInCheck || (wasPotentiallyPinned && movePotentiallyBreakingPin) || isKingMove) && playerInCheck' board' player
   in if not inCheck
        then return board'
        else Nothing
  where
    player = turn board
    wasInCheck = playerInCheck board
    wasPotentiallyPinned = playerPotentiallyPinned board player
    (king_x, king_y) = playerKingPosition board player
    squarePotentiallyUnderPin x y =
        y == king_y 
        || x == king_x
        || abs (y - king_y) == abs (x - king_x)
    -- en pessent move might clear a pin held by opponent's pawn
    isEnPassant = case pieceOnSquare board (fromCol candidate) (fromRow candidate) of
      Just (ChessPiece _ Pawn) -> (fromCol candidate) /= (toCol candidate)
      _ -> False
    movePotentiallyBreakingPin = 
        squarePotentiallyUnderPin (fromCol candidate) (fromRow candidate)
        || (isEnPassant && (squarePotentiallyUnderPin (toCol candidate) (fromRow candidate)))
    isKingMove =
      fromRow candidate == king_y && fromCol candidate == king_x

-- change current turn (not forgetting to update zebra hash)
-- used in null move pruning
applyNullMove :: ChessBoard -> ChessBoard
applyNullMove board@ChessBoard{ zebraHash, turn } =
    board { turn = otherPlayer turn, zebraHash = zebraHash `xor` zebraHashBlackTurn, prev = Nothing }

applyMove :: ChessBoard -> Move -> Maybe ChessBoard
applyMove board move = do
  let candidates = pseudoLegalCandidateMoves board
  matchedCandidate <- find (\move' -> move' == move) candidates
  candidateMoveLegal board matchedCandidate

-- return Just (capturingType, capturedType) if this is capture
-- Nothing otherwise
{-# INLINE getCaptureInfo #-}
getCaptureInfo :: ChessBoard -> Move -> Maybe (ChessPieceType, ChessPieceType)
getCaptureInfo board move =
  case pieceOnSquare board (fromCol move) (fromRow move) of
    Just (ChessPiece _ Pawn) ->
      case pieceOnSquare board (toCol move) (toRow move) of
        Just (ChessPiece _ attackedPieceType) -> Just (Pawn, attackedPieceType)
        -- if columns don't match but there was nothing on the target square, then this must have been en passant capture
        _ ->
          if (fromCol move /= toCol move && (enPassant board) == Just (toCol move))
            then Just (Pawn, Pawn)
            else Nothing
    Just (ChessPiece _ attackingPieceType) ->
      case pieceOnSquare board (toCol move) (toRow move) of
        Just (ChessPiece _ attackedPieceType) -> Just (attackingPieceType, attackedPieceType)
        _ -> Nothing
    Nothing -> Nothing

-- returns parsed pieces + rest of input
loadFenPieces :: String -> (Int, Int) -> ChessBoardPositions -> Maybe (ChessBoardPositions, String)
loadFenPieces ('r' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece Black Rock)
loadFenPieces ('n' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece Black Horse)
loadFenPieces ('b' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece Black Bishop)
loadFenPieces ('k' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece Black King)
loadFenPieces ('q' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece Black Queen)
loadFenPieces ('p' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece Black Pawn)
loadFenPieces ('R' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece White Rock)
loadFenPieces ('N' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece White Horse)
loadFenPieces ('B' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece White Bishop)
loadFenPieces ('K' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece White King)
loadFenPieces ('Q' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece White Queen)
loadFenPieces ('P' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ snd $ setPosition 0 parsedPieces x y (ChessPiece White Pawn)
loadFenPieces ('/' : rest) (_, y) parsedPieces =
  loadFenPieces rest (1, y - 1) parsedPieces
loadFenPieces (' ' : rest) _ parsedPieces =
  Just (parsedPieces, rest)
loadFenPieces (char : rest) (x, y) parsedPieces
  | isDigit char = loadFenPieces rest (x + digitToInt char, y) parsedPieces
  | otherwise = Nothing
loadFenPieces _ _ _ = Nothing

loadTurn :: String -> Maybe (PlayerColor, String)
loadTurn ('w' : ' ' : rest) = Just (White, rest)
loadTurn ('b' : ' ' : rest) = Just (Black, rest)
loadTurn _ = Nothing

loadCastlingRights :: String -> (Bool, Bool, Bool, Bool) -> Maybe ((Bool, Bool, Bool, Bool), String)
loadCastlingRights ('-' : ' ' : rest) _ = Just ((False, False, False, False), rest)
loadCastlingRights ('K' : rest) (_, wq, bk, bq) = loadCastlingRights rest (True, wq, bk, bq)
loadCastlingRights ('Q' : rest) (wk, _, bk, bq) = loadCastlingRights rest (wk, True, bk, bq)
loadCastlingRights ('k' : rest) (wk, wq, _, bq) = loadCastlingRights rest (wk, wq, True, bq)
loadCastlingRights ('q' : rest) (wk, wq, bk, _) = loadCastlingRights rest (wk, wq, bk, True)
loadCastlingRights (' ' : rest) rights = Just (rights, rest)
loadCastlingRights _ _ = Nothing

loadEnPassant :: String -> Maybe (Maybe Int, String)
loadEnPassant ('-' : ' ' : rest) = Just (Nothing, rest)
loadEnPassant (x : y : ' ' : rest) = do
  (x', _) <- parseSquareReference [x, y]
  return (Just x', rest)
loadEnPassant _ = Nothing

loadFullMoves :: String -> Maybe (Int, String)
loadFullMoves input = do
  let (fullMoveStr, input') = readNumber "" input
  fullMove <- readMaybe fullMoveStr
  return (fullMove, input')
  where
    readNumber result [] = (result, [])
    readNumber result (x : xs) =
      if isDigit x
        then readNumber (result ++ [x]) xs
        else (result, x : xs)

skipUntilWhitespace :: String -> String
skipUntilWhitespace (' ' : rest) = rest
skipUntilWhitespace [] = []
skipUntilWhitespace (_ : rest) = skipUntilWhitespace rest

loadFen :: String -> Maybe (ChessBoard, String)
loadFen input = do
  (pieces, input) <-
    loadFenPieces
      input
      (1, 8)
      ChessBoardPositions
        { white = zeroBits,
          black = zeroBits,
          bishops = zeroBits,
          horses = zeroBits,
          queens = zeroBits,
          kings = zeroBits,
          pawns = zeroBits,
          rocks = zeroBits
        }
  (turn, input) <- loadTurn input
  ((wk, wq, bk, bq), input) <- loadCastlingRights input (False, False, False, False)
  (enPassant, input) <- loadEnPassant input
  let input' = skipUntilWhitespace input
  let (fullMoves, input'') = case loadFullMoves input' of
        Just r -> r
        _ -> (0, input')
  let board =
        ChessBoard
          { turn = turn,
            fullMoves = fullMoves,
            pieces = pieces,
            enPassant = enPassant,
            whiteKingCastle = wk,
            blackKingCastle = bk,
            whiteQueenCastle = wq,
            blackQueenCastle = bq,
            prev = Nothing,
            zebraHash = 0
          }
  let hashedBoard = board {zebraHash = initialZebraHash board}
  return (hashedBoard, input'')

boardToFen :: ChessBoard -> String
boardToFen board =
  rankToStr 8
    ++ (concatMap (\i -> "/" ++ rankToStr (8 - i)) [1 .. 7])
    ++ " "
    ++ (if (turn board) == White then "w" else "b")
    ++ " "
    ++ castling
    ++ " "
    ++ enPeasent
    ++ " 1 1"
  where
    pieceToStr (Just (ChessPiece Black Pawn)) = "p"
    pieceToStr (Just (ChessPiece Black Horse)) = "n"
    pieceToStr (Just (ChessPiece Black Bishop)) = "b"
    pieceToStr (Just (ChessPiece Black Rock)) = "r"
    pieceToStr (Just (ChessPiece Black Queen)) = "q"
    pieceToStr (Just (ChessPiece Black King)) = "k"
    pieceToStr (Just (ChessPiece White Pawn)) = "P"
    pieceToStr (Just (ChessPiece White Horse)) = "N"
    pieceToStr (Just (ChessPiece White Bishop)) = "B"
    pieceToStr (Just (ChessPiece White Rock)) = "R"
    pieceToStr (Just (ChessPiece White Queen)) = "Q"
    pieceToStr (Just (ChessPiece White King)) = "K"
    pieceToStr Nothing = "1"

    rankToStr rank = concatMap (\file -> pieceToStr $ pieceOnSquare board file rank) [1 .. 8]

    castling =
      let str =
            (if (whiteKingCastle board) then "K" else "")
              ++ (if (whiteQueenCastle board) then "Q" else "")
              ++ (if (blackKingCastle board) then "k" else "")
              ++ (if (blackQueenCastle board) then "q" else "")
       in if (length str) == 0 then "-" else str

    enPeasent = case enPassant board of
      Just n -> fromJust $ squareReferenceToString (n, if turn board == Black then 3 else 6)
      Nothing -> "-"

zebraHashKeys :: Array Int Word64
zebraHashKeys =
  let gen = mkTFGen 69420
   in listArray (0, 780) (randoms gen)

zebraHashPiece :: Int -> Int -> ChessPiece -> Word64
zebraHashPiece x y (ChessPiece color pieceType) =
  let colorOffset = (fromEnum color) * 64 * 6
      pieceOffset = (fromEnum pieceType) * 64
      colOffset = (y - 1) * 8
      rowOffset = x - 1
      index = colorOffset + pieceOffset + colOffset + rowOffset
   in zebraHashKeys!index

zebraHashQueenCastle :: PlayerColor -> Word64
zebraHashQueenCastle White = zebraHashKeys!(64 * 12 + 0)
zebraHashQueenCastle Black = zebraHashKeys!(64 * 12 + 1)

zebraHashKingCastle :: PlayerColor -> Word64
zebraHashKingCastle White = zebraHashKeys!(64 * 12 + 2)
zebraHashKingCastle Black = zebraHashKeys!(64 * 12 + 3)

zebraHashBlackTurn :: Word64
zebraHashBlackTurn = zebraHashKeys!(64 * 12 + 4)

zebraHashEnPeasent :: Int -> Word64
zebraHashEnPeasent file = zebraHashKeys!(64 * 12 + 4 + file)

initialZebraHash :: ChessBoard -> Word64
initialZebraHash board@ChessBoard {turn, whiteKingCastle, whiteQueenCastle, blackKingCastle, blackQueenCastle, enPassant} =
  initialZebraHash' turn (boardPositions board) whiteKingCastle whiteQueenCastle blackKingCastle blackQueenCastle enPassant

initialZebraHash' :: PlayerColor -> [(Int, Int, ChessPiece)] -> Bool -> Bool -> Bool -> Bool -> Maybe Int -> Word64
initialZebraHash' turn pieces wkc wqc bkc bqc enPeasent =
  let piecesHashes = foldl' (\h (x, y, piece) -> h `xor` zebraHashPiece x y piece) 0 pieces
      wkcH = if wkc then zebraHashKingCastle White else 0
      wqcH = if wqc then zebraHashQueenCastle White else 0
      bkcH = if bkc then zebraHashKingCastle Black else 0
      bqcH = if bqc then zebraHashQueenCastle Black else 0
      turnH = if turn == Black then zebraHashBlackTurn else 0
      enPH = case enPeasent of
        Just file -> zebraHashEnPeasent file
        _ -> 0
   in piecesHashes `xor` wkcH `xor` wqcH `xor` bkcH `xor` bqcH `xor` turnH `xor` enPH

horseHops :: Array Int Int64
horseHops =
  let squares = do
        x <- [1 .. 8]
        y <- [1 .. 8]
        return (coordsToBitIndex x y, computeHorseHops x y)
   in array (0, 63) squares

computeHorseHops :: Int -> Int -> Int64
computeHorseHops x y =
  let hops =
        [ (x + 1, y + 2),
          (x + 1, y - 2),
          (x - 1, y + 2),
          (x - 1, y - 2),
          (x + 2, y + 1),
          (x + 2, y - 1),
          (x - 2, y + 1),
          (x - 2, y - 1)
        ]
   in coordsToBitmap $ filter (\(x', y') -> inBounds x' y') hops

emptyBoardHorseHops :: Int -> Int -> Int64
emptyBoardHorseHops x y = horseHops ! coordsToBitIndex x y

kingHops :: Array Int Int64
kingHops =
  let squares = do
        x <- [1 .. 8]
        y <- [1 .. 8]
        return (coordsToBitIndex x y, computeKingHops x y)
   in array (0, 63) squares

computeKingHops :: Int -> Int -> Int64
computeKingHops x y =
  let hops = [(x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1]]
   in coordsToBitmap $ filter (\(x', y') -> inBounds x' y' && not (x == x' && y == y')) hops

emptyBoardKingHops :: Int -> Int -> Int64
emptyBoardKingHops x y = kingHops!coordsToBitIndex x y

data FileState = OpenFile | SemiOpenFile | ClosedFile

-- array mapping (file 1..8), (fromRow 2..7), (toRow 2..7) to bitboard for pawn detection
fileBitsArray :: Array (Int, Int, Int) Int64
fileBitsArray =
    let content = do
            x <- [1..8]
            y1 <- [2..7]
            y2 <- [2..7]
            return ((x, y1, y2), fileBits x (min y1 y2) (max y1 y2))
    in array ((1, 2, 2), (8, 7, 7)) content
    where
        fileBits x y1 y2 = foldl' (\bits y -> setBit bits (coordsToBitIndex x y)) 0 [y1..y2]

{-# INLINE fileState #-}
fileState :: ChessBoard -> Int -> PlayerColor -> FileState
fileState ChessBoard { pieces = ChessBoardPositions { white, black, pawns }} x color =
    let fileBits = fileBitsArray!(x, 2, 7)
        whiteBlock = (white .&. pawns .&. fileBits) > 0
        blackBlock = (black .&. pawns .&. fileBits) > 0
        (myBlock, opponentBlock) = if color == White then (whiteBlock, blackBlock) else (blackBlock, whiteBlock)
    in if myBlock
       then ClosedFile
       else if opponentBlock
       then SemiOpenFile
       else OpenFile

{-# INLINE isPassedPawn #-}
isPassedPawn :: ChessBoard -> Int -> Int -> PlayerColor -> Bool
isPassedPawn ChessBoard { pieces = ChessBoardPositions { black, pawns }} x y White =
    let linesToCheck = do
            x' <- [x'' | x'' <- [x - 1 .. x + 1], x'' >= 1 && x'' <= 8]
            return $ fileBitsArray!(x', y + 1, 7)
        opponentPawns = black .&. pawns
        lineIsClear bits = opponentPawns .&. bits == 0
        onLastRow = y == 7
    in if onLastRow 
       then True
       else all lineIsClear linesToCheck
isPassedPawn ChessBoard { pieces = ChessBoardPositions { white, pawns }} x y Black =
    let linesToCheck = do
            x' <- [x'' | x'' <- [x - 1 .. x + 1], x'' >= 1 && x'' <= 8]
            return $ fileBitsArray!(x', 2, y - 1)
        opponentPawns = white .&. pawns
        lineIsClear bits = opponentPawns .&. bits == 0
        onLastRow = y == 2
    in if onLastRow 
       then True
       else all lineIsClear linesToCheck

{-# INLINE isBackwardPawn #-}
isBackwardPawn :: ChessBoard -> Int -> Int -> PlayerColor -> Bool
isBackwardPawn ChessBoard { pieces = ChessBoardPositions { white, pawns }} x y White =
    let onLastRow = y == 7
        lineToCheck = fileBitsArray!(x, y + 1, 7)
    in if onLastRow
       then False
       else (white .&. pawns .&. lineToCheck) /= 0
isBackwardPawn ChessBoard { pieces = ChessBoardPositions { black, pawns }} x y Black =
    let onLastRow = y == 2
        lineToCheck = fileBitsArray!(x, 2, y - 1)
    in if onLastRow
       then False
       else (black .&. pawns .&. lineToCheck) /= 0

pawnShieldBitsArray :: Array (Int, Int) Int64
pawnShieldBitsArray =
    let content = do
            x <- [1..6]
            y <- [2..6]
            let coords = [(x', y') | x' <- [x, x + 1, x + 2], y' <- [y, y + 1]]
            let bitmap = coordsToBitmap coords
            return ((x, y), bitmap)
    in array ((1, 2), (6, 6)) content

{-# INLINE countPawnShield #-}
countPawnShield :: ChessBoard -> Int -> Int -> PlayerColor -> Int
countPawnShield ChessBoard { pieces = ChessBoardPositions { white, black, pawns } } kingX kingY player =
  let x1 = case kingX of
        1 -> 1
        8 -> 6
        n -> n - 1
      y1 = case player of
        White -> kingY + 1
        Black -> kingY - 2
      shieldMask = if inRange (bounds pawnShieldBitsArray) (x1, y1) then pawnShieldBitsArray!(x1, y1) else 0
      matchedPawns = (if player == White then white else black) .&. pawns .&. shieldMask
  in popCount matchedPawns




  ----------------- MAGIC TIME

data MagicBitboard  = MagicBitboard 
    { magicMul :: !Int64
    , magicShift :: !Int
    , mask :: !Int64
    , boards :: IntMap.IntMap Int64
    } deriving (Show)

instance NFData MagicBitboard where
    rnf (MagicBitboard { boards }) = boards `deepseq` ()

-- returns all possible permutations of bitfield
-- for bits that are set 1 in input
permutateBits :: Int64 -> [Int64]
permutateBits n =
    if n == 0
    then [0]
    else let
           index = countTrailingZeros n
           otherBits = clearBit n index
           otherPermutations = permutateBits otherBits
         in 
           ((\bits -> setBit bits index) <$> otherPermutations) ++ otherPermutations

unfoldBitmapRay :: Int64 -> Int -> Int -> Int -> Int -> Int64
unfoldBitmapRay blocker x y dx dy = force $ go 0 (x + dx) (y + dy)
  where
    go result x' y' =
        if x' < 1 || x' > 8 || y' < 1 || y' > 8
        then result
        else if testBit blocker (coordsToBitIndex x' y')
             then setBit result (coordsToBitIndex x' y')
             else go (setBit result (coordsToBitIndex x' y')) (x' + dx) (y' + dy)

borderMask :: Int64
borderMask = 0

computeRockMask :: Int64 -> Int -> Int -> Int64
computeRockMask blockerMask x y =
    (unfoldBitmapRay blockerMask x y 1 0)
    .|. (unfoldBitmapRay blockerMask x y 0 1)
    .|. (unfoldBitmapRay blockerMask x y (-1) 0)
    .|. (unfoldBitmapRay blockerMask x y 0 (-1))

computeBishopMask :: Int64 -> Int -> Int -> Int64
computeBishopMask blockerMask x y =
    (unfoldBitmapRay blockerMask x y 1 1)
    .|. (unfoldBitmapRay blockerMask x y 1 (-1))
    .|. (unfoldBitmapRay blockerMask x y (-1) 1)
    .|. (unfoldBitmapRay blockerMask x y (-1) (-1))

{-# INLINE computeMagicBitboardKey #-}
computeMagicBitboardKey :: Int64 -> Int64 -> Int -> Int64
computeMagicBitboardKey blockBitmap magicMul magicShift =
    blockBitmap
{-
    let blockBitmapW :: Word64
        blockBitmapW = fromIntegral blockBitmap
        magicMulW :: Word64
        magicMulW = fromIntegral magicMul
        !result = fromIntegral $ (blockBitmapW * magicMulW) `shiftR` magicShift
    in result
-}

-- check if given magic numbers produce collisions;
-- if not returns complete magic board
tryMagicNumbers :: Int -> Int -> Int64 -> Int -> Int64 -> (Int64 -> Int -> Int -> Int64) -> Maybe MagicBitboard
tryMagicNumbers x y magicMul magicShift mask candidateMovesResolver =
    let possibleBlockBitmaps = permutateBits mask
    in case foldM step (Set.empty, []) possibleBlockBitmaps of
        Just (_, computedValues) -> Just (MagicBitboard magicMul magicShift mask (IntMap.fromList computedValues))
        _ -> Nothing
  where
    step :: (Set.Set Int64, [(IntMap.Key, Int64)]) -> Int64 -> Maybe (Set.Set Int64, [(IntMap.Key, Int64)])
    step (usedKeys, computedValues) blockBitmap =
        let !key = computeMagicBitboardKey blockBitmap magicMul magicShift --(abs (blockBitmap * magicMul)) `shiftR` magicShift
            !value = candidateMovesResolver blockBitmap x y
        in if Set.member key usedKeys
           then Nothing -- key was already placed, collision, abort
           else Just (Set.insert key usedKeys, (fromIntegral key, value):computedValues)

findMagicNumbers :: Int -> Int -> Int -> Int64 -> (Int64 -> Int -> Int -> Int64) -> MagicBitboard
findMagicNumbers x y magicShift mask candidateMoveResolver =
    let numberGen = mkTFGen 123
        magicMulOptions = take 100 $ randoms numberGen
        magicMulResults = (\magicMul -> tryMagicNumbers x y magicMul magicShift mask candidateMoveResolver) <$> magicMulOptions
    in case msum magicMulResults of
        Just v -> v
        _ -> error "Unexpected failure to find magic bitboard"

rockMagicBitboards :: Array (Int, Int) MagicBitboard
rockMagicBitboards =
    let values = do
            x <- [1..8]
            y <- [1..8]
            let mask = computeRockMask borderMask x y
            let !magic = findMagicNumbers x y 44 mask computeRockMask
            return ((x, y), magic)
    in array ((1, 1), (8, 8)) values

bishopMagicBitboards :: Array (Int, Int) MagicBitboard
bishopMagicBitboards =
    let values = do
            x <- [1..8]
            y <- [1..8]
            let mask = computeBishopMask borderMask x y
            let !magic = findMagicNumbers x y 44 mask computeBishopMask
            return ((x, y), magic)
    in array ((1, 1), (8, 8)) values

{-# INLINE magicCandidateMovesBitboard #-}
magicCandidateMovesBitboard :: Array (Int, Int) MagicBitboard -> Int -> Int -> PlayerColor -> ChessBoard -> Int64
magicCandidateMovesBitboard magicBitboards x y player ChessBoard { pieces =  ChessBoardPositions { white, black } } =
    let MagicBitboard { magicMul, magicShift, mask, boards } = magicBitboards ! (x, y)
        occupancy = mask .&. (white .|. black)
        key = computeMagicBitboardKey occupancy magicMul magicShift -- (abs (occupancy * magicMul)) `shiftR` magicShift
        bitboard = (IntMap.!) boards (fromIntegral key)
        myPieces = if player == White then white else black
    in bitboard .&. (complement myPieces)

{-# INLINE rockCandidateMoves #-}
rockCandidateMoves ::  Int -> Int -> PlayerColor -> ChessBoard -> [(Int, Int)]
rockCandidateMoves x y player board = bitmapToCoords $ magicCandidateMovesBitboard rockMagicBitboards x y player board

{-# INLINE bishopCandidateMoves #-}
bishopCandidateMoves :: Int -> Int -> PlayerColor -> ChessBoard -> [(Int, Int)]
bishopCandidateMoves x y player board = bitmapToCoords $ magicCandidateMovesBitboard bishopMagicBitboards x y player board

{-# INLINE queenCandidateMoves #-}
queenCandidateMoves :: Int -> Int -> PlayerColor -> ChessBoard -> [(Int, Int)]
queenCandidateMoves x y player board = bitmapToCoords $ 
    magicCandidateMovesBitboard rockMagicBitboards x y player board
    .|. magicCandidateMovesBitboard bishopMagicBitboards x y player board


debugRenderBitmap :: Int64 -> IO ()
debugRenderBitmap bitmap =
  putStrLn $ intercalate "\n" $ makeLine <$> [1..8]
  where
    makeLine y = intercalate " " $ (\x -> if testBit bitmap (coordsToBitIndex x (9 - y)) then "X" else "O") <$> [1..8]

debugMagicBitboardRockCandidates :: String -> Int -> Int -> IO ()
debugMagicBitboardRockCandidates fen x y = do
    let (board, _) = fromJust $ loadFen fen
    let bitboard = magicCandidateMovesBitboard rockMagicBitboards x y (turn board) board
    debugRenderBitmap bitboard
    
debugPlayerInCheck :: String -> PlayerColor -> Bool
debugPlayerInCheck fen color =
    let (board, _) = fromJust $ loadFen fen
    in playerInCheck' board color
    
debugPlayerPosition :: String -> PlayerColor -> (Int, Int)
debugPlayerPosition fen color =
    let (board, _) = fromJust $ loadFen fen
    in playerKingPosition board color

debugSquareThreatenedBy :: String -> Int -> Int -> PlayerColor -> Maybe (Int, Int, ChessPiece)
debugSquareThreatenedBy fen x y color =
    let (board, _) = fromJust $ loadFen fen
    in squareThreatenedBy board color x y

initPrecomputation :: ()
initPrecomputation =
    rockMagicBitboards 
    `deepseq` bishopMagicBitboards
    `deepseq` kingHops
    `deepseq` horseHops
    `deepseq` ()
