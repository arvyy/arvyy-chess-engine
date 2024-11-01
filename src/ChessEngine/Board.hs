{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module ChessEngine.Board
  ( ChessPieceType (..),
    PlayerColor (..),
    otherPlayer,
    ChessPiece (..),
    Move (..),
    Coord (..),
    makeCoord,
    unpackCoord,
    pattern Coord',
    CoordWithPiece (..),
    makeCoordWithPiece,
    unpackCoordWithPiece,
    pattern CoordWithPiece',
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
    boardNonPawnPositions,
    boardPawnPositions,
    hasPieceOnSquare,
    pieceOnSquare,
    quickMaterialCount,
    initialBoard,
    applyMoveUnsafe,
    is3foldRepetition,
    getCaptureInfo,
    playerKingPosition,
    parseMove,
    moveToString,
    loadFen,
    boardToFen,
    emptyOrOccupiedByOpponent,
    isPlayerOnSquare,
    inBounds,
    playerPositionsToList
  )
where

import Data.Bits
import Data.Char
import Data.Foldable (foldl')
import Data.Hashable
import Data.Int (Int64)
import Data.Maybe
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Data.Word (Word64, Word8, Word16)
import Data.Array.IArray
import System.Random.TF (mkTFGen)
import System.Random (randoms)
import Data.List (unfoldr)

newtype Coord = Coord Word8 deriving (Eq, Show)

{-# INLINE makeCoord #-}
makeCoord :: Int -> Int -> Coord
makeCoord x y =
    let bit = ((x - 1) `shiftL` 3) .|. (y - 1)
    in Coord $ fromIntegral bit

{-# INLINE unpackCoord #-}
unpackCoord :: Coord -> (Int, Int)
unpackCoord (Coord n) =
    (fromIntegral (n `shiftR` 3) + 1, fromIntegral (n .&. 7) + 1)

pattern Coord' :: Int -> Int -> Coord
pattern Coord' x y <- (unpackCoord -> (x, y))

newtype CoordWithPiece = CoordWithPiece Word16 deriving (Eq, Show)

{-# INLINE makeCoordWithPiece #-}
makeCoordWithPiece :: Coord -> ChessPiece -> CoordWithPiece
makeCoordWithPiece (Coord n) (ChessPiece color pieceType) =
    let bit = fromIntegral n .|. ((fromEnum color) `shiftL` 6) .|. ((fromEnum pieceType) `shiftL` 7)
    in CoordWithPiece $ fromIntegral bit

{-# INLINE unpackCoordWithPiece #-}
unpackCoordWithPiece :: CoordWithPiece -> (Coord, ChessPiece)
unpackCoordWithPiece (CoordWithPiece n) =
    let coord = fromIntegral $ n .&. 63
        color = fromIntegral $ (n `shiftR` 6) .&. 1
        pieceType = fromIntegral $ n `shiftR` 7
    in (Coord coord, ChessPiece (toEnum color) (toEnum pieceType))

pattern CoordWithPiece' :: Coord -> ChessPiece -> CoordWithPiece
pattern CoordWithPiece' coord chessPiece <- (unpackCoordWithPiece -> (coord, chessPiece))

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

positionsToList :: ChessBoardPositions -> [ChessPieceType] -> [CoordWithPiece]
positionsToList positions types =
  playerPositionsToList positions White types ++ playerPositionsToList positions Black types

playerPositionsToList :: ChessBoardPositions -> PlayerColor -> [ChessPieceType] -> [CoordWithPiece]
playerPositionsToList ChessBoardPositions {black, white, bishops, horses, queens, kings, pawns, rocks} color types =
    concatMap getTypeValues types
    where
        playerbitmap = if color == White then white else black

        getTypeValues :: ChessPieceType -> [CoordWithPiece]
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
          coord <- bitmapToCoords bitmap'
          return (makeCoordWithPiece coord (ChessPiece color pieceType))

{-# INLINE playerKingPosition #-}
playerKingPosition :: ChessBoard -> PlayerColor -> Coord
playerKingPosition ChessBoard {pieces = ChessBoardPositions {black, white, kings}} color =
  let bitmap = (if color == White then white else black) .&. kings
   in bitIndexToCoords (countTrailingZeros bitmap)

bitmapToCoords :: Int64 -> [Coord]
bitmapToCoords bitmap =
    unfoldr unfoldStep bitmap
  where
    unfoldStep bitmap =
        let index = countTrailingZeros bitmap
         in if index == 64
            then Nothing
            else Just (bitIndexToCoords index, clearBit bitmap index)

bitIndexToCoords :: Int -> Coord
bitIndexToCoords index =
  let x' = mod index 8
      y' = div index 8
   in makeCoord (x' + 1) (y' + 1)

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
        let positions = ChessBoardPositions {
                            black = if color == Black then (clearBit black bitIndex) else black,
                            white = if color == White then (clearBit white bitIndex) else white,
                            pawns = if pieceType == Pawn then (clearBit pawns bitIndex) else pawns,
                            horses = if pieceType == Horse then (clearBit horses bitIndex) else horses,
                            bishops = if pieceType == Bishop then (clearBit bishops bitIndex) else bishops,
                            rocks = if pieceType == Rock then (clearBit rocks bitIndex) else rocks,
                            queens = if pieceType == Queen then (clearBit queens bitIndex) else queens,
                            kings = if pieceType == King then (clearBit kings bitIndex) else kings }
        in (hash `xor` zebraHashPiece x y piece, positions)
    Nothing -> (hash, positions)
  where
    bitIndex = coordsToBitIndex x y

{-# INLINE setPosition #-}
setPosition :: Word64 -> ChessBoardPositions -> Int -> Int -> ChessPiece -> (Word64, ChessBoardPositions)
setPosition hash positions x y piece@(ChessPiece color pieceType) =
  let (hash', (ChessBoardPositions black white bishops horses queens kings pawns rocks)) = clearPosition hash positions x y
      positions' = ChessBoardPositions
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

boardPositions :: ChessBoard -> [CoordWithPiece]
boardPositions ChessBoard {pieces = pieces} = positionsToList pieces [Pawn, Bishop, Horse, Rock, Queen, King]

boardNonPawnPositions :: ChessBoard -> [CoordWithPiece]
boardNonPawnPositions ChessBoard {pieces = pieces} = positionsToList pieces [Bishop, Horse, Rock, Queen, King]

boardPawnPositions :: ChessBoard -> [CoordWithPiece]
boardPawnPositions ChessBoard {pieces = pieces} = positionsToList pieces [Pawn]

parseSquareReference :: String -> Maybe Coord
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
  if inBounds x' y' then Just (makeCoord x' y') else Nothing
parseSquareReference _ = Nothing

squareReferenceToString :: Coord -> Maybe String
squareReferenceToString (Coord' x y) = do
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
  Coord' ix iy <- parseSquareReference [x, y]
  Coord' ix2 iy2 <- parseSquareReference [x', y']
  promotion <- case rest of
    [] -> Just NoPromo
    ['n'] -> Just PromoHorse
    ['b'] -> Just PromoBishop
    ['q'] -> Just PromoQueen
    ['r'] -> Just PromoRock
    _ -> Nothing
  return $ createMove ix iy ix2 iy2 promotion
parseMove _ = Nothing

moveToString :: Move -> Maybe String
moveToString move = do
  let fromx = fromCol move
  let fromy = fromRow move
  let tox = toCol move
  let toy = toRow move
  let promo = promotion move
  from <- squareReferenceToString (makeCoord fromx fromy)
  to <- squareReferenceToString (makeCoord tox toy)
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

-- applies move blindly without validation for checks or piece movement rules
-- partial function if reference position is empty
applyMoveUnsafe :: ChessBoard -> Move -> ChessBoard
applyMoveUnsafe board move =
  let ChessPiece player pieceType = case pieceOnSquare board x y of
        Nothing -> error $ "Unsafe move tried to move unexisting piece. Square: " ++ show (x, y)
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
      hash'' = hash'
        `xor` (if blackQueenCastle' /= (blackQueenCastle board) then zebraHashQueenCastle Black else 0)
        `xor` (if blackKingCastle' /=  (blackKingCastle board)  then zebraHashKingCastle Black else 0)
        `xor` (if whiteQueenCastle' /= (whiteQueenCastle board) then zebraHashQueenCastle White else 0)
        `xor` (if whiteKingCastle' /=  (whiteKingCastle board)  then zebraHashKingCastle White else 0)
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

inBounds :: Int -> Int -> Bool
inBounds x y = x >= 1 && x <= 8 && y >= 1 && y <= 8

emptyOrOccupiedByOpponent :: ChessBoard -> PlayerColor -> Coord -> Bool
emptyOrOccupiedByOpponent ChessBoard {pieces = ChessBoardPositions {white = white}} White (Coord' x y) =
  let bitIndex = coordsToBitIndex x y
   in inBounds x y && not (testBit white bitIndex)
emptyOrOccupiedByOpponent ChessBoard {pieces = ChessBoardPositions {black = black}} Black (Coord' x y) =
  let bitIndex = coordsToBitIndex x y
   in inBounds x y && not (testBit black bitIndex)


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

-- return if given board is in 3fold repetition state
is3foldRepetition :: ChessBoard -> Bool
is3foldRepetition board = seenCount >= 2
  where
    isSamePos board1 board2 =
      ((pieces board1) == (pieces board2))
        && ((blackQueenCastle board1) == (blackQueenCastle board2))
        && ((blackKingCastle board1) == (blackKingCastle board2))
        && ((whiteQueenCastle board1) == (whiteQueenCastle board2))
        && ((whiteKingCastle board1) == (whiteKingCastle board2))
        && ((enPassant board1) == (enPassant board2))

    unfoldBoards b@ChessBoard {prev} =
      case prev of
        Just (ChessBoard {prev = Just b'}) -> b : unfoldBoards b'
        _ -> [b]

    otherBoards = drop 1 $ unfoldBoards board

    seenCount = length $ filter (isSamePos board) otherBoards

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
  Coord' x' _ <- parseSquareReference [x, y]
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
  let hashedBoard = board { zebraHash = initialZebraHash board }
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
                    Just n -> fromJust $ squareReferenceToString $ makeCoord n (if turn board == Black then 3 else 6)
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
    in zebraHashKeys ! index

zebraHashQueenCastle :: PlayerColor -> Word64
zebraHashQueenCastle White = zebraHashKeys ! (64 * 12 + 0)
zebraHashQueenCastle Black = zebraHashKeys ! (64 * 12 + 1)

zebraHashKingCastle :: PlayerColor -> Word64
zebraHashKingCastle White = zebraHashKeys ! (64 * 12 + 2)
zebraHashKingCastle Black = zebraHashKeys ! (64 * 12 + 3)

zebraHashBlackTurn :: Word64
zebraHashBlackTurn = zebraHashKeys ! (64 * 12 + 4)

zebraHashEnPeasent :: Int -> Word64
zebraHashEnPeasent file = zebraHashKeys ! (64 * 12 + 4 + file)

initialZebraHash :: ChessBoard -> Word64
initialZebraHash board@ChessBoard { turn, whiteKingCastle, whiteQueenCastle, blackKingCastle, blackQueenCastle, enPassant } =
    initialZebraHash' turn (boardPositions board) whiteKingCastle whiteQueenCastle blackKingCastle blackQueenCastle enPassant

initialZebraHash' :: PlayerColor -> [CoordWithPiece] -> Bool -> Bool -> Bool -> Bool -> Maybe Int -> Word64
initialZebraHash' turn pieces wkc wqc bkc bqc enPeasent =
    let piecesHashes = foldl' (\h (CoordWithPiece' (Coord' x y) piece) -> h `xor` zebraHashPiece x y piece) 0 pieces
        wkcH = if wkc then zebraHashKingCastle White else 0
        wqcH = if wqc then zebraHashQueenCastle White else 0
        bkcH = if bkc then zebraHashKingCastle Black else 0
        bqcH = if bqc then zebraHashQueenCastle Black else 0
        turnH = if turn == Black then zebraHashBlackTurn else 0
        enPH = case enPeasent of
                Just file -> zebraHashEnPeasent file
                _ -> 0
    in piecesHashes `xor` wkcH `xor` wqcH `xor` bkcH `xor` bqcH `xor` turnH `xor` enPH
