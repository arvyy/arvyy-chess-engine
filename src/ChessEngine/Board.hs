{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module ChessEngine.Board
  ( ChessPieceType (..),
    PlayerColor (..),
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
    boardNonPawnPositions,
    boardPawnPositions,
    pieceOnSquare,
    pseudoLegalCandidateMoves,
    candidateMoveLegal,
    initialBoard,
    applyMove,
    applyMoveUnsafe,
    isCaptureMove,
    playerInCheck,
    parseMove,
    moveToString,
    loadFen,
    pieceThreats,
    boardToFen
  )
where

import Data.Bits
import Data.Char
import Data.Hashable
import Data.Int (Int64)
import Data.Maybe
import GHC.Generics (Generic)
import Data.Foldable (find)
import ChessEngine.PrecomputedCandidateMoves

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
  playerPositionsToList positions White types ++ playerPositionsToList positions Black types

-- returns list of positions for given player
playerPositionsToList :: ChessBoardPositions -> PlayerColor -> [ChessPieceType] -> [(Int, Int, ChessPiece)]
playerPositionsToList positions@ChessBoardPositions {black, white, bishops, horses, queens, kings, pawns, rocks} color (t : rest) =
  let bitmap = case t of
        Pawn -> pawns
        Bishop -> bishops
        Horse -> horses
        Rock -> rocks
        Queen -> queens
        King -> kings
   in collectValues t bitmap ++ playerPositionsToList positions color rest
  where
    playerbitmap = if color == White then white else black
    collectValues pieceType bitmap = do
      let bitmap' = bitmap .&. playerbitmap
      (x, y) <- bitmapToCoords bitmap'
      return (x, y, ChessPiece color pieceType)
playerPositionsToList _ _ [] = []

{-# INLINE playerKingPosition #-}
playerKingPosition :: ChessBoardPositions -> PlayerColor -> (Int, Int)
playerKingPosition ChessBoardPositions {black, white, kings} color =
  let bitmap = (if color == White then white else black) .&. kings
   in bitIndexToCoords (countTrailingZeros bitmap)

bitmapToCoords :: Int64 -> [(Int, Int)]
bitmapToCoords bitmap =
  if index == 64
    then []
    else bitIndexToCoords index : bitmapToCoords (clearBit bitmap index)
  where
    index = countTrailingZeros bitmap

bitIndexToCoords :: Int -> (Int, Int)
bitIndexToCoords index =
  let x' = mod index 8
      y' = div index 8
   in (x' + 1, y' + 1)

{-# INLINE coordsToBitIndex #-}
coordsToBitIndex :: Int -> Int -> Int
coordsToBitIndex x y =
  let value = (y - 1) * 8 + x - 1
   in if value > 63 then error ("Coordinate out of bounds: " ++ show (x, y)) else value

{-# INLINE clearPosition #-}
clearPosition :: ChessBoardPositions -> Int -> Int -> ChessBoardPositions
clearPosition positions@(ChessBoardPositions black white bishops horses queens kings pawns rocks) x y =
  if not $ testBit (black .|. white) bitIndex
  then positions
  else ChessBoardPositions
    { black = clearBit black bitIndex,
      white = clearBit white bitIndex,
      bishops = clearBit bishops bitIndex,
      horses = clearBit horses bitIndex,
      queens = clearBit queens bitIndex,
      kings = clearBit kings bitIndex,
      pawns = clearBit pawns bitIndex,
      rocks = clearBit rocks bitIndex
    }
  where
    bitIndex = coordsToBitIndex x y

{-# INLINE setPosition #-}
setPosition :: ChessBoardPositions -> Int -> Int -> ChessPiece -> ChessBoardPositions
setPosition positions x y (ChessPiece color pieceType) =
  let (ChessBoardPositions black white bishops horses queens kings pawns rocks) = clearPosition positions x y
   in ChessBoardPositions
        { black = if color == Black then setBit black bitIndex else black,
          white = if color == White then setBit white bitIndex else white,
          bishops = if pieceType == Bishop then setBit bishops bitIndex else bishops,
          horses = if pieceType == Horse then setBit horses bitIndex else horses,
          queens = if pieceType == Queen then setBit queens bitIndex else queens,
          kings = if pieceType == King then setBit kings bitIndex else kings,
          pawns = if pieceType == Pawn then setBit pawns bitIndex else pawns,
          rocks = if pieceType == Rock then setBit rocks bitIndex else rocks
        }
  where
    bitIndex = coordsToBitIndex x y

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

piecePriority :: ChessPieceType -> Int
piecePriority = fromEnum

data PlayerColor = Black | White deriving (Eq, Show, Ord, Generic)

data ChessPiece = ChessPiece !PlayerColor !ChessPieceType deriving (Show, Eq)

data ChessBoard = ChessBoard
  { turn :: !PlayerColor,
    pieces :: !ChessBoardPositions,
    -- if last move was double pawn move, indicates the file for potential en passant followup
    enPassant :: !(Maybe Int),
    -- state for castling resolution
    whiteKingCastle :: !Bool,
    whiteQueenCastle :: !Bool,
    blackKingCastle :: !Bool,
    blackQueenCastle :: !Bool
  }
  deriving (Eq, Show, Ord, Generic)

instance Hashable PlayerColor

instance Hashable ChessBoardPositions

instance Hashable ChessBoard

data PromoChessPieceType = NoPromo | PromoHorse | PromoRock | PromoQueen | PromoBishop deriving (Show, Eq, Enum)

newtype Move = Move Int64 deriving (Eq)

instance Show Move
    where show move@(Move bitRepr) = case moveToString move of 
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
isPlayerOnSquare ChessBoard { pieces = ChessBoardPositions{ white } } White x y =
    let bitIndex = coordsToBitIndex x y
    in testBit white bitIndex
isPlayerOnSquare ChessBoard { pieces = ChessBoardPositions{ black } } Black x y =
    let bitIndex = coordsToBitIndex x y
    in testBit black bitIndex

pieceOnSquare :: ChessBoard -> Int -> Int -> Maybe ChessPiece
pieceOnSquare board = pieceOnSquare' (pieces board)

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
applyMoveUnsafe :: ChessBoard -> Move -> ChessBoard
applyMoveUnsafe board move =
  let ChessPiece player pieceType = case pieceOnSquare board x y of
        Nothing -> error $ "Unsafe move tried to move unexisting piece. Square: " ++ show (x, y)
        Just f -> f
      isEnPassantMove = pieceType == Pawn && enPassant board == Just x' && (if player == White then y == 5 else y == 4)
      isDoubleDipMove = pieceType == Pawn && abs (y - y') == 2
      isKingCastleMove = pieceType == King && x == 5 && x' == 7
      isQueenCastleMove = pieceType == King && x == 5 && x' == 3
      isCastleMove = isKingCastleMove || isQueenCastleMove
      oldPieces = pieces board
      newPieces
        | isEnPassantMove = applyEnPassant oldPieces player
        | isKingCastleMove = applyKingCastle oldPieces player
        | isQueenCastleMove = applyQueenCastle oldPieces player
        | otherwise = applyNormalMove oldPieces player pieceType
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
   in board
        { pieces = newPieces,
          enPassant = if isDoubleDipMove then Just x else Nothing,
          turn = otherPlayer (turn board),
          whiteKingCastle = whiteKingCastle',
          blackKingCastle = blackKingCastle',
          whiteQueenCastle = whiteQueenCastle',
          blackQueenCastle = blackQueenCastle'
        }
  where
    (x, y, x', y', promotion) = moveToTuple move
    applyKingCastle :: ChessBoardPositions -> PlayerColor -> ChessBoardPositions
    applyKingCastle positions color =
      let p1 = clearPosition positions 5 y
          p2 = clearPosition p1 8 y
          p3 = setPosition p2 6 y (ChessPiece color Rock)
          p4 = setPosition p3 7 y (ChessPiece color King)
       in p4

    applyQueenCastle :: ChessBoardPositions -> PlayerColor -> ChessBoardPositions
    applyQueenCastle positions color =
      let p1 = clearPosition positions 1 y
          p2 = clearPosition p1 5 y
          p3 = setPosition p2 4 y (ChessPiece color Rock)
          p4 = setPosition p3 3 y (ChessPiece color King)
       in p4

    applyEnPassant :: ChessBoardPositions -> PlayerColor -> ChessBoardPositions
    applyEnPassant positions color =
      let p1 = clearPosition positions x y
          p2 = clearPosition p1 x' y
          p3 = setPosition p2 x' y' (ChessPiece color Pawn)
       in p3

    applyNormalMove :: ChessBoardPositions -> PlayerColor -> ChessPieceType -> ChessBoardPositions
    applyNormalMove positions color pieceType =
      let p1 = clearPosition positions x y
          newMovedPiece = case promotion of
            NoPromo -> ChessPiece color pieceType
            PromoHorse -> ChessPiece color Horse
            PromoRock -> ChessPiece color Rock
            PromoQueen -> ChessPiece color Queen
            PromoBishop -> ChessPiece color Bishop
          p2 = setPosition p1 x' y' newMovedPiece
       in p2

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
pieceThreats :: ChessBoard -> (Int, Int, ChessPiece) -> [(Int, Int)]
pieceThreats board (x, y, ChessPiece color King) =
  let candidates = filter (/= (x, y)) [(x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1]]
   in {-# SCC "m_pieceThreats_King" #-} filter
        (emptyOrOccupiedByOpponent board color)
        candidates
pieceThreats board (x, y, ChessPiece color Queen) = {-# SCC "m_pieceThreats_Queen" #-} (pieceThreats board (x, y, ChessPiece color Rock) ++ pieceThreats board (x, y, ChessPiece color Bishop))
pieceThreats board (x, y, ChessPiece color Pawn) = {-# SCC "m_pieceThreats_Pawn" #-}
  let nextRow = if color == Black then y - 1 else y + 1
      candidates = [(x - 1, nextRow), (x + 1, nextRow)]
   in filter
        (emptyOrOccupiedByOpponent board color)
        candidates
pieceThreats board (x, y, ChessPiece color Bishop) = {-# SCC "m_pieceThreats_Bishop" #-}
   let rays = emptyBoardBishopRays x y
   in concatMap (rayToValidMoves color board) rays
pieceThreats board (x, y, ChessPiece color Rock) = {-# SCC "m_pieceThreats_Rock" #-}
   let rays = emptyBoardRockRays x y
   in concatMap (rayToValidMoves color board) rays
pieceThreats board (x, y, ChessPiece color Horse) = {-# SCC "m_pieceThreats_Horse" #-} 
   let candidates =
        [ (x + 1, y + 2),
          (x + 1, y - 2),
          (x - 1, y + 2),
          (x - 1, y - 2),
          (x + 2, y + 1),
          (x + 2, y - 1),
          (x - 2, y + 1),
          (x - 2, y - 1)
        ]
   in  filter
        (emptyOrOccupiedByOpponent board color)
        candidates

rayToValidMoves :: PlayerColor -> ChessBoard -> [(Int, Int)] -> [(Int, Int)]
rayToValidMoves color board squares = filterUntilHit squares
  where
    filterUntilHit :: [(Int, Int)] -> [(Int, Int)]
    filterUntilHit ((x, y) : rest) 
        | isPlayerOnSquare board color x y = []
        | isPlayerOnSquare board (otherPlayer color) x y = [(x, y)]
        | otherwise = (x, y) : filterUntilHit rest
    filterUntilHit [] = []

squareUnderThreat :: ChessBoard -> PlayerColor -> Int -> Int -> Bool
squareUnderThreat board player x y =
  threatenedByBishopOrQueen
    || threatenedByRockOrQueen
    || threatenedByHorse
    || threatenedByPawn
    || threatenedByKing
  where
    opponentColor = if player == White then Black else White
    threatenedByHorse =
        any (\(x', y') -> inBounds x' y' && hasPieceOnSquare board x' y' (ChessPiece opponentColor Horse))
            [ (x + 1, y + 2),
              (x + 1, y - 2),
              (x - 1, y + 2),
              (x - 1, y - 2),
              (x + 2, y + 1),
              (x + 2, y - 1),
              (x - 2, y + 1),
              (x - 2, y - 1) ]

    threatenedOnRay :: [ChessPieceType] -> [(Int, Int)] -> Bool
    threatenedOnRay threateningTypes ray =
      let fold ((x', y') : rest) =
            (inBounds x' y' && (case pieceOnSquare board x' y' of
                Just (ChessPiece color' pieceType') ->
                  color' == opponentColor && elem pieceType' threateningTypes
                Nothing -> fold rest))
          fold [] = False
       in fold ray

    threatenedByBishopOrQueen = any (\ray -> threatenedOnRay [Queen, Bishop] ray) (emptyBoardBishopRays x y)
    threatenedByRockOrQueen = any (\ray -> threatenedOnRay [Queen, Rock] ray) (emptyBoardRockRays x y)

    threatenedByPawn =
      let y' = if player == White then y + 1 else y - 1
          pawnExists x' = inBounds x' y' && hasPieceOnSquare board x' y' (ChessPiece opponentColor Pawn)
       in pawnExists (x - 1) || pawnExists (x + 1)

    threatenedByKing =
      let (x', y') = playerKingPosition (pieces board) opponentColor
       in abs (x - x') <= 1 && abs (y - y') <= 1

playerPotentiallyPinned :: ChessBoard -> PlayerColor -> Bool
playerPotentiallyPinned board player =
  any (\ray -> checkRayPin ray False [Queen, Bishop]) (emptyBoardBishopRays x y) ||
  any (\ray -> checkRayPin ray False [Queen, Rock]) (emptyBoardRockRays x y)
  where
    opponentColor = if player == White then Black else White
    (x, y) = playerKingPosition (pieces board) player

    checkRayPin :: [(Int, Int)] -> Bool -> [ChessPieceType] -> Bool
    checkRayPin ((x, y) : rest) ownPieceSeen pinnerTypes =
        let ownPiece = isPlayerOnSquare board player x y
            opponentPiece = isPlayerOnSquare board opponentColor x y
        in if not ownPiece && not opponentPiece
           then checkRayPin rest ownPieceSeen pinnerTypes
           else if ownPieceSeen && opponentPiece && case pieceOnSquare board x y of
                                                        Just (ChessPiece color pieceType) -> color == opponentColor && elem pieceType pinnerTypes
                                                        _ -> False
           then True
           else if not ownPieceSeen && ownPiece
           then checkRayPin rest True pinnerTypes
           else False
    checkRayPin [] _ _ = False

{-# INLINE playerInCheck #-}
playerInCheck :: ChessBoard -> Bool
playerInCheck board = playerInCheck' board (turn board)

{-# INLINE playerInCheck' #-}
playerInCheck' :: ChessBoard -> PlayerColor -> Bool
playerInCheck' board player =
  let (x, y) = playerKingPosition (pieces board) player
   in squareUnderThreat board player x y

pawnCandidateMoves :: ChessBoard -> Int -> Int -> PlayerColor -> [Move]
pawnCandidateMoves board x y player =
  let (dir, inStartingPos, inEnPassantPos, promotesOnMove) =
        if player == White
          then (1, y == 2, y == 5, y == 7)
          else (-1, y == 7, y == 4, y == 2)
      y' = y + dir
      aheadIsClear =
        inBounds x (y + dir) && case pieceOnSquare board x y' of
          Just _ -> False
          Nothing -> True
      normalCaptures = do
        x' <- [x - 1, x + 1]
        x' <- ([x' | inBounds x' y'])
        x' <- case pieceOnSquare board x' y' of
          Just (ChessPiece color _) -> ([x' | color /= player])
          _ -> []
        if promotesOnMove
          then do
            createMove x y x' y' <$> [PromoHorse, PromoRock, PromoQueen, PromoBishop, PromoHorse]
          else return $ createMove x y x' y' NoPromo
      enPassantCaptures = do
        x' <- [x - 1, x + 1]
        x' <- ([x' | inBounds x' y'])
        x' <- ([x' | inEnPassantPos])
        x' <- case enPassant board of
          Just col -> ([x' | col == x'])
          _ -> []
        return $ createMove x y x' y' NoPromo
      doubleDipMove =
        let doubleAheadIsClear =
              inBounds x (y + 2 * dir) && case pieceOnSquare board x (y + 2 * dir) of
                Just _ -> False
                Nothing -> True
            canDoubleDip = inStartingPos && aheadIsClear && doubleAheadIsClear
         in ([createMove x y x (y + 2 * dir) NoPromo | canDoubleDip])
      singleMove
        | aheadIsClear && not promotesOnMove = [createMove x y x (y + dir) NoPromo]
        | aheadIsClear && promotesOnMove = map (\promotion -> createMove x y x (y + dir) promotion) [PromoHorse, PromoRock, PromoQueen, PromoBishop]
        | otherwise = []
   in normalCaptures ++ enPassantCaptures ++ doubleDipMove ++ singleMove

canCastleKingSide :: ChessBoard -> PlayerColor -> Bool
canCastleKingSide board color =
  let hasRights = if color == White then whiteKingCastle board else blackKingCastle board
      y = if color == White then 1 else 8
      hasEmptySpaces = case (pieceOnSquare board 6 y, pieceOnSquare board 7 y) of
        (Nothing, Nothing) -> True
        _ -> False
      travelsThroughCheck = case filter (\x' -> squareUnderThreat board color x' y) [5, 6, 7] of
        [] -> False
        _ -> True
   in hasRights && hasEmptySpaces && not travelsThroughCheck

canCastleQueenSide :: ChessBoard -> PlayerColor -> Bool
canCastleQueenSide board color =
  let hasRights = if color == White then whiteQueenCastle board else blackQueenCastle board
      y = if color == White then 1 else 8
      hasEmptySpaces = case (pieceOnSquare board 2 y, pieceOnSquare board 3 y, pieceOnSquare board 4 y) of
        (Nothing, Nothing, Nothing) -> True
        _ -> False
      travelsThroughCheck = case filter (\x' -> squareUnderThreat board color x' y) [3, 4, 5] of
        [] -> False
        _ -> True
   in hasRights && hasEmptySpaces && not travelsThroughCheck

kingCandidateMoves :: ChessBoard -> Int -> Int -> PlayerColor -> [Move]
kingCandidateMoves board x y player =
  let baseMoves =
        map (\(x', y') -> createMove x y x' y' NoPromo) $
          pieceThreats board (x, y, ChessPiece player King)
      castleKingSide = ([createMove x y 7 y NoPromo | canCastleKingSide board player])
      castleQueenSide = ([createMove x y 3 y NoPromo | canCastleQueenSide board player])
   in baseMoves ++ castleKingSide ++ castleQueenSide

pieceCandidateMoves :: ChessBoard -> (Int, Int, ChessPiece) -> [Move]
pieceCandidateMoves board (x, y, ChessPiece color Pawn) = pawnCandidateMoves board x y color
pieceCandidateMoves board (x, y, ChessPiece color King) = kingCandidateMoves board x y color
pieceCandidateMoves board piece@(x, y, _) = map
        (\(x', y') -> createMove x y x' y' NoPromo)
        (pieceThreats board piece)

-- candidate moves before handling invalid ones (eg., not resolving being in check)
-- ie., pseudo legal
pseudoLegalCandidateMoves :: ChessBoard -> [Move]
pseudoLegalCandidateMoves board = {-# SCC "m_pseudoLegalCandidateMoves" #-}
  let player = turn board
      playerPieces = playerPositionsToList (pieces board) player [Pawn, Bishop, Horse, Rock, Queen, King]
  in concatMap (\p -> pieceCandidateMoves board p) playerPieces

-- returns just if given candidate is legal, empty otherwise
-- (candidate can be illegal because pseudoLegalCandidateMoves returns pseudolegal moves)
candidateMoveLegal :: ChessBoard -> Move -> Maybe ChessBoard
candidateMoveLegal board candidate =
  let board' = applyMoveUnsafe board candidate
      inCheck = (wasInCheck || (wasPotentiallyPinned && movePotentiallyBreakingPin candidate) || isKingMove candidate) && playerInCheck' board' player
  in if not inCheck
     then return board'
     else Nothing
  where
    player = turn board
    wasInCheck = playerInCheck board
    wasPotentiallyPinned = playerPotentiallyPinned board player
    (king_x, king_y) = playerKingPosition (pieces board) player
    movePotentiallyBreakingPin move =
        fromRow move == king_y
            || fromCol move == king_x
            || abs (fromRow move - king_y) == abs (fromCol move - king_x)
    isKingMove move =
        fromRow move == king_y && fromCol move == king_x

applyMove :: ChessBoard -> Move -> Maybe ChessBoard
applyMove board move = do
  let candidates = pseudoLegalCandidateMoves board
  matchedCandidate <- find (\move' -> move' == move) candidates
  candidateMoveLegal board matchedCandidate

isCaptureMove :: ChessBoard -> Move -> Bool
isCaptureMove board move =
  case pieceOnSquare board (toCol move) (toRow move) of
        Just _ -> True
        _ -> False

-- returns parsed pieces + rest of input
loadFenPieces :: String -> (Int, Int) -> ChessBoardPositions -> Maybe (ChessBoardPositions, String)
loadFenPieces ('r' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece Black Rock)
loadFenPieces ('n' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece Black Horse)
loadFenPieces ('b' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece Black Bishop)
loadFenPieces ('k' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece Black King)
loadFenPieces ('q' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece Black Queen)
loadFenPieces ('p' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece Black Pawn)
loadFenPieces ('R' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece White Rock)
loadFenPieces ('N' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece White Horse)
loadFenPieces ('B' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece White Bishop)
loadFenPieces ('K' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece White King)
loadFenPieces ('Q' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece White Queen)
loadFenPieces ('P' : rest) (x, y) parsedPieces =
  loadFenPieces rest (x + 1, y) $ setPosition parsedPieces x y (ChessPiece White Pawn)
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
  let input = skipUntilWhitespace input
  let input = skipUntilWhitespace input
  let board =
        ChessBoard
          { turn = turn,
            pieces = pieces,
            enPassant = enPassant,
            whiteKingCastle = wk,
            blackKingCastle = bk,
            whiteQueenCastle = wq,
            blackQueenCastle = bq
          }
  return (board, input)

boardToFen :: ChessBoard -> String
boardToFen board =
    rankToStr 8 ++
    (concatMap (\i -> "/" ++ rankToStr (8 - i)) [1..7]) ++ 
    " " ++
    (if (turn board) == White then "w" else "b") ++ 
    " " ++
    castling ++
    " - 1 1" -- TODO fix missing en pessant

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

    rankToStr rank = concatMap (\file -> pieceToStr $ pieceOnSquare board file rank) [1..8]

    castling = 
        let str =
                (if (whiteKingCastle board) then "K" else "") ++
                (if (whiteQueenCastle board) then "Q" else "") ++
                (if (blackKingCastle board) then "k" else "") ++
                (if (blackQueenCastle board) then "q" else "")
        in if (length str) == 0 then "-" else str
