module ChessEngine.Board
  ( ChessPieceType (..),
    PlayerColor (..),
    ChessPiece (..),
    Move (..),
    ChessBoard (..),
    ChessBoardPositions (..),
    boardPositions,
    pieceOnSquare,
    candidateMoves,
    initialBoard,
    applyMove,
    playerInCheck,
    parseMove,
    moveToString,
    loadFen,
  )
where

import Data.Bits
import Data.Char
import Data.Int (Int64)
import Data.Maybe
import GHC.Stack

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
  deriving (Eq, Ord)

instance Show ChessBoardPositions where
  show positions = show $ positionsToList positions

positionsToList :: ChessBoardPositions -> [(Int, Int, ChessPiece)]
positionsToList positions = do
  x <- [1 .. 8]
  y <- [1 .. 8]
  piece <- case (pieceOnSquare' positions x y) of
    Nothing -> []
    Just p -> [p]
  return (x, y, piece)

coordsToBitIndex :: Int -> Int -> Int
coordsToBitIndex x y =
  let value = (y - 1) * 8 + x - 1
   in if value > 63 then error ("Coordinate out of bounds: " ++ (show (x, y))) else value

clearPosition :: ChessBoardPositions -> Int -> Int -> ChessBoardPositions
clearPosition (ChessBoardPositions black white bishops horses queens kings pawns rocks) x y =
  ChessBoardPositions
    { black = clearBit black bit,
      white = clearBit white bit,
      bishops = clearBit bishops bit,
      horses = clearBit horses bit,
      queens = clearBit queens bit,
      kings = clearBit kings bit,
      pawns = clearBit pawns bit,
      rocks = clearBit rocks bit
    }
  where
    bit = coordsToBitIndex x y

setPosition :: ChessBoardPositions -> Int -> Int -> ChessPiece -> ChessBoardPositions
setPosition positions x y (ChessPiece color pieceType) =
  let (ChessBoardPositions black white bishops horses queens kings pawns rocks) = clearPosition positions x y
   in ChessBoardPositions
        { black = if color == Black then setBit black bit else black,
          white = if color == White then setBit white bit else white,
          bishops = if pieceType == Bishop then setBit bishops bit else bishops,
          horses = if pieceType == Horse then setBit horses bit else horses,
          queens = if pieceType == Queen then setBit queens bit else queens,
          kings = if pieceType == King then setBit kings bit else kings,
          pawns = if pieceType == Pawn then setBit pawns bit else pawns,
          rocks = if pieceType == Rock then setBit rocks bit else rocks
        }
  where
    bit = coordsToBitIndex x y

pieceOnSquare' :: ChessBoardPositions -> Int -> Int -> Maybe ChessPiece
pieceOnSquare' (ChessBoardPositions black white bishops horses queens kings pawns rocks) x y = do
  color' <- color
  pieceType' <- pieceType
  return $ ChessPiece color' pieceType'
  where
    bit = coordsToBitIndex x y
    color
      | testBit black bit = Just Black
      | testBit white bit = Just White
      | otherwise = Nothing
    pieceType
      | testBit bishops bit = Just Bishop
      | testBit horses bit = Just Horse
      | testBit queens bit = Just Queen
      | testBit kings bit = Just King
      | testBit pawns bit = Just Pawn
      | testBit rocks bit = Just Rock
      | otherwise = Nothing

data ChessPieceType = Pawn | Horse | Rock | Queen | King | Bishop deriving (Show, Eq)

data PromoChessPieceType = PromoHorse | PromoRock | PromoQueen | PromoBishop deriving (Show, Eq)

data PlayerColor = Black | White deriving (Eq, Show, Ord)

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
  deriving (Eq, Show, Ord)

data Move = Move
  { fromCol :: !Int,
    fromRow :: !Int,
    toCol :: !Int,
    toRow :: !Int,
    promotion :: !(Maybe PromoChessPieceType)
  }
  deriving (Show, Eq)

boardPositions :: ChessBoard -> [(Int, Int, ChessPiece)]
boardPositions ChessBoard {pieces = pieces} = positionsToList pieces

parseSquareReference :: String -> Maybe (Int, Int)
parseSquareReference (x : y : []) = do
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
  y' <- if (isDigit y) then Just (digitToInt y) else Nothing
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
    [] -> Just Nothing
    ['n'] -> Just (Just PromoHorse)
    ['b'] -> Just (Just PromoBishop)
    ['q'] -> Just (Just PromoQueen)
    ['r'] -> Just (Just PromoRock)
    _ -> Nothing
  return $ Move (fst fromSquare) (snd fromSquare) (fst toSquare) (snd toSquare) promotion
parseMove _ = Nothing

moveToString :: Move -> Maybe String
moveToString (Move fromx fromy tox toy promo) = do
  from <- squareReferenceToString (fromx, fromy)
  to <- squareReferenceToString (tox, toy)
  let promoStr = case promo of
        Nothing -> ""
        Just PromoHorse -> "n"
        Just PromoRock -> "r"
        Just PromoQueen -> "q"
        Just PromoBishop -> "b"
  return (from ++ to ++ promoStr)

initialBoard :: ChessBoard
initialBoard =
  let (board, _) = fromJust $ loadFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
   in board

otherPlayer :: PlayerColor -> PlayerColor
otherPlayer Black = White
otherPlayer White = Black

pieceOnSquare :: ChessBoard -> Int -> Int -> Maybe ChessPiece
pieceOnSquare board x y = pieceOnSquare' (pieces board) x y

-- applies move blindly without validation for checks or piece movement rules
-- partial function if reference position is empty
applyMoveUnsafe :: ChessBoard -> Move -> ChessBoard
applyMoveUnsafe board (Move x y x' y' promotion) =
  let ChessPiece player pieceType = case pieceOnSquare board x y of
        Nothing -> error "Unsafe move tried to move unexisting piece"
        Just f -> f
      isEnPassantMove = pieceType == Pawn && (enPassant board) == Just x' && (if player == White then (y == 5) else (y == 4))
      isKingCastleMove = pieceType == King && x == 5 && x' == 7
      isQueenCastleMove = pieceType == King && x == 5 && x' == 3
      isCastleMove = isKingCastleMove || isQueenCastleMove
      oldPieces = (pieces board)
      newPieces
        | isEnPassantMove = applyEnPassant oldPieces player
        | isKingCastleMove = applyKingCastle oldPieces player
        | isQueenCastleMove = applyQueenCastle oldPieces player
        | otherwise = applyNormalMove oldPieces player pieceType
      whiteKingCastle' =
        not (isCastleMove && player == White)
          && (whiteKingCastle board)
          && case (player, pieceType) of
            (White, King) -> False
            (White, Rock) -> if (x == 8 && y == 1) then False else True
            _ -> True
      whiteQueenCastle' =
        (not (isCastleMove && player == White))
          && (whiteQueenCastle board)
          && case (player, pieceType) of
            (White, King) -> False
            (White, Rock) -> if (x == 1 && y == 1) then False else True
            _ -> True
      blackKingCastle' =
        (not (isCastleMove && player == Black))
          && (blackKingCastle board)
          && case (player, pieceType) of
            (Black, King) -> False
            (Black, Rock) -> if (x == 8 && y == 8) then False else True
            _ -> True
      blackQueenCastle' =
        (not (isCastleMove && player == Black))
          && (blackQueenCastle board)
          && case (player, pieceType) of
            (Black, King) -> False
            (Black, Rock) -> if (x == 1 && y == 8) then False else True
            _ -> True
   in board
        { pieces = newPieces,
          turn = otherPlayer (turn board),
          whiteKingCastle = whiteKingCastle',
          blackKingCastle = blackKingCastle',
          whiteQueenCastle = whiteQueenCastle',
          blackQueenCastle = blackQueenCastle'
        }
  where
    matchesPosition :: Int -> Int -> (Int, Int, ChessPiece) -> Bool
    matchesPosition x y (x', y', _) = x == x' && y == y'

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
            Just PromoHorse -> (ChessPiece color Horse)
            Just PromoRock -> (ChessPiece color Rock)
            Just PromoQueen -> (ChessPiece color Queen)
            Just PromoBishop -> (ChessPiece color Bishop)
            Nothing -> (ChessPiece color pieceType)
          p2 = setPosition p1 x' y' newMovedPiece
       in p2

inBounds :: Int -> Int -> Bool
inBounds x y = x >= 1 && x <= 8 && y >= 1 && y <= 8

emptyOrOccupiedByOpponent :: ChessBoard -> PlayerColor -> (Int, Int) -> Bool
emptyOrOccupiedByOpponent ChessBoard {pieces = ChessBoardPositions {white = white}} White (x, y) =
  let bit = coordsToBitIndex x y
   in inBounds x y && not (testBit white bit)
emptyOrOccupiedByOpponent ChessBoard {pieces = ChessBoardPositions {black = black}} Black (x, y) =
  let bit = coordsToBitIndex x y
   in inBounds x y && not (testBit black bit)

-- given board and piece find threatened squares (for the purposes of check, castling, and eval for controlled squares)
-- ignores own pin status
pieceThreats :: ChessBoard -> (Int, Int, ChessPiece) -> [(Int, Int)]
pieceThreats board (x, y, ChessPiece color King) =
  let candidates = filter ((/= (x, y))) [(x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1]]
   in filter
        (emptyOrOccupiedByOpponent board color)
        candidates
pieceThreats board (x, y, ChessPiece color Queen) = pieceThreats board (x, y, ChessPiece color Rock) ++ pieceThreats board (x, y, ChessPiece color Bishop)
pieceThreats board (x, y, ChessPiece color Pawn) =
  let nextRow = if color == Black then y - 1 else y + 1
      candidates = [(x - 1, nextRow), (x + 1, nextRow)]
   in filter
        (emptyOrOccupiedByOpponent board color)
        candidates
pieceThreats board (x, y, ChessPiece color Bishop) =
  let directions = [(-1, -1), (1, -1), (-1, 1), (1, 1)]
      rays = map (pieceThreatsRay color board (x, y)) directions
   in concat rays
pieceThreats board (x, y, ChessPiece color Rock) =
  let directions = [(0, -1), (0, 1), (-1, 0), (1, 0)]
      rays = map (pieceThreatsRay color board (x, y)) directions
   in concat rays
pieceThreats board (x, y, ChessPiece color Horse) =
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
   in filter
        (emptyOrOccupiedByOpponent board color)
        candidates

pieceThreatsRay :: PlayerColor -> ChessBoard -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
pieceThreatsRay color board (x, y) (dx, dy) =
  let squares =
        ( filter (\square -> inBounds (fst square) (snd square))
            . map (\i -> (x + i * dx, y + i * dy))
        )
          [1 .. 7]
      squareAndPieceList :: [((Int, Int), Maybe ChessPiece)]
      squareAndPieceList = map (\square -> (square, pieceOnSquare board (fst square) (snd square))) squares
   in (filterUntilHit squareAndPieceList)
  where
    filterUntilHit :: [((Int, Int), Maybe ChessPiece)] -> [(Int, Int)]
    filterUntilHit (((x, y), Nothing) : rest) = [(x, y)] ++ (filterUntilHit rest)
    filterUntilHit (((x, y), Just (ChessPiece pieceColor _)) : rest) = if pieceColor == color then [] else [(x, y)]
    filterUntilHit [] = []

squareUnderThreat :: ChessBoard -> PlayerColor -> Int -> Int -> Bool
squareUnderThreat board player x y =
  let opponentPieces =
        filter
          ( \p -> case p of
              (_, _, ChessPiece pieceColor _) -> player /= pieceColor
          )
          $ positionsToList (pieces board) -- TODO find directly without intermediate list
      opponentThreats = concat (map (\p -> pieceThreats board p) opponentPieces)
      matchingThreats = filter ((==) (x, y)) opponentThreats
   in not (null matchingThreats)

playerInCheck :: ChessBoard -> PlayerColor -> Bool
playerInCheck board player =
  let kings =
        filter
          ( \p -> case p of
              (_, _, ChessPiece piecePlayer King) -> piecePlayer == player
              _ -> False
          )
          $ positionsToList (pieces board) -- TODO find directly without intermediate list
      king = case kings of
        [k] -> k
        [] -> error $ "Missing king on the board " ++ (show board)
        (k : rest) -> error $ "Multiple kings on the board for " ++ (show board)
   in case king of
        (x, y, _) -> squareUnderThreat board player x y

pawnCandidateMoves :: ChessBoard -> Int -> Int -> PlayerColor -> [Move]
pawnCandidateMoves board x y player =
  let (dir, inStartingPos, inEnPassantPos, promotesOnMove) =
        if player == White
          then (1, y == 2, y == 5, y == 7)
          else (-1, y == 7, y == 4, y == 2)
      y' = y + dir
      aheadIsClear =
        inBounds x (y + 2 * dir) && case pieceOnSquare board x y' of
          Just _ -> False
          Nothing -> True
      normalCaptures = do
        x' <- [x - 1, x + 1]
        x' <- if not (inBounds x' y') then [] else [x']
        x' <- case pieceOnSquare board x' y' of
          Just (ChessPiece color _) -> if color /= player then [x'] else []
          _ -> []
        return (Move x y x' y' Nothing)
      enPassantCaptures = do
        x' <- [x - 1, x + 1]
        x' <- if not (inBounds x' y') then [] else [x']
        x' <- if inEnPassantPos then [x'] else []
        x' <- case (enPassant board) of
          Just col -> if col == x' then [x'] else []
          _ -> []
        return (Move x y x' y' Nothing)
      doubleDipMove =
        let doubleAheadIsClear =
              inBounds x (y + 2 * dir) && case pieceOnSquare board x (y + 2 * dir) of
                Just _ -> False
                Nothing -> True
            canDoubleDip = inStartingPos && aheadIsClear && doubleAheadIsClear
         in if canDoubleDip then [Move x y x (y + 2 * dir) Nothing] else []
      singleMove =
        if aheadIsClear && not promotesOnMove
          then [Move x y x (y + dir) Nothing]
          else
            if aheadIsClear && promotesOnMove
              then map (\promotion -> Move x y x (y + dir) (Just promotion)) [PromoHorse, PromoRock, PromoQueen, PromoBishop]
              else []
   in normalCaptures ++ enPassantCaptures ++ doubleDipMove ++ singleMove

canCastleKingSide :: ChessBoard -> PlayerColor -> Bool
canCastleKingSide board color =
  let hasRights = if color == White then (whiteKingCastle board) else (blackKingCastle board)
      y = if color == White then 1 else 8
      hasEmptySpaces = case (pieceOnSquare board 6 y, pieceOnSquare board 7 y) of
        (Nothing, Nothing) -> True
        _ -> False
      travelsThroughCheck = case filter (\x' -> squareUnderThreat board color x' y) [5, 6, 7] of
        [] -> False
        _ -> True
   in hasRights && hasEmptySpaces && (not travelsThroughCheck)

canCastleQueenSide :: ChessBoard -> PlayerColor -> Bool
canCastleQueenSide board color =
  let hasRights = if color == White then (whiteQueenCastle board) else (blackQueenCastle board)
      y = if color == White then 1 else 8
      hasEmptySpaces = case (pieceOnSquare board 2 y, pieceOnSquare board 3 y, pieceOnSquare board 4 y) of
        (Nothing, Nothing, Nothing) -> True
        _ -> False
      travelsThroughCheck = case filter (\x' -> squareUnderThreat board color x' y) [3, 4, 5] of
        [] -> False
        _ -> True
   in hasRights && hasEmptySpaces && (not travelsThroughCheck)

kingCandidateMoves :: ChessBoard -> Int -> Int -> PlayerColor -> [Move]
kingCandidateMoves board x y player =
  let baseMoves =
        map (\pos -> Move x y (fst pos) (snd pos) Nothing) $
          pieceThreats board (x, y, ChessPiece player King)
      castleKingSide = if canCastleKingSide board player then [Move x y 7 y Nothing] else []
      castleQueenSide = if canCastleQueenSide board player then [Move x y 3 y Nothing] else []
   in baseMoves ++ castleKingSide ++ castleQueenSide

pieceCandidateMoves :: ChessBoard -> (Int, Int, ChessPiece) -> [Move]
pieceCandidateMoves board (x, y, ChessPiece color Pawn) = pawnCandidateMoves board x y color
pieceCandidateMoves board (x, y, ChessPiece color King) = kingCandidateMoves board x y color
pieceCandidateMoves board piece =
  let x = case piece of (x, _, _) -> x
      y = case piece of (_, y, _) -> y
   in map
        ( \p -> case p of
            (x', y') -> Move x y x' y' Nothing
        )
        (pieceThreats board piece)

-- candidate moves before handling invalid ones (eg., not resolving being in check)
candidateMoves' :: ChessBoard -> [Move]
candidateMoves' board =
  let player = (turn board)
      playerPieces =
        filter
          ( \p -> case p of
              (_, _, ChessPiece pieceColor _) -> pieceColor == player
          )
          $ positionsToList (pieces board) -- TODO find directly without intermediate list
      piecesCandidates = map (pieceCandidateMoves board) playerPieces
   in concat piecesCandidates

candidateMoves :: ChessBoard -> [(Move, ChessBoard)]
candidateMoves board =
  let player = (turn board)
      candidates = candidateMoves' board
      validCandidates = do
        candidate <- candidates
        let board' = applyMoveUnsafe board candidate
        let inCheck = playerInCheck board' player
        if not inCheck then [(candidate, board')] else []
   in validCandidates

applyMove :: ChessBoard -> Move -> Maybe ChessBoard
applyMove board move =
  let candidates = candidateMoves board
      matches = filter (\c -> (fst c) == move) candidates
   in case matches of
        [] -> Nothing
        ((move, board') : _) -> Just board'

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
loadFenPieces ('/' : rest) (x, y) parsedPieces =
  loadFenPieces rest (1, y - 1) parsedPieces
loadFenPieces (' ' : rest) (x, y) parsedPieces =
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
loadCastlingRights ('-' : ' ' : rest) rights = Just ((False, False, False, False), rest)
loadCastlingRights ('K' : rest) (wk, wq, bk, bq) = loadCastlingRights rest (True, wq, bk, bq)
loadCastlingRights ('Q' : rest) (wk, wq, bk, bq) = loadCastlingRights rest (wk, True, bk, bq)
loadCastlingRights ('k' : rest) (wk, wq, bk, bq) = loadCastlingRights rest (wk, wq, True, bq)
loadCastlingRights ('q' : rest) (wk, wq, bk, bq) = loadCastlingRights rest (wk, wq, bk, True)
loadCastlingRights (' ' : rest) rights = Just (rights, rest)
loadCastlingRights _ _ = Nothing

loadEnPassant :: String -> Maybe ((Maybe Int), String)
loadEnPassant ('-' : ' ' : rest) = Just (Nothing, rest)
loadEnPassant (x : y : ' ' : rest) = do
  (x', _) <- parseSquareReference [x, y]
  return $ (Just x', rest)
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
