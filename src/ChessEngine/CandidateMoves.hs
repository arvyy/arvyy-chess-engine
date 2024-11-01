module ChessEngine.CandidateMoves
   (  
   pseudoLegalCandidateMoves,
   candidateMoveLegal,
   applyMove,
   playerInCheck,
   pieceThreats
   )
where

import ChessEngine.Board
import ChessEngine.PrecomputedCandidateMoves
import Data.Foldable

-- given board and piece find threatened squares (for the purposes of check, castling, and eval for controlled squares)
-- ignores own pin status
{-# INLINE pieceThreats #-}
pieceThreats :: ChessBoard -> CoordWithPiece -> [Coord]
pieceThreats board (CoordWithPiece' (Coord' x y) (ChessPiece color King)) =
  let hops = emptyBoardKingHops x y
   in {-# SCC "m_pieceThreats_King" #-}
      filter
        (emptyOrOccupiedByOpponent board color)
        hops
pieceThreats board (CoordWithPiece' coord (ChessPiece color Queen)) = 
    {-# SCC "m_pieceThreats_Queen" #-} 
    (pieceThreats board (makeCoordWithPiece coord (ChessPiece color Rock)) ++ pieceThreats board (makeCoordWithPiece coord (ChessPiece color Bishop)))
pieceThreats board (CoordWithPiece' (Coord' x y) (ChessPiece color Pawn)) =
  {-# SCC "m_pieceThreats_Pawn" #-}
  let nextRow = if color == Black then y - 1 else y + 1
      candidates = [(makeCoord (x - 1) nextRow), (makeCoord (x + 1) nextRow)]
   in filter
        (emptyOrOccupiedByOpponent board color)
        candidates
pieceThreats board (CoordWithPiece' (Coord' x y) (ChessPiece color Bishop)) =
  {-# SCC "m_pieceThreats_Bishop" #-}
  let rays = emptyBoardBishopRays x y
   in concatMap (rayToValidMoves color board) rays
pieceThreats board (CoordWithPiece' (Coord' x y) (ChessPiece color Rock)) =
  {-# SCC "m_pieceThreats_Rock" #-}
  let rays = emptyBoardRockRays x y
   in concatMap (rayToValidMoves color board) rays
pieceThreats board (CoordWithPiece' (Coord' x y) (ChessPiece color Horse)) =
  {-# SCC "m_pieceThreats_Horse" #-}
  let hops = emptyBoardHorseHops x y
   in filter
        (emptyOrOccupiedByOpponent board color)
        hops

{-# INLINE rayToValidMoves #-}
rayToValidMoves :: PlayerColor -> ChessBoard -> [Coord] -> [Coord]
rayToValidMoves color board squares = filterUntilHit squares
  where
    filterUntilHit :: [Coord] -> [Coord]
    filterUntilHit positions = 
        case foldlM foldStep [] positions of
            Right moves -> moves
            Left moves -> moves

    foldStep :: [Coord] -> Coord -> Either [Coord] [Coord]
    foldStep moves (Coord' x y)
      | isPlayerOnSquare board color x y = Left moves
      | isPlayerOnSquare board (otherPlayer color) x y = Left $ (makeCoord x y):moves
      | otherwise = Right $ (makeCoord x y) : moves

{-# INLINE squareUnderThreat #-}
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
      any
        (\(Coord' x' y') -> hasPieceOnSquare board x' y' (ChessPiece opponentColor Horse))
        (emptyBoardHorseHops x y)

    threatenedOnRay :: [ChessPieceType] -> [Coord] -> Bool
    threatenedOnRay threateningTypes ray =
      case foldlM foldStep False ray of
        Right _ -> False
        Left t -> t

      where
        foldStep _ (Coord' x y)
            | Just (ChessPiece color' pieceType') <- pieceOnSquare board x y = Left $ color' == opponentColor && elem pieceType' threateningTypes
            | otherwise = Right False

    threatenedByBishopOrQueen = any (\ray -> threatenedOnRay [Queen, Bishop] ray) (emptyBoardBishopRays x y)
    threatenedByRockOrQueen = any (\ray -> threatenedOnRay [Queen, Rock] ray) (emptyBoardRockRays x y)

    threatenedByPawn =
      let y' = if player == White then y + 1 else y - 1
          pawnExists x' = inBounds x' y' && hasPieceOnSquare board x' y' (ChessPiece opponentColor Pawn)
       in pawnExists (x - 1) || pawnExists (x + 1)

    threatenedByKing =
      let (Coord' x' y') = playerKingPosition board opponentColor
       in abs (x - x') <= 1 && abs (y - y') <= 1

{-# INLINE playerPotentiallyPinned #-}
playerPotentiallyPinned :: ChessBoard -> PlayerColor -> Bool
playerPotentiallyPinned board player =
  any (\ray -> checkRayPin ray False [Queen, Bishop]) (emptyBoardBishopRays x y)
    || any (\ray -> checkRayPin ray False [Queen, Rock]) (emptyBoardRockRays x y)
  where
    opponentColor = if player == White then Black else White
    (Coord' x y) = playerKingPosition board player

    checkRayPin :: [Coord] -> Bool -> [ChessPieceType] -> Bool
    checkRayPin moves ownPieceSeen pinnerTypes =
        case foldlM foldStepper ownPieceSeen moves of
            Right _ -> False
            Left v -> v

        where
            foldStepper ownPieceSeen (Coord' x y) =
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
  let (Coord' x y) = playerKingPosition board player
   in squareUnderThreat board player x y

{-# INLINE pawnCandidateMoves #-}
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
            createMove x y x' y' <$> [PromoQueen, PromoRock, PromoBishop, PromoHorse]
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
        | aheadIsClear && promotesOnMove = map (\promotion -> createMove x y x (y + dir) promotion) [PromoQueen, PromoRock, PromoHorse, PromoBishop]
        | otherwise = []
   in normalCaptures ++ enPassantCaptures ++ doubleDipMove ++ singleMove

{-# INLINE canCastleKingSide #-}
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

{-# INLINE canCastleQueenSide #-}
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

{-# INLINE kingCandidateMoves #-}
kingCandidateMoves :: ChessBoard -> Int -> Int -> PlayerColor -> [Move]
kingCandidateMoves board x y player =
  let baseMoves =
        map (\(Coord' x' y') -> createMove x y x' y' NoPromo) $
          pieceThreats board (makeCoordWithPiece (makeCoord x y) (ChessPiece player King))
      castleKingSide = ([createMove x y 7 y NoPromo | canCastleKingSide board player])
      castleQueenSide = ([createMove x y 3 y NoPromo | canCastleQueenSide board player])
   in baseMoves ++ castleKingSide ++ castleQueenSide

{-# INLINE pieceCandidateMoves #-}
pieceCandidateMoves :: ChessBoard -> CoordWithPiece -> [Move]
pieceCandidateMoves board (CoordWithPiece' (Coord' x y) (ChessPiece color Pawn)) = pawnCandidateMoves board x y color
pieceCandidateMoves board (CoordWithPiece' (Coord' x y) (ChessPiece color King)) = kingCandidateMoves board x y color
pieceCandidateMoves board piece@(CoordWithPiece' (Coord' x y) _) =
  map
    (\(Coord' x' y') -> createMove x y x' y' NoPromo)
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
candidateMoveLegal :: ChessBoard -> Move -> Maybe ChessBoard
candidateMoveLegal board candidate =
  let board' = applyMoveUnsafe board candidate
      inCheck = (wasInCheck || (wasPotentiallyPinned && movePotentiallyBreakingPin) || isKingMove || isEnPassant) && playerInCheck' board' player
   in if not inCheck
        then return board'
        else Nothing
  where
    player = turn board
    wasInCheck = playerInCheck board
    wasPotentiallyPinned = playerPotentiallyPinned board player
    (Coord' king_x king_y) = playerKingPosition board player
    movePotentiallyBreakingPin =
      fromRow candidate == king_y
        || fromCol candidate == king_x
        || abs (fromRow candidate - king_y) == abs (fromCol candidate - king_x)
    isKingMove =
      fromRow candidate == king_y && fromCol candidate == king_x
    -- en pessent move might clear a pin held by opponent's pawn
    isEnPassant = case pieceOnSquare board (fromCol candidate) (fromRow candidate) of
      Just (ChessPiece _ Pawn) -> (fromCol candidate) /= (toCol candidate)
      _ -> False


applyMove :: ChessBoard -> Move -> Maybe ChessBoard
applyMove board move = do
  let candidates = pseudoLegalCandidateMoves board
  matchedCandidate <- find (\move' -> move' == move) candidates
  candidateMoveLegal board matchedCandidate
