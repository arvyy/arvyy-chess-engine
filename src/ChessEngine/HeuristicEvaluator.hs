module ChessEngine.HeuristicEvaluator
( finalDepthEval )
where

import Control.Monad.ST
import ChessEngine.Board
import ChessEngine.EvaluatorData
import ChessEngine.Heatmaps
import Data.Foldable

evaluatePawns :: ChessCache s -> ChessBoard -> ST s Float
evaluatePawns cache board = do
  let key = pawns $ pieces board
  pawnEvaluation <- getPawnEvaluation cache key
  case pawnEvaluation of
    Just v -> return v
    Nothing -> do
      let pawnEvaluation' = doEvaluatePawns $ boardPawnPositions board
      putPawnEvaluation cache key pawnEvaluation'
      return pawnEvaluation'
  where
    doEvaluatePawns :: [(Int, Int, ChessPiece)] -> Float
    doEvaluatePawns ((x, y, ChessPiece color _) : rest) =
      let score =
            1
              + (if isPassedPawn x y color then 0.4 else 0)
              + (if isBackwardDoubledPawn x y color then (-0.3) else 0)
              + (if isProtectedPawn x y color then 0.05 else 0)
              + (piecePositionBonus x y (ChessPiece color Pawn) * 0.2)
              + 0
          multiplier = if color == White then 1 else -1
       in (score * multiplier) + doEvaluatePawns rest
    doEvaluatePawns [] = 0.0

    isPassedPawn x y White = null $ do
      x' <- [x - 1 .. x + 1]
      y' <- [y + 1 .. 7]
      case pieceOnSquare board x' y' of
        Just (ChessPiece Black Pawn) -> [False]
        _ -> []
    isPassedPawn x y Black = null $ do
      x' <- [x - 1 .. x + 1]
      y' <- [2 .. y - 1]
      case pieceOnSquare board x' y' of
        Just (ChessPiece White Pawn) -> [False]
        _ -> []

    isBackwardDoubledPawn x y White = not $ null $ do
      y' <- [y + 1 .. 7]
      case pieceOnSquare board x y' of
        Just (ChessPiece White Pawn) -> [False]
        _ -> []
    isBackwardDoubledPawn x y Black = not $ null $ do
      y' <- [2 .. y - 1]
      case pieceOnSquare board x y' of
        Just (ChessPiece Black Pawn) -> [False]
        _ -> []

    isProtectedPawn x y White = not $ null $ do
      x' <- [x - 1, x + 1]
      let y' = y - 1
      case pieceOnSquare board x' y' of
        Just (ChessPiece White Pawn) -> [False]
        _ -> []
    isProtectedPawn x y Black = not $ null $ do
      x' <- [x - 1, x + 1]
      let y' = y + 1
      case pieceOnSquare board x' y' of
        Just (ChessPiece Black Pawn) -> [False]
        _ -> []

-- returns current value as negamax (ie, score is multipled for -1 if current player is black)
finalDepthEval :: ChessCache s -> ChessBoard -> ST s PositionEval
finalDepthEval cache board = do
  let nonPawnScore = foldl' (\score piece -> score + scorePiece piece) 0 $ boardNonPawnPositions board
  pawnScore <- (\value -> value * pieceMul White) <$> evaluatePawns cache board
  return $ PositionEval (nonPawnScore + pawnScore)
  where
    pieceMul color = if color == turn board then 1 else -1

    scorePiece :: (Int, Int, ChessPiece) -> Float
    scorePiece piece@(_, _, ChessPiece player King) = (0 + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Queen) = (9 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Bishop) = (3 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Horse) = (3 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Rock) = (5 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece (_, _, ChessPiece _ Pawn) = 0 -- pawns are scored separately

scorePieceThreats :: ChessBoard -> (Int, Int, ChessPiece) -> Float
scorePieceThreats board piece =
  let isOwnSide y = case piece of
        (_, _, ChessPiece White _) -> y < 5
        _ -> y > 4
      (ownSide, opponentSide) =
        foldl'
          (\(own, opponent) (_, y) -> if isOwnSide y then (own + 1.0, opponent) else (own, opponent + 1.0))
          (0.0, 0.0)
          (pieceThreats board piece)
      typeMultiplier = case piece of
        -- due to queen range, it needs reduced reward otherwise bot is very eager to play with queen
        -- without developing other pieces
        -- TODO fix this by punishing evaluation for pieces being in starting position?
        (_, _, ChessPiece _ Queen) -> 0.5
        _ -> 1.0
   in (log (ownSide + 1.0) * 0.1 + log (opponentSide + 1.0) * 0.11) * typeMultiplier

scorePiecePosition :: ChessBoard -> (Int, Int, ChessPiece) -> Float
scorePiecePosition _ (x, y, piece@(ChessPiece _ pieceType)) =
  let squareRating = piecePositionBonus x y piece -- 0. - 1. rating, which needs to be first curved and then mapped onto range
      maxBonus = case pieceType of
        Pawn -> 0.2
        King -> 1
        Bishop -> 0.5
        Horse -> 0.5
        Rock -> 0.2
        Queen -> 0.0
      score = (squareRating ** 1.8) * maxBonus
   in score
