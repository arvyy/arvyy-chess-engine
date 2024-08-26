{-# LANGUAGE ScopedTypeVariables #-}

module ChessEngine.HeuristicEvaluator
( finalDepthEval, finalDepthEvalExplained )
where

import ChessEngine.Board
import ChessEngine.EvaluatorData
import ChessEngine.Heatmaps
import Data.Foldable

evaluatePawns :: ChessCache -> ChessBoard -> IO Int
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
    doEvaluatePawns :: [(Int, Int, ChessPiece)] -> Int
    doEvaluatePawns ((x, y, ChessPiece color _) : rest) =
      let score =
            100
              + (if isPassedPawn x y color then 40 else 0)
              + (if isBackwardDoubledPawn x y color then (-30) else 0)
              + (if isProtectedPawn x y color then 50 else 0)
              + (floor $ 100 * piecePositionBonus x y (ChessPiece color Pawn))
              + 0
          multiplier = if color == White then 1 else -1
       in (score * multiplier) + doEvaluatePawns rest
    doEvaluatePawns [] = 0

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

finalDepthEval :: ChessCache -> ChessBoard -> IO PositionEval
finalDepthEval cache board = do
    (eval, _) <- finalDepthEval' (const ()) cache board
    return eval

finalDepthEvalExplained :: ChessBoard -> IO (PositionEval, [String])
finalDepthEvalExplained board = do
    cache <- create
    finalDepthEval' return cache board

-- returns current value as negamax (ie, score is multipled for -1 if current player is black)
finalDepthEval' :: Monoid m => (String -> m) -> ChessCache -> ChessBoard -> IO (PositionEval, m)
finalDepthEval' infoConsumer cache board = do
  let nonPawnScoreRaw = foldl' (\score piece -> score + scorePiece piece) 0 $ boardNonPawnPositions board
  let nonPawnScore = explain infoConsumer nonPawnScoreRaw "Non pawns"
  pawnScoreRaw <- (\value -> value * pieceMul White) <$> evaluatePawns cache board
  let pawnScore = explain infoConsumer pawnScoreRaw "Pawns"
  let myKingScoreRaw = scoreKingSafety board (turn board)
  let myKingScore = explain infoConsumer myKingScoreRaw "My king safety score"
  let opponentKingScoreRaw = scoreKingSafety board (otherPlayer (turn board)) * (-1)
  let opponentKingScore = explain infoConsumer opponentKingScoreRaw "Opponent king safety score"
  let (evalScore, explanation) = explain' infoConsumer (addScores [nonPawnScore, pawnScore, myKingScore, opponentKingScore]) "Total result"
  return (PositionEval evalScore, explanation)
  where
    pieceMul color = if color == turn board then 1 else -1

    addScore :: Monoid m => (Int, m) -> (Int, m) -> (Int, m)
    addScore (f1, m1) (f2, m2) = (f1 + f2, m1 <> m2)

    addScores :: Monoid m => [(Int, m)] -> (Int, m)
    addScores [a, b] = addScore a b
    addScores (x : rest) = addScore x (addScores rest)

    explain :: Monoid m => (String -> m) -> Int -> String -> (Int, m)
    explain infoConsumer score text = (score, infoConsumer (text ++ ": " ++ (show score)))

    explain' :: Monoid m => (String -> m) -> (Int, m) -> String -> (Int, m)
    explain' infoConsumer (score, explanation) text = (score, explanation <> infoConsumer (text ++ ": " ++ (show score)))

    scorePiece :: (Int, Int, ChessPiece) -> Int
    scorePiece piece@(_, _, ChessPiece player King) = (0 + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Queen) = (900 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Bishop) = (300 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Horse) = (300 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Rock) = (500 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece (_, _, ChessPiece _ Pawn) = 0 -- pawns are scored separately

scorePieceThreats :: ChessBoard -> (Int, Int, ChessPiece) -> Int
scorePieceThreats board piece =
  let isOwnSide y = case piece of
        (_, _, ChessPiece White _) -> y < 4
        _ -> y > 5
      (ownSide, opponentSide) =
        foldl'
          (\(own, opponent) (_, y) -> if isOwnSide y then (own + 1.0, opponent) else (own, opponent + 1.0))
          (0.0, 0.0)
          (pieceThreats board piece)
      (_, _, ChessPiece _ pieceType) = piece
      (multiplier, maxBonus) = case pieceType of
        -- due to queen range, it needs reduced reward otherwise bot is very eager to play with queen
        -- without developing other pieces
        -- TODO fix this by punishing evaluation for pieces being in starting position?
        Pawn -> (1, 100)
        Horse -> (1.5, 200)
        Bishop -> (1, 200)
        Rock -> (2, 300)
        Queen -> (0.5, 300)
        King -> (0.5, 100)
   in floor $ min (100 * log (1 + ownSide + (opponentSide * 1.5)) * multiplier) maxBonus

-- score from position tables only
scorePiecePosition :: ChessBoard -> (Int, Int, ChessPiece) -> Int
scorePiecePosition _ (x, y, piece@(ChessPiece _ pieceType)) =
  let squareRating = piecePositionBonus x y piece -- 0. - 1. rating, which needs to be first curved and then mapped onto range
      maxBonus = case pieceType of
        Pawn -> 0.4
        King -> 0.2
        Bishop -> 1
        Horse -> 1.5
        Rock -> 1.2
        Queen -> 0.0
        _ -> 0.0
      score = (squareRating ** 1.8) * maxBonus
   in floor $ score * 100

-- score relatively to given color
-- score most likely to be negative, ie, penalty for lacking safety
scoreKingSafety :: ChessBoard -> PlayerColor -> Int
scoreKingSafety board player = 
    floor $ fromIntegral (scorePawnShield + 0) * safetyMultiplier
    where
        (king_x, king_y) = playerKingPosition board player

        -- expect 3 pawns in front of king 2x3 rectangle; penalize by -1 for each missing pawn
        scorePawnShield :: Int
        scorePawnShield = 
            let 
              x1 = case king_x of
                      1 -> 1
                      8 -> 6
                      n -> n - 1
              x2 = x1 + 2
              y1 = case player of
                      White -> king_y + 1
                      Black -> king_y - 2
              y2 = y1 + 1
              pawnShield = countPawnsInArea x1 y1 x2 y2
            in ((min 3 pawnShield) - 3) * 100

        countPawnsInArea x1 y1 x2 y2 =
            let squares = do
                            x' <- [(max x1 1) .. (min x2 8)]
                            y' <- [(max y1 1) .. (min y2 8)]
                            return (x', y')
            in foldl' (\count (x, y) -> if hasPieceOnSquare board x y (ChessPiece player Pawn) 
                                        then count + 1 
                                        else count) 
                      0 squares

        --TODO
        --scoreOpenFile

        safetyMultiplier :: Float
        safetyMultiplier =
            let opponentMaterial = fromIntegral $ quickMaterialCount board (otherPlayer player)
                lowBound = 5.0
                highBound = 30.0
                range = highBound - lowBound
                adjustedMaterial = min highBound (max lowBound opponentMaterial)
            in (adjustedMaterial - lowBound) / range
