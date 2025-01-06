{-# LANGUAGE ScopedTypeVariables #-}

module ChessEngine.HeuristicEvaluator (finalDepthEval, finalDepthEvalExplained) where

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
    
    doEvaluatePawns = foldl' (\score pawn -> score + doEvaluatePawn pawn) 0

    doEvaluatePawn (x, y, ChessPiece color _) =
      let untilPromotion = if color == White then 8 - y else y - 1
          passedPawnBonus = (8 - untilPromotion) * 4
          score =
            100
              + (if isPassedPawn board x y color then passedPawnBonus else 0)
              + (if isBackwardPawn board x y color then (-20) else 0)
              + (if isProtectedPawn x y color then 15 else 0)
              + (floor $ 100 * piecePositionBonus x y (ChessPiece color Pawn))
              + 0
          multiplier = if color == White then 1 else -1
       in (score * multiplier)

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
finalDepthEval' :: (Monoid m) => (String -> m) -> ChessCache -> ChessBoard -> IO (PositionEval, m)
finalDepthEval' infoConsumer cache board = do
  let nonPawnScoreRaw = foldl' (\score piece -> score + scorePiece piece) 0 $ boardNonPawnPositions board
  let nonPawnScore = explain infoConsumer nonPawnScoreRaw "Non pawns"
  pawnScoreRaw <- (\value -> value * pieceMul White) <$> evaluatePawns cache board
  let pawnScore = explain infoConsumer pawnScoreRaw "Pawns"
  let myKingScoreRaw = scoreKingSafety board (turn board)
  let myKingScore = explain infoConsumer myKingScoreRaw "My king safety score"
  let opponentKingScoreRaw = scoreKingSafety board (otherPlayer (turn board)) * (-1)
  let opponentKingScore = explain infoConsumer opponentKingScoreRaw "Opponent king safety score"
  let myConnectedRockScore = explain infoConsumer (scoreConnectedRocks board (turn board)) "My connected rock bonus"
  let opponentConnectedRockScore = explain infoConsumer ((-1) * scoreConnectedRocks board (otherPlayer (turn board))) "Opponent connected rock bonus"
  let (evalScore, explanation) = explain' infoConsumer (addScores [nonPawnScore, pawnScore, myKingScore, opponentKingScore, myConnectedRockScore, opponentConnectedRockScore]) "Total result"
  return (PositionEval evalScore, explanation)
  where
    pieceMul color = if color == turn board then 1 else -1

    addScore :: (Monoid m) => (Int, m) -> (Int, m) -> (Int, m)
    addScore (f1, m1) (f2, m2) = (f1 + f2, m1 <> m2)

    addScores :: (Monoid m) => [(Int, m)] -> (Int, m)
    addScores scores =
      foldl' addScore (0, mempty) scores

    explain :: (Monoid m) => (String -> m) -> Int -> String -> (Int, m)
    explain infoConsumer score text = (score, infoConsumer (text ++ ": " ++ (show score)))

    explain' :: (Monoid m) => (String -> m) -> (Int, m) -> String -> (Int, m)
    explain' infoConsumer (score, explanation) text = (score, explanation <> infoConsumer (text ++ ": " ++ (show score)))

    scorePiece :: (Int, Int, ChessPiece) -> Int
    scorePiece piece@(_, _, ChessPiece player King) = (0 + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Queen) = (900 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Bishop) = (300 + scorePieceThreats board piece + scorePiecePosition board piece + scoreTrappedBishop board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Horse) = (300 + scorePieceThreats board piece + scorePiecePosition board piece + scoreTrappedHorse board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Rock) = (500 + scorePieceThreats board piece + scorePiecePosition board piece + scoreRockOnFile board piece) * pieceMul player
    scorePiece (_, _, ChessPiece _ Pawn) = 0 -- pawns are scored separately

scorePieceThreats :: ChessBoard -> (Int, Int, ChessPiece) -> Int
scorePieceThreats board piece =
  let isOwnSide y = case piece of
        (_, _, ChessPiece White _) -> y < 4
        _ -> y > 5
      mobilityScore =
        foldl'
          (\score (x, y) -> if isOwnSide y then score + 10 else score + 13)
          0
          (pieceThreats board piece)
      (_, _, ChessPiece _ pieceType) = piece
      (maxBonus1, maxMobilityScore1, maxBonus2, maxMobilityScore2) = case pieceType of
        -- due to queen range, it needs reduced reward otherwise bot is very eager to play with queen
        -- without developing other pieces
        -- TODO fix this by punishing evaluation for pieces being in starting position?
        Horse -> (30, 30, 10, 80)
        Bishop -> (30, 40, 10, 100)
        Rock -> (40, 50, 20, 100)
        Queen -> (5, 40, 10, 150)
        _ -> (0, 0, 0, 0)
   in (maxBonus1 * (min mobilityScore maxMobilityScore1)) `div` maxMobilityScore1
        + (maxBonus2 * (min mobilityScore maxMobilityScore2)) `div` maxMobilityScore2

-- score from position tables only
scorePiecePosition :: ChessBoard -> (Int, Int, ChessPiece) -> Int
scorePiecePosition _ (x, y, piece@(ChessPiece _ pieceType)) =
  let squareRating = piecePositionBonus x y piece -- 0. - 1. rating, which needs to be first curved and then mapped onto range
      maxBonus = case pieceType of
        King -> 10
        Bishop -> 30
        Horse -> 30
        Rock -> 40
        _ -> 0
      score = squareRating * maxBonus
   in floor score

-- penalize stupid horse being stuck at the opponent's edge and controlled by pawns
scoreTrappedHorse :: ChessBoard -> (Int, Int, ChessPiece) -> Int
scoreTrappedHorse board (8, 8, ChessPiece White Horse)
    | hasPieceOnSquare board 6 7 (ChessPiece Black Pawn) || hasPieceOnSquare board 8 7 (ChessPiece Black Pawn) = -150
    | otherwise = 0
scoreTrappedHorse board (1, 8, ChessPiece White Horse)
    | hasPieceOnSquare board 3 7 (ChessPiece Black Pawn) || hasPieceOnSquare board 1 7 (ChessPiece Black Pawn) = -150
    | otherwise = 0
scoreTrappedHorse board (8, 1, ChessPiece Black Horse)
    | hasPieceOnSquare board 6 2 (ChessPiece White Pawn) || hasPieceOnSquare board 8 2 (ChessPiece White Pawn) = -150
    | otherwise = 0
scoreTrappedHorse board (1, 1, ChessPiece Black Horse)
    | hasPieceOnSquare board 3 2 (ChessPiece White Pawn) || hasPieceOnSquare board 1 2 (ChessPiece White Pawn) = -150
    | otherwise = 0
scoreTrappedHorse board (8, 7, ChessPiece White Horse)
    | hasPieceOnSquare board 8 6 (ChessPiece Black Pawn) && hasPieceOnSquare board 7 7 (ChessPiece Black Pawn) = -150
    | hasPieceOnSquare board 6 6 (ChessPiece Black Pawn) && hasPieceOnSquare board 7 7 (ChessPiece Black Pawn) = -150
    | otherwise = 0
scoreTrappedHorse board (1, 7, ChessPiece White Horse)
    | hasPieceOnSquare board 1 6 (ChessPiece Black Pawn) && hasPieceOnSquare board 2 7 (ChessPiece Black Pawn) = -150
    | hasPieceOnSquare board 3 6 (ChessPiece Black Pawn) && hasPieceOnSquare board 2 7 (ChessPiece Black Pawn) = -150
    | otherwise = 0
scoreTrappedHorse board (8, 2, ChessPiece Black Horse)
    | hasPieceOnSquare board 8 3 (ChessPiece White Pawn) && hasPieceOnSquare board 7 2 (ChessPiece White Pawn) = -150
    | hasPieceOnSquare board 6 3 (ChessPiece White Pawn) && hasPieceOnSquare board 7 2 (ChessPiece White Pawn) = -150
    | otherwise = 0
scoreTrappedHorse board (1, 2, ChessPiece Black Horse)
    | hasPieceOnSquare board 1 3 (ChessPiece White Pawn) && hasPieceOnSquare board 2 2 (ChessPiece White Pawn) = -150
    | hasPieceOnSquare board 3 3 (ChessPiece White Pawn) && hasPieceOnSquare board 2 2 (ChessPiece White Pawn) = -150
    | otherwise = 0
scoreTrappedHorse _ _ = 0

-- penalize stupid bishop being stuck at the opponent's edge and controlled by pawns
scoreTrappedBishop :: ChessBoard -> (Int, Int, ChessPiece) -> Int
scoreTrappedBishop board (8, 7, ChessPiece White Bishop)
    | hasPieceOnSquare board 6 7 (ChessPiece Black Pawn) && hasPieceOnSquare board 7 6 (ChessPiece Black Pawn) = -150
    | otherwise = 0
scoreTrappedBishop board (1, 7, ChessPiece White Bishop)
    | hasPieceOnSquare board 3 7 (ChessPiece Black Pawn) && hasPieceOnSquare board 2 6 (ChessPiece Black Pawn) = -150
    | otherwise = 0
scoreTrappedBishop board (8, 2, ChessPiece Black Bishop)
    | hasPieceOnSquare board 6 2 (ChessPiece White Pawn) && hasPieceOnSquare board 7 3 (ChessPiece White Pawn) = -150
    | otherwise = 0
scoreTrappedBishop board (1, 2, ChessPiece Black Bishop)
    | hasPieceOnSquare board 3 2 (ChessPiece White Pawn) && hasPieceOnSquare board 2 3 (ChessPiece White Pawn) = -150
    | otherwise = 0
scoreTrappedBishop _ _ = 0

scoreRockOnFile :: ChessBoard -> (Int, Int, ChessPiece) -> Int
scoreRockOnFile board (x, _, ChessPiece color Rock) =
    case fileState board x color of
        ClosedFile -> 0
        SemiOpenFile -> 5
        OpenFile -> 10
scoreRockOnFile _ _ = 0

scoreConnectedRocks :: ChessBoard -> PlayerColor -> Int
scoreConnectedRocks board color =
    let rocks = findPiecePositions board (ChessPiece color Rock)
    in case rocks of
        [(x1, y1), (x2, y2)] 
            | (x1 == x2 && connectedVertically x1 y1 y2)
            || (y1 == y2 && connectedHorizontally y1 x1 x2) -> 10
        _ -> 0
    where
        connectedVertically x y1 y2 =
            let y1' = (min y1 y2) + 1
                y2' = (max y1 y2) - 1
                points = (\y -> (x, y)) <$> [y1'..y2']
            in rangeEmpty points
        connectedHorizontally y x1 x2 =
            let x1' = (min x1 x2) + 1
                x2' = (max x1 x2) - 1
                points = (\x -> (x, y)) <$> [x1'..x2']
            in rangeEmpty points
        rangeEmpty points = all (\(x, y) -> squareEmpty board x y) points

-- score relatively to given color
-- score most likely to be negative, ie, penalty for lacking safety
scoreKingSafety :: ChessBoard -> PlayerColor -> Int
scoreKingSafety board player =
  (floor $ (fromIntegral (scorePawnShield + scoreSurroundingOpenFiles)) * safetyMultiplier)
    + scoreKingOnEdgeInEndgame
    + scoreKingCloseToOpponentKingInWinningEndGame
  where
    opponentMaterial = fromIntegral $ quickMaterialCount board (otherPlayer player)
    myMaterial = fromIntegral $ quickMaterialCount board player

    (king_x, king_y) = playerKingPosition board player
    (opponentKingX, opponentKingY) = playerKingPosition board (otherPlayer player)

    -- expect 3 pawns in front of king 2x3 rectangle; penalize by -100 for each missing pawn
    scorePawnShield :: Int
    scorePawnShield =
      let pawnShield = countPawnShield board king_x king_y player
       in ((min 3 pawnShield) - 3) * 80

    -- penalize if file is open or semi-open for opponent
    scoreSurroundingOpenFiles :: Int
    scoreSurroundingOpenFiles =
        let v1 = if (king_x > 1) then scoreOpenFile 1 else 0
            v2 = if (king_x < 8) then scoreOpenFile 8 else 0
            v3 = scoreOpenFile king_x
        in v1 + v2 + v3

    scoreOpenFile :: Int -> Int
    scoreOpenFile  x =
        let state = fileState board x (otherPlayer player)
        in case state of
                OpenFile -> (-100)
                _ -> 0


    -- when this side is down to king, score it worse the closer it is to edge
    -- so that winning side knows to push it towards the edge and not blunder 50-move draw
    scoreKingOnEdgeInEndgame :: Int
    scoreKingOnEdgeInEndgame =
      let losingEndgame = myMaterial == 0
          distanceToEdgeX = (min king_x (9 - king_x)) - 1
          distanceToEdgeY = (min king_y (9 - king_y)) - 1
          distanceToEdge = min distanceToEdgeX distanceToEdgeY
          penalty = (3 - distanceToEdge) * (-100)
       in if losingEndgame then penalty else 0

    -- when this side has material and other side only king
    -- score it better for beign near opponent king (to prevent 50-move draw of K+R vs K where king is required for mate)
    scoreKingCloseToOpponentKingInWinningEndGame :: Int
    scoreKingCloseToOpponentKingInWinningEndGame =
      let opponentHasPawns = quickPawnCount board (otherPlayer player)  > 0
          distance = max (abs $ king_x - opponentKingX) (abs $ king_y - opponentKingY)
          bonus = 100 - (distance * (-10))
       in if myMaterial > 0 && opponentMaterial == 0 && (not opponentHasPawns)
            then bonus
            else 0

    safetyMultiplier :: Float
    safetyMultiplier =
      let lowBound = 5.0
          highBound = 30.0
          range = highBound - lowBound
          adjustedMaterial = min highBound (max lowBound opponentMaterial)
       in (adjustedMaterial - lowBound) / range
