{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module ChessEngine.HeuristicEvaluator (finalDepthEval, finalDepthEvalExplained) where

import ChessEngine.Board
import ChessEngine.EvaluatorData
import ChessEngine.Heatmaps
import Data.Foldable
import Data.List (nubBy, nub)
import qualified Control.Foldl as Foldl
import Data.List.Fusion.Probe (fuseThis)
import GHC.List (build)

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
  -- add special target move -1 -1 as a marker for piece itself, so that piece doesn't get lost in case it has no candidate moves
  let piecesWithThreats = (boardNonPawnPositions board) >>= (\piece -> (\(x, y) -> (piece, x, y)) <$> (-1, -1) : pieceThreats board piece)
  let (nonPawnScoreRaw, myKingScoreRaw, opponentKingScoreRaw) = Foldl.fold (joinFold3
        (scorePiecesFold (turn board) board) 
        (scoreKingSafetyFold board (turn board)) 
        ((* (-1)) <$> scoreKingSafetyFold board (otherPlayer (turn board))))
        piecesWithThreats
  -- let nonPawnScoreRaw = foldl' (\score piece -> score + scorePiece piece) 0 $ piecesWithThreats
  let nonPawnScore = explain infoConsumer nonPawnScoreRaw "Non pawns"
  pawnScoreRaw <- (\value -> value * pieceMul White) <$> evaluatePawns cache board
  let pawnScore = explain infoConsumer pawnScoreRaw "Pawns"
  -- let myKingScoreRaw = scoreKingSafety board (turn board) piecesWithThreats
  let myKingScore = explain infoConsumer myKingScoreRaw "My king safety score"
  -- let opponentKingScoreRaw = scoreKingSafety board (otherPlayer (turn board)) piecesWithThreats * (-1)
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

joinFold3 :: Foldl.Fold a r -> Foldl.Fold a r -> Foldl.Fold a r -> Foldl.Fold a (r, r, r)
joinFold3 a b c =
    let p1 = (,) <$> a <*> b
        p2 = (\(v1, v2) v3 -> (v1, v2, v3)) <$> p1 <*> c
    in p2

{-# INLINE foldDistinct #-}
foldDistinct :: Eq e => (s -> e -> s) -> s -> Foldl.Fold e s
foldDistinct folder init = 
    Foldl.Fold (\(prev, state) el -> 
                      let !newState = case prev of
                                      Just el' | el' == el -> state
                                      _ -> folder state el
                      in (Just el, newState)) 
                (Nothing, init)
                (\(_, state) -> state)


scorePiecesFold :: PlayerColor -> ChessBoard -> Foldl.Fold ((Int, Int, ChessPiece), Int, Int) Int
scorePiecesFold player board = (+) <$> (scorePiecesBaseValueFold player board) <*> (scorePiecesThreatsFold player board)

scorePiecesThreatsFold :: PlayerColor -> ChessBoard -> Foldl.Fold ((Int, Int, ChessPiece), Int, Int) Int
scorePiecesThreatsFold player board = Foldl.Fold (\score el -> score + scorePieceThreat el) 0 id
    where
      scorePieceThreat ((_, _, ChessPiece color pieceType), (-1), (-1)) = 0 -- skip fake marker
      scorePieceThreat ((_, _, ChessPiece color pieceType), x', y') =
          let isOwnSide y = case color of
                White -> y < 5
                _ -> y > 4
            in (4 + (if isOwnSide y' then 0 else 1) + (if x' > 1 && x' < 8 then 1 else 0)) * pieceMul color
      pieceMul color = if color == player then 1 else -1

scorePiecesBaseValueFold :: PlayerColor -> ChessBoard -> Foldl.Fold ((Int, Int, ChessPiece), Int, Int) Int
scorePiecesBaseValueFold player board = 
    --Foldl.premap (\(piece, _, _) -> piece) $ foldDistinct (\score piece -> score + scorePiece piece) 0
    Foldl.Fold (\score (piece, x, y) -> if x == (-1) && y == (-1) then score + scorePiece piece else score) 0 id
    where
      scorePiece :: (Int, Int, ChessPiece) -> Int
      scorePiece (piece@(_, _, ChessPiece player King)) = (0 + scorePiecePosition board piece) * pieceMul player
      scorePiece (piece@(_, _, ChessPiece player Queen)) = (900 + scorePiecePosition board piece) * pieceMul player
      scorePiece (piece@(_, _, ChessPiece player Bishop)) = (300 + scorePiecePosition board piece + scoreTrappedBishop board piece) * pieceMul player
      scorePiece (piece@(_, _, ChessPiece player Horse)) = (300 + scorePiecePosition board piece + scoreTrappedHorse board piece) * pieceMul player
      scorePiece (piece@(_, _, ChessPiece player Rock)) = (500 + scorePiecePosition board piece + scoreRockOnFile board piece) * pieceMul player
      scorePiece _ = 0 -- pawns are scored separately

      pieceMul color = if color == player then 1 else -1
      

{-
scorePieceThreats :: PlayerColor -> [(Int, Int)] -> Int
scorePieceThreats player threats =
  let isOwnSide y = case player of
        White -> y < 5
        _ -> y > 4
      mobilityScore =
        foldl'
          (\score (x, y) -> score + 4 + (if isOwnSide y then 0 else 1) + (if x > 1 && x < 8 then 1 else 0))
          0
          threats
    in mobilityScore
-}

-- score from position tables only
scorePiecePosition :: ChessBoard -> (Int, Int, ChessPiece) -> Int
scorePiecePosition _ (x, y, piece@(ChessPiece _ pieceType)) =
  let squareRating = piecePositionBonus x y piece -- 0. - 1. rating, which needs to be first curved and then mapped onto range
      maxBonus = case pieceType of
        King -> 50
        Bishop -> 20
        Horse -> 20
        Rock -> 30
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
scoreKingSafetyFold :: ChessBoard -> PlayerColor -> Foldl.Fold ((Int, Int, ChessPiece), Int, Int) Int
scoreKingSafetyFold board player =
  {-
  (\scoreSafetyBoxThreat -> 
      (floor $ (fromIntegral (scorePawnShield + scoreSurroundingOpenFiles)) * safetyMultiplier)
        + scoreSafetyBoxThreat 
        + scoreKingOnEdgeInEndgame
        + scoreKingCloseToOpponentKingInWinningEndGame
  ) <$> scoreSafetyBoxThreatFold
  -}
  pure $ 
      (floor $ (fromIntegral (scorePawnShield + scoreSurroundingOpenFiles)) * safetyMultiplier)
        + scoreKingOnEdgeInEndgame
        + scoreKingCloseToOpponentKingInWinningEndGame
  where
    opponentColor = otherPlayer player
    opponentMaterial = fromIntegral $ quickMaterialCount board opponentColor
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
      where
        scoreOpenFile  x =
            let state = fileState board x (otherPlayer player)
            in case state of
                    OpenFile -> (-100)
                    _ -> 0

    scoreSafetyBoxThreatFold :: Foldl.Fold ((Int, Int, ChessPiece), Int, Int) Int
    scoreSafetyBoxThreatFold =
        {-# SCC "m_scoreSafetyBoxThreat" #-}
        {-
        let squares = [(x, y) | x <- [king_x - 1 .. king_x + 1], y <- [king_y - 1 .. king_y + 1], inBounds x y && (x /= king_x || y /= king_y)]
            threateners = concatMap (\(x, y) -> squareThreatenedBy board player x y) squares
            threatenersCount = length $ nub threateners
            threatScores = (\(_, _, ChessPiece _ pieceType) -> scoreThreatener pieceType) <$> threateners
        in ((foldl' (+) 0 threatScores) * countMultiplier threatenersCount) `div` 100
        -}

        {-
        let opponentThreats = filter (\((_, _, ChessPiece color _), _) -> color == opponentColor) piecesWithThreats
            boxThreats = map (\((_, _, ChessPiece _ pieceType), threats) -> 
                                        let boxThreats = filter squareInSafetyBox threats
                                        in (pieceType, length boxThreats)) 
                                   opponentThreats
            threatenersCount = length boxThreats
            threatScores = map (\(pieceType, count) -> scoreThreatener pieceType * count) boxThreats
        in ((foldl' (+) 0 threatScores) * countMultiplier threatenersCount) `div` 100
        -}
        Foldl.prefilter relevantThreat $ finalScore <$> scoreThreatenerFold <*> countThreatenrsFold
      where

        countThreatenrsFold :: Foldl.Fold ((Int, Int, ChessPiece), Int, Int) Int
        countThreatenrsFold =
            {-
            let countDistinct :: Foldl.Fold (Int, Int, ChessPiece) Int
                countDistinct = foldDistinct (\count _ -> count + 1) 0
            in Foldl.premap (\(piece, _, _) -> piece) countDistinct
            -}
            Foldl.Fold (\count (_, x, y) -> if x == (-1) && y == (-1) then count + 1 else count) 0 id

        scoreThreatenerFold :: Foldl.Fold ((Int, Int, ChessPiece), Int, Int) Int
        scoreThreatenerFold = Foldl.Fold (\score ((_, _, (ChessPiece _ pieceType)), _, _) -> score + scoreThreatener pieceType) 0 id
        
        finalScore :: Int -> Int -> Int
        finalScore baseThreatScore attackerCount =
            (baseThreatScore * countMultiplier attackerCount) `div` 100

        relevantThreat :: ((Int, Int, ChessPiece), Int, Int) -> Bool
        relevantThreat ((_, _, ChessPiece color _), x', y') = color /= player && squareInSafetyBox x' y'

        squareInSafetyBox :: Int -> Int -> Bool
        squareInSafetyBox x y = (abs (x - king_x) <= 1) && (abs (y - king_y) <= 1)
        
        scoreThreatener Horse = (-20)
        scoreThreatener Bishop = (-20)
        scoreThreatener Rock = (-40)
        scoreThreatener Queen = (-80)
        scoreThreatener _ = 0

        countMultiplier count =
            case count of
                1 -> 0
                2 -> 50
                3 -> 75
                4 -> 88
                5 -> 94
                6 -> 97
                _ -> 99


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
