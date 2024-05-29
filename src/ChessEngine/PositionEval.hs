{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module ChessEngine.PositionEval
  ( PositionEval (..),
    evaluate,
    EvaluateResult (..),
  )
where

import ChessEngine.Board
import ChessEngine.Heatmaps
import Control.Monad
import Control.Monad.ST
import Data.Foldable
import qualified Data.HashTable.ST.Basic as Map
import Data.Int (Int64)
import Data.List (partition, sortBy)
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Ord
import Debug.Trace
import Control.Applicative

newtype PositionEval = PositionEval Float
  deriving (Eq, Show, Ord)

negateEval :: PositionEval -> PositionEval
negateEval (PositionEval v) = PositionEval (-v)

data TranspositionValue = TranspositionValue PositionEval Int deriving (Show)
type TranspositionTable s = Map.HashTable s ChessBoard TranspositionValue
type PawnTable s = Map.HashTable s Int64 Float
type KillerMoveTable s = Map.HashTable s Int [Move]

data ChessCache s = ChessCache (TranspositionTable s) (PawnTable s) (KillerMoveTable s)

putValue :: ChessCache s -> ChessBoard -> Int -> PositionEval -> ST s ()
putValue (ChessCache table _ _) board depth value = do
  existingValue <- Map.lookup table board
  case existingValue of
    Just (TranspositionValue _ prevDepth) ->
      when (depth > prevDepth) $ Map.insert table board (TranspositionValue value depth)
    Nothing -> Map.insert table board (TranspositionValue value depth)

getValue :: ChessCache s -> ChessBoard -> ST s (Maybe TranspositionValue)
getValue (ChessCache table _ _) board = Map.lookup table board

putPawnEvaluation :: ChessCache s -> Int64 -> Float -> ST s ()
putPawnEvaluation (ChessCache _ pawns _) pawnPosition value = Map.insert pawns pawnPosition value

getPawnEvaluation :: ChessCache s -> Int64 -> ST s (Maybe Float)
getPawnEvaluation (ChessCache _ pawns _) position = Map.lookup pawns position

putKillerMove :: ChessCache s -> Int -> Move -> ST s ()
putKillerMove (ChessCache _ _ killerMoves) depth move = 
    do 
        existing' <- Map.lookup killerMoves depth
        let new = case existing' of
                    Just lst -> take 2 (move : lst)
                    Nothing -> [move]
        Map.insert killerMoves depth new

isKillerMove :: ChessCache s -> Int -> Move -> ST s Bool
isKillerMove (ChessCache _ _ killerMoves) depth move =
    do
        existing' <- Map.lookup killerMoves depth
        let existing = fromMaybe [] existing'
        let result = elem move existing
        return result

create :: ST s (ChessCache s)
create = do
  table <- Map.new
  pawns <- Map.new
  killerMoves <- Map.new
  return (ChessCache table pawns killerMoves)

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
  let score = foldl' (\score piece -> score + scorePiece piece) 0 $ boardNonPawnPositions board
  pawnScore <- (\value -> value * pieceMul White) <$> evaluatePawns cache board
  return $ PositionEval (score + pawnScore)
  where
    pieceMul color = if color == (turn board) then 1 else -1

    scorePiece :: (Int, Int, ChessPiece) -> Float
    scorePiece piece@(_, _, ChessPiece player King) = (0 + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Queen) = (9 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Bishop) = (3 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Horse) = (3 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Rock) = (5 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player

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
            (_, _, ChessPiece _ Queen) -> 0.5
            _ -> 1.0
       in (log (ownSide + 1.0) * 0.1 + log (opponentSide + 1.0) * 0.11) * typeMultiplier

    scorePiecePosition :: ChessBoard -> (Int, Int, ChessPiece) -> Float
    scorePiecePosition board (x, y, piece@(ChessPiece _ pieceType)) =
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

-- end of the game
outOfMovesEval :: ChessBoard -> PositionEval
outOfMovesEval board =
  let player = (turn board)
      inCheck = playerInCheck board player
   in if (not inCheck)
        then PositionEval 0
        else PositionEval $ (-1) / 0

data EvaluateParams = EvaluateParams
  { moves :: ![Move],
    firstChoice :: ![Move],
    alpha :: !PositionEval,
    beta :: !PositionEval,
    depth :: !Int,
    maxDepth :: !Int,
    board :: !ChessBoard,
    nodesParsed :: !Int,
    currentBest :: !(PositionEval, [Move]),
    allowNullMove :: !Bool,
    allowCaching :: !Bool,
    isQuetMove :: !Bool
  }

data EvaluateResult = EvaluateResult
  { nodesParsed :: !Int,
    finished :: !Bool,
    evaluation :: !PositionEval,
    moves :: ![Move],
    continuation :: EvaluateResult
  }
  deriving (Show)

evaluate' :: ChessCache s -> EvaluateParams -> ST s ((PositionEval, [Move]), Int)
evaluate' cache params@EvaluateParams {moves, board, depth = depth', maxDepth, nodesParsed, allowCaching, isQuetMove} = do
  let depth = max depth' 0
  tableHit <- getValue cache board
  let doSearch = do
        sortedCandidates <- sortCandidates cache board depth (candidateMoves board)
        ((eval', moves'), nodes, failedHigh) <- evaluate'' cache params sortedCandidates
        when allowCaching $ putValue cache board depth eval'
        when ({-allowCaching && -}failedHigh && isQuetMove && (not $ null moves)) $ putKillerMove cache (depth + 1) (last moves)
        return ((eval', moves'), if allowCaching then nodes + 1 else nodes)
  case tableHit of
    Just (TranspositionValue eval cachedDepth) ->
      if cachedDepth >= depth
        then return ((eval, moves), nodesParsed)
        else doSearch
    Nothing -> doSearch

sortCandidates :: ChessCache s -> ChessBoard -> Int -> [(Move, ChessBoard)] -> ST s [(Move, ChessBoard)]
sortCandidates cache board depth candidates =
    do
        (movesFromCache, otherMoves) <- partitionAndSortCacheMoves cache candidates
        let (goodCaptureMoves, badCaptureMoves, otherMoves') = partitionAndSortCaptureMoves otherMoves
        (killerMoves, otherMoves'') <- partitionKillerMoves cache depth otherMoves'
        return $ movesFromCache ++ goodCaptureMoves ++ killerMoves ++ otherMoves'' ++ badCaptureMoves
  where
    partitionAndSortCacheMoves :: ChessCache s -> [(Move, ChessBoard)] -> ST s ([(Move, ChessBoard)], [(Move, ChessBoard)])
    partitionAndSortCacheMoves cache moves = do
        movesWithScores <- mapM (\m -> augmentWithScore cache m) moves
        let (movesFromCache, otherMoves) = partition (\(move, board, maybeEval) -> isJust maybeEval) movesWithScores
        let removeEval (move, board, _) = (move, board)
        return (removeEval <$> movesFromCache, removeEval <$> otherMoves)

    augmentWithScore :: ChessCache s -> (Move, ChessBoard) -> ST s (Move, ChessBoard, Maybe PositionEval)
    augmentWithScore cache (move, board') = do
      cachedValue <- getValue cache board'
      let positionValue = fmap (\(TranspositionValue eval depth) -> eval) cachedValue
      return (move, board', positionValue)

    partitionAndSortCaptureMoves :: [(Move, ChessBoard)] -> ([(Move, ChessBoard)], [(Move, ChessBoard)], [(Move, ChessBoard)])
    partitionAndSortCaptureMoves moves =
        let augmentedMoves = augmentWithCaptureInfo <$> moves
            (captureMoves, otherMoves) = partition (\(move, board, capture) -> isJust capture) augmentedMoves
            (goodCaptures, badCaptures) = partition (\(_, _, Just n) -> n >= 0) captureMoves
            removeCaptureInfo (move, board, eval) = (move, board)
            captureMoveComparator (_, _, Just v1) (_, _, Just v2) = compare v1 v2
            goodCaptures' = removeCaptureInfo <$> (sortBy (flip captureMoveComparator) goodCaptures)
            otherMoves' = removeCaptureInfo <$> otherMoves
            badCaptures' = removeCaptureInfo <$> badCaptures
        in (goodCaptures', badCaptures', otherMoves')

    augmentWithCaptureInfo :: (Move, ChessBoard) -> (Move, ChessBoard, Maybe Int)
    augmentWithCaptureInfo (move@Move { fromCol, fromRow, toCol, toRow }, board') =
        let diff = do 
                (ChessPiece _ capturedType) <- pieceOnSquare board toCol toRow
                (ChessPiece _ capturingType) <- pieceOnSquare board fromCol fromRow
                return $ fromEnum capturedType - fromEnum capturingType
        in (move, board', diff)

    partitionKillerMoves :: ChessCache s -> Int -> [(Move, ChessBoard)] -> ST s ([(Move, ChessBoard)], [(Move, ChessBoard)])
    partitionKillerMoves cache depth moves =
        do movesWithKillerFlag <- mapM (\(move, board) -> 
                                            do isKiller <- isKillerMove cache depth move
                                               return (move, board, isKiller))
                                            moves
           let (killerMoves, otherMoves) = partition (\(_, _, isKiller) -> isKiller) movesWithKillerFlag
           let removeKillerFlag (move, board, _) = (move, board)
           return (removeKillerFlag <$> killerMoves, removeKillerFlag <$> otherMoves)
            

horizonEval :: ChessCache s -> Int -> ChessBoard -> PositionEval -> PositionEval -> ST s PositionEval
horizonEval cache depth board alpha beta
  | depth <= 0 = finalDepthEval cache board
  | otherwise = do
      pat <- finalDepthEval cache board
      let alpha' = max alpha pat
      let capturingMoves = (filter (\(move, _) -> isCaptureMove board move) (candidateMoves board))
      if pat > beta
        then return pat
        else foldHorizonEval cache depth capturingMoves alpha' beta

foldHorizonEval :: ChessCache s -> Int -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> ST s PositionEval
foldHorizonEval cache depth ((_, board') : rest) alpha beta = do
  value <- negateEval <$> horizonEval cache (depth - 1) board' (negateEval beta) (negateEval alpha)
  let alpha' = max alpha value
  if value > beta
    then return value
    else foldHorizonEval cache depth rest alpha' beta
foldHorizonEval _ _ [] alpha _ = return alpha


-- position result, corresponding moves to final depth, node count, true if result was a fail-high
type EvaluateInternalResult s = ST s ((PositionEval, [Move]), Int, Bool)

evaluate'' :: ChessCache s -> EvaluateParams -> [(Move, ChessBoard)] -> EvaluateInternalResult s
evaluate'' cache params@EvaluateParams {moves, firstChoice, alpha, beta, depth, maxDepth, board, nodesParsed, allowNullMove} candidates'
  | null candidates =
      let eval = outOfMovesEval board
       in return ((eval, moves), nodesParsed, False)
  | depth <= 0 = do
      eval <- horizonEval cache 10 board alpha beta
      return ((eval, moves), nodesParsed, False)
  | otherwise = foldCandidates cache candidates alpha beta
  where
    foldCandidates :: ChessCache s -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> EvaluateInternalResult s
    foldCandidates cache candidates alpha beta =
      foldCandidates' cache True (PositionEval $ (-1) / 0, []) candidates alpha beta 0 (False, False, False) nodesParsed

    foldCandidates' :: ChessCache s -> Bool -> (PositionEval, [Move]) -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> Int -> (Bool, Bool, Bool) -> Int -> EvaluateInternalResult s
    foldCandidates' cache first bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, nullWindowTried) nodesParsed
      | alpha >= beta = return (bestMoveValue, nodesParsed, True)
      -- try null move if there is sufficient depth left & null move is allowed (ie., wasn't done on previous move)
      | not nullMoveTried && allowNullMove && depth > 3 = do
          let params' =
                params
                  { allowNullMove = False,
                    moves = [],
                    firstChoice = [],
                    depth = depth - 3, -- R = 2
                    board = candidateBoard {turn = if (turn board) == White then Black else White},
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha,
                    isQuetMove = not $ isCaptureMove board candidateMove
                  }
          (moveValue, newNodesParsed) <- do
            ((v, moves), nodes) <- evaluate' cache params'
            return ((negateEval v, moves), nodes)
          if (fst moveValue) > beta
            then -- passing up move still causes it to exceed beta -- cut off
              return ((beta, moves ++ [candidateMove]), newNodesParsed, True)
            else (foldCandidates' cache first bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (True, lmrTried, nullWindowTried) newNodesParsed)

      -- if this is 3rd+ candidate move under consideration in a depth of 3+ from start,
      -- evaluate with reduced depth (LMR).
      | not lmrTried && siblingIndex > 1 && (maxDepth - depth) > 1 && not (isCaptureMove board candidateMove) = do
          let newMovesPath = moves ++ [candidateMove]
              params' =
                params
                  { allowNullMove = True,
                    moves = newMovesPath,
                    firstChoice = (followUpFirstChoice candidateMove),
                    depth = (depth - 2), -- subtract 2 to lower depth search in fringe node
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha,
                    allowCaching = False,
                    isQuetMove = not $ isCaptureMove board candidateMove
                  }
          evaluated' <- evaluate' cache params'
          let (moveValue, newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, moves), nodes)
          if (fst moveValue) > (fst bestMoveValue)
            then -- if found better move, re-evaluate with proper depth
              (foldCandidates' cache False bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, True, nullWindowTried) newNodesParsed)
            else (foldCandidates' cache False bestMoveValue restCandidates alpha beta (siblingIndex + 1) (False, False, False) newNodesParsed)
      | (not nullWindowTried) && not first = do
          let nullBeta = case alpha of PositionEval v -> PositionEval (v + 0.0001)
              newMovesPath = moves ++ [candidateMove]
              params' =
                params
                  { allowNullMove = True,
                    moves = newMovesPath,
                    firstChoice = (followUpFirstChoice candidateMove),
                    depth = (depth - 1),
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval nullBeta,
                    beta = negateEval alpha,
                    allowCaching = False,
                    isQuetMove = not $ isCaptureMove board candidateMove
                  }
          evaluated' <- evaluate' cache params'
          let (moveValue, newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, moves), nodes)
          if (fst moveValue) > (fst bestMoveValue)
            then -- found better with reduced window: do full search
              (foldCandidates' cache False bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, True) newNodesParsed)
            else (foldCandidates' cache False bestMoveValue restCandidates alpha beta (siblingIndex + 1) (False, False, False) newNodesParsed)
      | otherwise = do
          let newMovesPath = moves ++ [candidateMove]
              params' =
                params
                  { allowNullMove = True,
                    moves = newMovesPath,
                    firstChoice = (followUpFirstChoice candidateMove),
                    depth = (depth - 1),
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha,
                    isQuetMove = not $ isCaptureMove board candidateMove
                  }
          evaluated' <- evaluate' cache params'
          let (moveValue, newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, moves), nodes)
              newBestMoveValue =
                if (first || (fst moveValue) > (fst bestMoveValue))
                  then moveValue
                  else bestMoveValue
              newAlpha = max alpha (fst newBestMoveValue)
          foldCandidates' cache False newBestMoveValue restCandidates newAlpha beta (siblingIndex + 1) (False, False, False) newNodesParsed
    foldCandidates' cache _ bestMoveValue [] alpha beta _ _ nodesParsed = return (bestMoveValue, nodesParsed, alpha > beta)

    candidates =
      let newCandidatesList = case firstChoice of
            [] -> candidates'
            (m : _) -> case partition (\c -> m == (fst c)) candidates' of
              ([], _) -> candidates'
              ((candidate : _), others) -> candidate : others
       in newCandidatesList

    followUpFirstChoice :: Move -> [Move]
    followUpFirstChoice move =
      case firstChoice of
        [] -> []
        (m : rest) -> if m == move then rest else []

evaluateIteration :: ChessCache s -> ChessBoard -> (PositionEval, [Move]) -> Int -> ST s ((PositionEval, [Move]), Int)
evaluateIteration cache board lastDepthBest depth =
  let (lastEval, firstChoice) = lastDepthBest
      params =
        EvaluateParams
          { moves = [],
            firstChoice = firstChoice,
            alpha = PositionEval $ (-1) / 0,
            beta = PositionEval $ 1 / 0,
            depth = depth,
            maxDepth = depth,
            board = board,
            nodesParsed = 0,
            currentBest = lastDepthBest,
            allowNullMove = True,
            allowCaching = True,
            isQuetMove = False
          }
   in do
        evaluate' cache params

iterateM' :: (a -> ST s a) -> ST s a -> Int -> ST s a
iterateM' f mx n = do
  if n == 0
    then mx
    else iterateM' f (mx >>= f) (n - 1)

evaluate :: ChessBoard -> Int -> EvaluateResult
evaluate board targetDepth =
  let (eval, moves, nodesParsed) = runST $ do
        (_, (eval, moves), _, nodesParsed) <- iterateM' computeNext firstEvaluation (targetDepth - startingDepth)
        return (eval, moves, nodesParsed)
      evalTransformation = if (turn board) == White then id else negateEval
      result =
        EvaluateResult
          { moves = moves,
            nodesParsed = nodesParsed,
            finished = True,
            evaluation = evalTransformation eval,
            continuation = result
          }
   in result
  where
    computeNext :: (Int, (PositionEval, [Move]), ChessCache s, Int) -> ST s (Int, (PositionEval, [Move]), ChessCache s, Int)
    computeNext current = do
      let (depth, lastDepthBest, cache, _) = current
      (thisDepthBest, nodesParsed) <- evaluateIteration cache board lastDepthBest (depth + 1)
      return (depth + 1, thisDepthBest, cache, nodesParsed)
    startingDepth = 1
    firstEvaluation :: ST s (Int, (PositionEval, [Move]), ChessCache s, Int)
    firstEvaluation = do
      cache <- create
      computeNext ((startingDepth - 1), (PositionEval 0, []), cache, 0)
