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
import Data.Maybe (fromJust)
import Data.Ord
import Debug.Trace

newtype PositionEval = PositionEval Float
  deriving (Eq, Show, Ord)

negateEval :: PositionEval -> PositionEval
negateEval (PositionEval v) = PositionEval (-v)

data TranspositionValue = TranspositionValue PositionEval Int deriving (Show)

type TranspositionTable s = Map.HashTable s ChessBoard TranspositionValue

type PawnTable s = Map.HashTable s Int64 Float

data ChessCache s = ChessCache (TranspositionTable s) (PawnTable s)

putValue :: ChessCache s -> ChessBoard -> Int -> PositionEval -> ST s ()
putValue (ChessCache table _) board depth value = do
  existingValue <- Map.lookup table board
  case existingValue of
    Just (TranspositionValue _ prevDepth) ->
      when (depth > prevDepth) $ Map.insert table board (TranspositionValue value depth)
    Nothing -> Map.insert table board (TranspositionValue value depth)

getValue :: ChessCache s -> ChessBoard -> ST s (Maybe TranspositionValue)
getValue (ChessCache table _) board = Map.lookup table board

putPawnEvaluation :: ChessCache s -> Int64 -> Float -> ST s ()
putPawnEvaluation (ChessCache _ pawns) pawnPosition value = Map.insert pawns pawnPosition value

getPawnEvaluation :: ChessCache s -> Int64 -> ST s (Maybe Float)
getPawnEvaluation (ChessCache _ pawns) position = Map.lookup pawns position

create :: ST s (ChessCache s)
create = do
  table <- Map.new
  pawns <- Map.new
  return (ChessCache table pawns)

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
              + (if isProtectedPawn x y color then 0.1 else 0)
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
      y' <- [y - 1 .. 2]
      case pieceOnSquare board x' y' of
        Just (ChessPiece White Pawn) -> [False]
        _ -> []

    isBackwardDoubledPawn x y White = not $ null $ do
      y' <- [y + 1 .. 7]
      case pieceOnSquare board x y' of
        Just (ChessPiece White Pawn) -> [False]
        _ -> []
    isBackwardDoubledPawn x y Black = not $ null $ do
      y' <- [y - 1 .. 2]
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
    allowCaching :: !Bool
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
evaluate' cache params@EvaluateParams {moves, board, depth = depth', nodesParsed, allowCaching} = do
  let depth = max depth' 0
  tableHit <- getValue cache board
  let doSearch = do
        sortedCandidates <- sortCandidates cache (candidateMoves board) (turn board)
        ((eval', moves'), nodes) <- evaluate'' cache params sortedCandidates
        when allowCaching $ putValue cache board depth eval'
        return ((eval', moves'), if allowCaching then nodes + 1 else nodes)
  case tableHit of
    Just (TranspositionValue eval cachedDepth) ->
      if cachedDepth >= depth
        then return ((eval, moves), nodesParsed)
        else doSearch
    Nothing -> doSearch

sortCandidates :: ChessCache s -> [(Move, ChessBoard)] -> PlayerColor -> ST s [(Move, ChessBoard)]
sortCandidates cache lst player = do
  augmentedList <- augmentWithScore cache lst
  let sortedCandidates = fmap removeScore (sortBy comparator augmentedList)
  return sortedCandidates
  where
    augmentWithScore :: ChessCache s -> [(Move, ChessBoard)] -> ST s [(Move, ChessBoard, Maybe PositionEval)]
    augmentWithScore cache ((move, board) : rest) = do
      cachedValue <- getValue cache board
      let positionValue = fmap (\(TranspositionValue eval depth) -> eval) cachedValue
      augmentedRest <- augmentWithScore cache rest
      return $ (move, board, positionValue) : augmentedRest
    augmentWithScore cache [] = return []

    evalComparator :: PositionEval -> PositionEval -> Ordering
    evalComparator =
      if player == White
        then (\a b -> compare (Down a) (Down b))
        else (\a b -> compare a b)
    positionComparator :: Maybe PositionEval -> Maybe PositionEval -> Ordering
    positionComparator (Just score1) (Just score2) = evalComparator score1 score2
    positionComparator (Just _) Nothing = LT
    positionComparator Nothing (Just _) = GT
    positionComparator Nothing Nothing = EQ
    comparator (_, _, value1) (_, _, value2) = positionComparator value1 value2
    removeScore (move, board, _) = (move, board)

horizonEval :: ChessCache s -> Int -> ChessBoard -> PositionEval -> PositionEval -> ST s PositionEval
horizonEval cache depth board alpha beta
  | depth <= 0 = finalDepthEval cache board
  | otherwise = do
      pat <- finalDepthEval cache board
      let alpha' = max alpha pat
      let capturingMoves = (filter (\(move, _) -> isCaptureMove board move) (candidateMoves board))
      if pat >= beta
        then return beta
        else foldHorizonEval cache depth capturingMoves alpha' beta

foldHorizonEval :: ChessCache s -> Int -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> ST s PositionEval
foldHorizonEval cache depth ((_, board') : rest) alpha beta = do
  value <- negateEval <$> horizonEval cache (depth - 1) board' (negateEval beta) (negateEval alpha)
  let alpha' = max alpha value
  if value >= beta
    then return beta
    else foldHorizonEval cache depth rest alpha' beta
foldHorizonEval _ _ [] alpha _ = return alpha

evaluate'' :: ChessCache s -> EvaluateParams -> [(Move, ChessBoard)] -> ST s ((PositionEval, [Move]), Int)
evaluate'' cache params@EvaluateParams {moves, firstChoice, alpha, beta, depth, maxDepth, board, nodesParsed, allowNullMove} candidates'
  | null candidates =
      let eval = outOfMovesEval board
       in return ((eval, moves), nodesParsed)
  | depth <= 0 = do
      eval <- horizonEval cache 10 board alpha beta
      return ((eval, moves), nodesParsed)
  | otherwise = foldCandidates cache candidates alpha beta
  where
    foldCandidates :: ChessCache s -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> ST s ((PositionEval, [Move]), Int)
    foldCandidates cache candidates alpha beta =
      foldCandidates' cache True (PositionEval $ (-1) / 0, []) candidates alpha beta 0 (False, False, False) nodesParsed

    foldCandidates' :: ChessCache s -> Bool -> (PositionEval, [Move]) -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> Int -> (Bool, Bool, Bool) -> Int -> ST s ((PositionEval, [Move]), Int)
    foldCandidates' cache first bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, nullWindowTried) nodesParsed
      | alpha >= beta = return (bestMoveValue, nodesParsed)
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
                    beta = negateEval alpha
                  }
          (moveValue, _) <- do
            ((v, moves), nodes) <- evaluate' cache params'
            return ((negateEval v, moves), nodes)
          if (fst moveValue) > beta
            then -- passing up move still causes it to exceed beta -- cut off
              return ((beta, moves ++ [candidateMove]), nodesParsed)
            else (foldCandidates' cache first bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (True, lmrTried, nullWindowTried) nodesParsed)

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
                    allowCaching = False
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
                    allowCaching = False
                  }
          evaluated' <- evaluate' cache params'
          let moveValue = case evaluated' of ((v, moves), _) -> (negateEval v, moves)
          if (fst moveValue) > (fst bestMoveValue)
            then -- found better with reduced window: do full search
              (foldCandidates' cache False bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, True) nodesParsed)
            else (foldCandidates' cache False bestMoveValue restCandidates alpha beta (siblingIndex + 1) (False, False, False) nodesParsed)
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
                    beta = negateEval alpha
                  }
          evaluated' <- evaluate' cache params'
          let (moveValue, newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, moves), nodes)
              newBestMoveValue =
                if (first || (fst moveValue) > (fst bestMoveValue))
                  then moveValue
                  else bestMoveValue
              newAlpha = max alpha (fst newBestMoveValue)
          foldCandidates' cache False newBestMoveValue restCandidates newAlpha beta (siblingIndex + 1) (False, False, False) newNodesParsed
    foldCandidates' cache _ bestMoveValue [] _ _ _ _ nodesParsed = return (bestMoveValue, nodesParsed)

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
            allowCaching = True
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
