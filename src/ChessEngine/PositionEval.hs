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
import ChessEngine.EvaluatorData
import ChessEngine.HeuristicEvaluator
import Control.Monad
import Control.Monad.ST
import Data.List (partition, sortBy)
import Data.Ord

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
          (moveValue, newNodesParsed) <- do
            ((v, moves), nodes) <- evaluate' cache params'
            return ((negateEval v, moves), nodes)
          if (fst moveValue) > beta
            then -- passing up move still causes it to exceed beta -- cut off
              return ((beta, moves ++ [candidateMove]), newNodesParsed)
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
