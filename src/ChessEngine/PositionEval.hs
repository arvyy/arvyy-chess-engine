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
import Data.Maybe (isJust)

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
evaluate' cache params@EvaluateParams {moves, board, depth = depth', nodesParsed, allowCaching, alpha, beta} = do
  let depth = max depth' 0
  tableHit <- getValue cache board
  let doSearch = do
        sortedCandidates <- sortCandidates cache board alpha depth (candidateMoves board)
        let sortedCandidatesWithBoards = (\move -> (move, applyMoveUnsafe board move)) <$> sortedCandidates
        ((eval', moves'), nodes, bound) <- evaluate'' cache params sortedCandidatesWithBoards
        when allowCaching $ putValue cache board depth eval' bound
        return ((eval', moves'), if allowCaching then nodes + 1 else nodes)
  case tableHit of
    Just (TranspositionValue bound eval cachedDepth) ->
      if cachedDepth < depth
      then doSearch
      else if ((bound == Exact) ||
               (bound == LowerBound && eval > beta) ||
               (bound == UpperBound && eval < alpha))
      then return ((eval, moves), nodesParsed)
      else doSearch
    Nothing -> doSearch

sortCandidates :: ChessCache s -> ChessBoard -> PositionEval -> Int -> [Move] -> ST s [Move]
sortCandidates cache board alpha depth candidates =
    do
        -- (goodMovesFromCache, otherMoves) <- partitionAndSortCacheMoves cache alpha candidates
        let (goodCaptureMoves, badCaptureMoves, otherMoves') = partitionAndSortCaptureMoves candidates
        (killerMoves, otherMoves'') <- partitionKillerMoves cache depth otherMoves'
        return $ goodCaptureMoves ++ killerMoves ++ badCaptureMoves ++ otherMoves''
  where
    partitionAndSortCacheMoves :: ChessCache s -> PositionEval -> [Move] -> ST s ([Move], [Move])
    partitionAndSortCacheMoves cache alpha moves = do
        movesWithScores <- mapM (\m -> augmentWithScore cache m) moves
        let (movesFromCache, otherMoves) = partition (\(move, maybeEval) -> case maybeEval of 
                                                                                Just v -> v >= alpha
                                                                                _ -> False) 
                                                     movesWithScores
        let removeEval (move, _) = move
        return $ (removeEval <$> movesFromCache, removeEval <$> otherMoves)

    augmentWithScore :: ChessCache s -> Move -> ST s (Move, Maybe PositionEval)
    augmentWithScore cache move = do
      cachedValue <- getValue cache (applyMoveUnsafe board move)
      let positionValue = do
            (TranspositionValue bound eval _) <- cachedValue
            if (bound == UpperBound)
            then Nothing
            else return eval
      return (move, positionValue)

    partitionAndSortCaptureMoves :: [Move] -> ([Move], [Move], [Move])
    partitionAndSortCaptureMoves moves =
        let augmentedMoves = augmentWithCaptureInfo <$> moves
            (captureMoves, otherMoves) = partition (\(_, capture) -> isJust capture) augmentedMoves
            (goodCaptures, badCaptures) = partition (\(_, Just n) -> n >= 0) captureMoves
            removeCaptureInfo (move, _) = move
            captureMoveComparator (_, Just v1) (_, Just v2) = compare v1 v2
            goodCaptures' = removeCaptureInfo <$> (sortBy (flip captureMoveComparator) goodCaptures)
            badCaptures' = removeCaptureInfo <$> (sortBy (flip captureMoveComparator) badCaptures)
            otherMoves' = removeCaptureInfo <$> otherMoves
        in (goodCaptures', badCaptures', otherMoves')

    augmentWithCaptureInfo :: Move -> (Move, Maybe Int)
    augmentWithCaptureInfo move@Move { fromCol, fromRow, toCol, toRow } =
        let diff = do 
                (ChessPiece _ capturedType) <- pieceOnSquare board toCol toRow
                (ChessPiece _ capturingType) <- pieceOnSquare board fromCol fromRow
                return $ fromEnum capturedType - fromEnum capturingType
        in (move, diff)

    partitionKillerMoves :: ChessCache s -> Int -> [Move] -> ST s ([Move], [Move])
    partitionKillerMoves cache depth moves =
        -- TODO implement killer moves
        return ([], moves)
            

horizonEval :: ChessCache s -> Int -> ChessBoard -> PositionEval -> PositionEval -> ST s PositionEval
horizonEval cache depth board alpha beta
  | depth <= 0 = finalDepthEval cache board
  | otherwise = do
      pat <- finalDepthEval cache board
      let alpha' = max alpha pat
      let capturingMoves = (filter (\move -> examineMove pat move) (candidateMoves board))
      if pat >= beta
        then return beta
        else foldHorizonEval cache depth board capturingMoves alpha' beta
  where
    -- examine only captures
    -- apply "delta pruning", only consider captures
    -- where captured piece + buffer > alpha (buffer recommended to be 200 centipawns)
    deltaBuffer = 2.0
    examineMove :: PositionEval -> Move -> Bool
    examineMove pat Move { toCol, toRow } =
        case pieceOnSquare board toCol toRow of
            Just (ChessPiece _ Pawn) -> evalAdd pat (1 + deltaBuffer) > alpha
            Just (ChessPiece _ Bishop) -> evalAdd pat (3 + deltaBuffer) > alpha
            Just (ChessPiece _ Horse) -> evalAdd pat (3 + deltaBuffer) > alpha
            Just (ChessPiece _ Rock) -> evalAdd pat (5 + deltaBuffer) > alpha
            Just (ChessPiece _ Queen) -> evalAdd pat (9 + deltaBuffer) > alpha
            _ -> False


foldHorizonEval :: ChessCache s -> Int -> ChessBoard -> [Move] -> PositionEval -> PositionEval -> ST s PositionEval
foldHorizonEval cache depth board (move : rest) alpha beta = do
  value <- negateEval <$> horizonEval cache (depth - 1) (applyMoveUnsafe board move) (negateEval beta) (negateEval alpha)
  let alpha' = max alpha value
  if value >= beta
    then return beta
    else foldHorizonEval cache depth board rest alpha' beta
foldHorizonEval _ _ _ [] alpha _ = return alpha

evaluate'' :: ChessCache s -> EvaluateParams -> [(Move, ChessBoard)] -> ST s ((PositionEval, [Move]), Int, TableValueBound)
evaluate'' cache params@EvaluateParams {moves, firstChoice, alpha, beta, depth, maxDepth, board, nodesParsed, allowNullMove} candidates'
  | null candidates =
      let eval = outOfMovesEval board
       in return ((eval, moves), nodesParsed, Exact)
  | depth <= 0 = do
      eval <- horizonEval cache 10 board alpha beta
      return ((eval, moves), nodesParsed, Exact)
  | otherwise = foldCandidates cache candidates alpha beta
  where
    foldCandidates :: ChessCache s -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> ST s ((PositionEval, [Move]), Int, TableValueBound)
    foldCandidates cache candidates alpha beta =
      foldCandidates' cache True (PositionEval $ (-1) / 0, []) candidates alpha beta 0 (False, False, False) nodesParsed

    foldCandidates' :: ChessCache s -> Bool -> (PositionEval, [Move]) -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> Int -> (Bool, Bool, Bool) -> Int -> ST s ((PositionEval, [Move]), Int, TableValueBound)
    foldCandidates' cache first bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, nullWindowTried) nodesParsed
      | alpha >= beta = return (bestMoveValue, nodesParsed, LowerBound)
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
              return ((beta, moves ++ [candidateMove]), newNodesParsed, LowerBound)
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
                    allowCaching = False }
          evaluated' <- evaluate' cache params'
          let (moveValue, newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, moves), nodes)
          if (fst moveValue) > (fst bestMoveValue)
            then -- found better with reduced window: do full search
              (foldCandidates' cache False bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, True) newNodesParsed)
            else do 
                -- record found upper bound (? is this correct???)
                when (allowCaching params) $ putValue cache candidateBoard (depth - 1) (fst moveValue) UpperBound
                (foldCandidates' cache False bestMoveValue restCandidates alpha beta (siblingIndex + 1) (False, False, False) newNodesParsed)
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
    foldCandidates' cache _ bestMoveValue [] _ _ _ _ nodesParsed = return (bestMoveValue, nodesParsed, Exact)

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
