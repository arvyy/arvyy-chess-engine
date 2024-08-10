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
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Debug.Trace

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
    alpha :: !PositionEval,
    beta :: !PositionEval,
    depth :: !Int,
    maxDepth :: !Int,
    board :: !ChessBoard,
    nodesParsed :: !Int,
    currentBest :: !(PositionEval, [Move]),
    allowNullMove :: !Bool }

data EvaluateResult = EvaluateResult
  { nodesParsed :: !Int,
    finished :: !Bool,
    evaluation :: !PositionEval,
    moves :: ![Move],
    continuation :: EvaluateResult }
  deriving (Show)

evaluate' :: ChessCache s -> EvaluateParams -> ST s ((PositionEval, [Move]), Int)
evaluate' cache params@EvaluateParams {moves, board, depth = depth', nodesParsed, alpha, beta } = do
  let depth = max depth' 0
  tableHit <- getValue cache board
  let doSearch = do
        sortedCandidates <- sortCandidates cache board alpha depth (pseudoLegalCandidateMoves board)
        let sortedCandidatesWithBoards = mapMaybe (\move -> (\board' -> (move, board')) <$> candidateMoveLegal board move) sortedCandidates
        ((eval', moves', maybeBestMove), nodes, bound) <- evaluate'' cache params sortedCandidatesWithBoards
        case maybeBestMove of
            Just bestMove -> putValue cache board depth eval' bound bestMove
            _ -> return ()
        let result = ((eval', moves'), nodes + 1)
        return result
  case tableHit of
    Just (TranspositionValue bound eval cachedDepth bestMove) ->
      if cachedDepth < depth
      then doSearch
      else if ((bound == Exact) ||
               (bound == LowerBound && eval > beta) ||
               (bound == UpperBound && eval < alpha))
      then return ((eval, moves ++ [bestMove]), nodesParsed)
      else doSearch
    Nothing -> doSearch

sortCandidates :: ChessCache s -> ChessBoard -> PositionEval -> Int -> [Move] -> ST s [Move]
sortCandidates cache board alpha depth candidates =
    do
        (goodMovesFromCache, otherMoves) <- partitionAndSortCacheMoves cache candidates
        let (goodCaptureMoves, badCaptureMoves, otherMoves') = partitionAndSortCaptureMoves otherMoves
        (killerMoves, otherMoves'') <- partitionKillerMoves cache depth otherMoves'
        return $ goodMovesFromCache ++ goodCaptureMoves ++ killerMoves ++ badCaptureMoves ++ otherMoves''
  where
    partitionAndSortCacheMoves :: ChessCache s -> [Move] -> ST s ([Move], [Move])
    partitionAndSortCacheMoves cache moves = do
        maybeCachedMove <- getValue cache board
        return $ case maybeCachedMove of
            Just (TranspositionValue _ _ _ move) -> partition (\m -> move == m) moves
            Nothing -> ([], moves)

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
    augmentWithCaptureInfo move =
        let diff = do 
                (ChessPiece _ capturedType) <- pieceOnSquare board (toCol move) (toRow move)
                (ChessPiece _ capturingType) <- pieceOnSquare board (fromCol move) (fromRow move)
                return $ captureScore capturingType - captureScore capturedType
        in (move, diff)
    
    captureScore :: ChessPieceType -> Int
    captureScore Pawn = 1
    captureScore Horse = 3
    captureScore Bishop = 3
    captureScore Rock = 5
    captureScore Queen = 9
    captureScore King = 999

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
      let capturingMoves = (filter (\move -> (examineMove pat move) && (isJust $ candidateMoveLegal board move)) (pseudoLegalCandidateMoves board))
      if pat >= beta
        then return beta
        else foldHorizonEval cache depth board capturingMoves alpha' beta
  where
    -- examine only captures
    -- apply "delta pruning", only consider captures
    -- where captured piece + buffer > alpha (buffer recommended to be 200 centipawns)
    deltaBuffer = 2.0
    examineMove :: PositionEval -> Move -> Bool
    examineMove pat move =
      case pieceOnSquare board (toCol move) (toRow move) of
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

evaluate'' :: ChessCache s -> EvaluateParams -> [(Move, ChessBoard)] -> ST s ((PositionEval, [Move], Maybe Move), Int, TableValueBound)
evaluate'' cache params@EvaluateParams {moves, alpha, beta, depth, maxDepth, board, nodesParsed, allowNullMove} candidates
  | null candidates =
      let eval = outOfMovesEval board
       in return ((eval, moves, Nothing), nodesParsed, Exact)
  | depth <= 0 = do
      eval <- {-# SCC "m_horizonEval" #-} horizonEval cache 10 board alpha beta
      return ((eval, moves, Nothing), nodesParsed, Exact)
  | otherwise = do 
    foldCandidates cache candidates
  where
    foldCandidates :: ChessCache s -> [(Move, ChessBoard)] -> ST s ((PositionEval, [Move], Maybe Move), Int, TableValueBound)
    foldCandidates cache candidates =
      foldCandidates' cache True (PositionEval $ (-1) / 0, [], Nothing) candidates alpha beta 0 (False, False, False) nodesParsed

    foldCandidates' :: ChessCache s -> Bool -> (PositionEval, [Move], Maybe Move) -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> Int -> (Bool, Bool, Bool) -> Int -> ST s ((PositionEval, [Move], Maybe Move), Int, TableValueBound)
    foldCandidates' cache first bestMoveValue@(bestEval, _, _) ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, nullWindowTried) nodesParsed
      | alpha >= beta = return (bestMoveValue, nodesParsed, LowerBound)
      -- try null move if there is sufficient depth left & null move is allowed (ie., wasn't done on previous move)
      | not nullMoveTried && allowNullMove && depth < 4 && (not $ playerInCheck candidateBoard (turn candidateBoard)) = do
          let params' =
                params
                  { allowNullMove = False,
                    moves = [],
                    depth = depth - 3, -- R = 2
                    board = candidateBoard {turn = if (turn board) == White then Black else White},
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha }
          (moveValue, newNodesParsed) <- do
            ((v, moves), nodes) <- evaluate' cache params'
            return ((negateEval v, moves), nodes)
          if (fst moveValue) > beta
            then -- passing up move still causes it to exceed beta -- cut off
              return ((beta, moves ++ [candidateMove], Just candidateMove), newNodesParsed, LowerBound)
            else (foldCandidates' cache first bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (True, lmrTried, nullWindowTried) newNodesParsed)

      -- if this is 3rd+ candidate move under consideration in a depth of 3+ from start,
      -- evaluate with reduced depth (LMR).
      | not lmrTried && siblingIndex > 1 && (maxDepth - depth) > 1 && not (isCaptureMove board candidateMove) = do
          let newMovesPath = moves ++ [candidateMove]
              params' =
                params
                  { moves = newMovesPath,
                    depth = (depth - 2), -- subtract 2 to lower depth search in fringe node
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha }
          evaluated' <- evaluate' cache params'
          let (moveValue@(eval, _, _), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, moves, Just candidateMove), nodes)
          if eval > bestEval
            then -- if found better move, re-evaluate with proper depth
              (foldCandidates' cache False bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, True, nullWindowTried) newNodesParsed)
            else (foldCandidates' cache False bestMoveValue restCandidates alpha beta (siblingIndex + 1) (False, False, False) newNodesParsed)

      | (not nullWindowTried) && not first = do
          let nullBeta = case alpha of PositionEval v -> PositionEval (v + 0.0001)
              newMovesPath = moves ++ [candidateMove]
              params' =
                params
                  { moves = newMovesPath,
                    depth = (depth - 1),
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval nullBeta,
                    beta = negateEval alpha }
          evaluated' <- evaluate' cache params'
          let (moveValue@(eval, _, _), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, moves, Just candidateMove), nodes)
          if eval > bestEval
            then -- found better with reduced window: do full search
              (foldCandidates' cache False bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, True) newNodesParsed)
            else do 
                -- record found upper bound (? is this correct???)
                -- putValue cache candidateBoard (depth - 1) (fst moveValue) UpperBound
                (foldCandidates' cache False bestMoveValue restCandidates alpha beta (siblingIndex + 1) (False, False, False) newNodesParsed)
      | otherwise = do
          let newMovesPath = moves ++ [candidateMove]
              params' =
                params
                  { moves = newMovesPath,
                    depth = (depth - 1),
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha }
          evaluated' <- evaluate' cache params'
          let (moveValue@(eval, _, _), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, moves, Just candidateMove), nodes)
              newBestMoveValue =
                if (first || eval > bestEval)
                  then moveValue
                  else bestMoveValue
              newAlpha = max alpha (max eval bestEval)
          foldCandidates' cache False newBestMoveValue restCandidates newAlpha beta (siblingIndex + 1) (False, False, False) newNodesParsed
    foldCandidates' _ _ bestMoveValue [] _ _ _ _ nodesParsed = return (bestMoveValue, nodesParsed, Exact)


evaluateIteration :: ChessCache s -> ChessBoard -> (PositionEval, [Move]) -> Int -> ST s ((PositionEval, [Move]), Int)
evaluateIteration cache board lastDepthBest depth =
  let params =
        EvaluateParams
          { moves = [],
            alpha = PositionEval $ (-1) / 0,
            beta = PositionEval $ 1 / 0,
            depth = depth,
            maxDepth = depth,
            board = board,
            nodesParsed = 0,
            currentBest = lastDepthBest,
            allowNullMove = True }
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
      return $ (depth + 1, thisDepthBest, cache, nodesParsed)
    startingDepth = 1
    firstEvaluation :: ST s (Int, (PositionEval, [Move]), ChessCache s, Int)
    firstEvaluation = do
      cache <- create
      computeNext ((startingDepth - 1), (PositionEval 0, []), cache, 0)
