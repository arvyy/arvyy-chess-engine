{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module ChessEngine.PositionEval
  ( PositionEval (..),
    evaluate,
    collectEvaluationInfo,
    EvaluateResult (..),
  )
where

import ChessEngine.Board
import ChessEngine.EvaluatorData
import ChessEngine.HeuristicEvaluator
import Control.Monad
import Control.Monad.ST
import Data.List (partition, sortBy, intercalate)
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Debug.Trace
import Control.Monad.Trans.Reader
import Data.IORef
import Control.Monad.IO.Class (liftIO)

-- end of the game
outOfMovesEval :: ChessBoard -> PositionEval
outOfMovesEval board =
  let inCheck = playerInCheck board
   in if (not inCheck)
        then PositionEval 0
        else PositionEval (-10000)

data EvaluateParams = EvaluateParams
  { alpha :: !PositionEval,
    beta :: !PositionEval,
    ply :: !Int,
    depth :: !Int,
    maxDepth :: !Int,
    board :: !ChessBoard,
    nodesParsed :: !Int,
    currentBest :: !(PositionEval, [Move]),
    allowNullMove :: !Bool,
    showUCIInfo :: !Bool }

-- TODO rename to Context
data EvaluateResult = EvaluateResult
  { nodesParsed :: !Int,
    finished :: !Bool,
    evaluation :: !PositionEval,
    moves :: ![Move],
    latestEvaluationInfo :: [String],
    showDebug :: !Bool }
  deriving (Show)

type App = ReaderT (IORef EvaluateResult) IO

evaluate' :: ChessCache -> EvaluateParams -> App ((PositionEval, [Move]), Int)
evaluate' cache params@EvaluateParams {board, depth = depth', nodesParsed, alpha, beta, ply } =
  if is3foldRepetition board
  then return ((PositionEval 0, []), 0)
  else do
      let depth = max depth' 0
      tableHit <- liftIO $ getValue cache board
      let doSearch = do
            sortedCandidates <- liftIO $ sortCandidates cache board ply (pseudoLegalCandidateMoves board)
            let sortedCandidatesWithBoards = mapMaybe (\move -> (\board' -> (move, board')) <$> candidateMoveLegal board move) sortedCandidates
            ((eval', moves', bestMove), nodes, bound) <- evaluate'' cache params sortedCandidatesWithBoards
            when (bound == LowerBound) $ liftIO $
                 case moves' of
                    move : _ -> when (not $ isCaptureMove board move) $ putKillerMove cache ply move
                    _ -> return ()
            liftIO $ putValue cache board depth eval' bound moves'
            let result = ((eval', moves'), nodes + 1)
            return result
      case tableHit of
        Just (TranspositionValue bound eval cachedDepth bestMoveLine) ->
          if (cachedDepth < depth)
          then doSearch
          else if ((bound == Exact) ||
                   (bound == LowerBound && eval > beta) ||
                   (bound == UpperBound && eval < alpha))
          then return ((eval, bestMoveLine), nodesParsed)
          else doSearch
        Nothing -> doSearch

sortCandidates :: ChessCache -> ChessBoard -> Int -> [Move] -> IO [Move]
sortCandidates cache board ply candidates =
    do
        (goodMovesFromCache, otherMoves) <- partitionAndSortCacheMoves cache candidates
        let (goodCaptureMoves, badCaptureMoves, otherMoves') = partitionAndSortCaptureMoves board otherMoves
        (killerMoves, otherMoves'') <- partitionKillerMoves cache otherMoves'
        return $ goodMovesFromCache ++ goodCaptureMoves ++ killerMoves ++ badCaptureMoves ++ otherMoves''
  where
    partitionAndSortCacheMoves :: ChessCache -> [Move] -> IO ([Move], [Move])
    partitionAndSortCacheMoves cache moves = do
        maybeCachedMove <- getValue cache board
        return $ case maybeCachedMove of
            Just (TranspositionValue _ _ _ (move : _)) -> partition (\m -> move == m) moves
            _ -> ([], moves)

    partitionKillerMoves :: ChessCache -> [Move] -> IO ([Move], [Move])
    partitionKillerMoves cache moves = do
        killers <- getKillerMoves cache ply
        return $ partition (\m -> elem m killers) moves

partitionAndSortCaptureMoves :: ChessBoard -> [Move] -> ([Move], [Move], [Move])
partitionAndSortCaptureMoves board moves = {-# SCC "m_partitionAndSortCaptureMoves" #-}
    let augmentedMoves = augmentWithCaptureInfo <$> moves
        (captureMoves, otherMoves) = partition (\(_, capture) -> isJust capture) augmentedMoves
        (goodCaptures, badCaptures) = partition (\(_, Just n) -> n >= 0) captureMoves
        removeCaptureInfo (move, _) = move
        captureMoveComparator (_, Just v1) (_, Just v2) = compare v1 v2
        goodCaptures' = removeCaptureInfo <$> (sortBy (flip captureMoveComparator) goodCaptures)
        badCaptures' = removeCaptureInfo <$> (sortBy (flip captureMoveComparator) badCaptures)
        otherMoves' = removeCaptureInfo <$> otherMoves
    in (goodCaptures', badCaptures', otherMoves')

    where
        captureScore :: ChessPieceType -> Int
        captureScore Pawn = 100
        captureScore Horse = 300
        captureScore Bishop = 300
        captureScore Rock = 500
        captureScore Queen = 900
        captureScore King = 9999

        augmentWithCaptureInfo :: Move -> (Move, Maybe Int)
        augmentWithCaptureInfo move =
            let diff = do 
                    (ChessPiece _ capturedType) <- pieceOnSquare board (toCol move) (toRow move)
                    (ChessPiece _ capturingType) <- pieceOnSquare board (fromCol move) (fromRow move)
                    return $ captureScore capturedType - captureScore capturingType
            in (move, diff)

horizonEval :: ChessCache -> Int -> ChessBoard -> PositionEval -> PositionEval -> IO PositionEval
horizonEval cache depth board alpha beta =  {-# SCC "m_horizonEval" #-}
      if playerInCheck board
      then {-# SCC "m_horizonEval_incheck" #-}
          let moves = sortMoves $ (filter (\move -> (isJust $ candidateMoveLegal board move)) (pseudoLegalCandidateMoves board))
          in if (null moves) 
             then return $ outOfMovesEval board
             else foldHorizonEval cache depth board moves alpha beta
      else {-# SCC "m_horizonEval_not_incheck" #-} do
          pat <- finalDepthEval cache board
          let alpha' = max alpha pat
          let capturingMoves = sortMoves $ (filter (\move -> (examineCaptureMove pat move) && (isJust $ candidateMoveLegal board move)) (pseudoLegalCandidateMoves board))
          if pat >= beta
            then return pat
            else foldHorizonEval cache depth board capturingMoves alpha' beta
  where
    -- examine only captures when not in check
    -- apply "delta pruning", only consider captures
    -- where captured piece + buffer > alpha (buffer recommended to be 200 centipawns)
    deltaBuffer = 200
    examineCaptureMove :: PositionEval -> Move -> Bool
    examineCaptureMove pat move =
      case pieceOnSquare board (toCol move) (toRow move) of
            Just (ChessPiece _ Pawn) -> evalAdd pat (100 + deltaBuffer) > alpha
            Just (ChessPiece _ Bishop) -> evalAdd pat (300 + deltaBuffer) > alpha
            Just (ChessPiece _ Horse) -> evalAdd pat (300 + deltaBuffer) > alpha
            Just (ChessPiece _ Rock) -> evalAdd pat (500 + deltaBuffer) > alpha
            Just (ChessPiece _ Queen) -> evalAdd pat (900 + deltaBuffer) > alpha
            _ -> False
    
    sortMoves moves =
        let (goodCaptures, badCaptures, other) = partitionAndSortCaptureMoves board moves
        in goodCaptures ++ badCaptures ++ other

foldHorizonEval :: ChessCache -> Int -> ChessBoard -> [Move] -> PositionEval -> PositionEval -> IO PositionEval
foldHorizonEval cache depth board (move : rest) alpha beta = do
  value <- negateEval <$> horizonEval cache (depth - 1) (applyMoveUnsafe board move) (negateEval beta) (negateEval alpha)
  let alpha' = max alpha value
  if value >= beta
    then return value
    else foldHorizonEval cache depth board rest alpha' beta
foldHorizonEval _ _ _ [] alpha _ = return alpha

evaluate'' :: ChessCache -> EvaluateParams -> [(Move, ChessBoard)] -> App ((PositionEval, [Move], Maybe Move), Int, TableValueBound)
evaluate'' cache params@EvaluateParams { alpha, beta, depth, maxDepth, ply, board, nodesParsed, allowNullMove, showUCIInfo } candidates
  | null candidates =
      let eval = outOfMovesEval board
       in return ((eval, [], Nothing), nodesParsed, Exact)
  | depth <= 0 = do
      eval <- liftIO $ horizonEval cache 10 board alpha beta
      return ((eval, [], Nothing), nodesParsed, Exact)
  | otherwise = foldCandidates cache candidates
  where
    foldCandidates :: ChessCache -> [(Move, ChessBoard)] -> App ((PositionEval, [Move], Maybe Move), Int, TableValueBound)
    foldCandidates cache candidates =
      foldCandidates' cache True False (PositionEval $ (-10000), [], Nothing) candidates alpha beta 0 (False, False, False) nodesParsed

-- TODO struct for passing arguments around?
    foldCandidates' :: ChessCache -> Bool -> Bool -> (PositionEval, [Move], Maybe Move) -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> Int -> (Bool, Bool, Bool) -> Int -> App ((PositionEval, [Move], Maybe Move), Int, TableValueBound)
    foldCandidates' cache first raisedAlpha bestMoveValue@(bestEval, _, _) ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, nullWindowTried) nodesParsed
      | alpha >= beta = return (bestMoveValue, nodesParsed, LowerBound)
      -- try null move if there is sufficient depth left & null move is allowed (ie., wasn't done on previous move)
      -- don't use null move in end game (when opponent  has more than on minor piece) to avoig zugzwang
      -- currently disabled due to buggy implementation
      | not nullMoveTried && allowNullMove && depth >= 4 && (not $ playerInCheck candidateBoard) && (quickMaterialCount candidateBoard (turn candidateBoard) > 3) = do
          let params' =
                params
                  { allowNullMove = False,
                    ply = ply + 1,
                    depth = depth - 3, -- R = 2
                    board = candidateBoard {turn = if (turn board) == White then Black else White},
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha }
          ((moveValue, moves), newNodesParsed) <- do
            ((v, moves), nodes) <- evaluate' cache params'
            return ((negateEval v, moves), nodes)
          if moveValue > beta
            then -- passing up move still causes it to exceed beta -- cut off
              return ((beta, candidateMove : moves, Just candidateMove), newNodesParsed, LowerBound)
            else (foldCandidates' cache first raisedAlpha bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (True, lmrTried, nullWindowTried) newNodesParsed)

      -- if this is 3rd+ candidate move under consideration in a depth of 3+ from start,
      -- evaluate with reduced depth (LMR).
      | not lmrTried && siblingIndex > 1 && (maxDepth - depth) > 1 && not (isCaptureMove board candidateMove) = do
          let params' =
                params
                  { depth = (depth - 2), -- subtract 2 to lower depth search in fringe node
                    ply = ply + 1,
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha }
          evaluated' <- evaluate' cache params'
          let (moveValue@(eval, _, _), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, candidateMove : moves, Just candidateMove), nodes)
          if eval > bestEval
            then -- if found better move, re-evaluate with proper depth
              (foldCandidates' cache False raisedAlpha bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, True, nullWindowTried) newNodesParsed)
            else (foldCandidates' cache False raisedAlpha bestMoveValue restCandidates alpha beta (siblingIndex + 1) (False, False, False) newNodesParsed)

      | (not nullWindowTried) && not first && not (isNullWindow alpha beta) = do
          let nullBeta = case alpha of PositionEval v -> PositionEval (v + 1)
              params' =
                params
                  { depth = (depth - 1),
                    ply = ply + 1,
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval nullBeta,
                    beta = negateEval alpha }
          evaluated' <- evaluate' cache params'
          let (moveValue@(eval, _, _), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, candidateMove : moves, Just candidateMove), nodes)
          if eval > bestEval
            then -- found better with reduced window: do full search
              (foldCandidates' cache False raisedAlpha bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, True) newNodesParsed)
            else  
              (foldCandidates' cache False raisedAlpha bestMoveValue restCandidates alpha beta (siblingIndex + 1) (False, False, False) newNodesParsed)
      | otherwise = do
          let params' =
                params
                  { depth = (depth - 1),
                    ply = ply + 1,
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha }
          evaluated' <- evaluate' cache params'
          let (moveValue@(eval, moveLine, _), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, candidateMove : moves, Just candidateMove), nodes)
          newBestMoveValue <-
                if (first || eval > bestEval)
                  then do
                    when (depth == maxDepth) $ do
                        env <- ask
                        result <- liftIO $ readIORef env
                        let lastEvalInfo = collectEvaluationInfo (turn board) nodesParsed eval moveLine
                        let newResult = result { nodesParsed = newNodesParsed, evaluation = eval, moves = moveLine, latestEvaluationInfo = lastEvalInfo }
                        liftIO $ writeIORef env newResult
                        when showUCIInfo $
                            liftIO $ forM_ lastEvalInfo putStrLn 
                    return moveValue
                  else return bestMoveValue
          let (alpha', raisedAlpha') = 
                let v = max eval bestEval
                in if (v > alpha)
                   then (v, True)
                   else (alpha, raisedAlpha)
          foldCandidates' cache False raisedAlpha' newBestMoveValue restCandidates alpha' beta (siblingIndex + 1) (False, False, False) newNodesParsed
    foldCandidates' _ _ raisedAlpha bestMoveValue [] _ _ _ _ nodesParsed = return (bestMoveValue, nodesParsed, if raisedAlpha then Exact else UpperBound)

    isNullWindow :: PositionEval -> PositionEval -> Bool
    isNullWindow (PositionEval alpha) (PositionEval beta) = (alpha - beta) <= 1


evaluateIteration :: ChessCache -> ChessBoard -> (PositionEval, [Move]) -> Int -> Bool -> App ((PositionEval, [Move]), Int)
evaluateIteration cache board lastDepthBest depth showDebug =
  let params =
        EvaluateParams
          { alpha = PositionEval $ (-10000),
            beta = PositionEval $ 10000,
            depth = depth,
            maxDepth = depth,
            ply = 0,
            board = board,
            nodesParsed = 0,
            currentBest = lastDepthBest,
            -- TODO return null moves; currently it hallucinates and blunders pieces :(
            -- allowNullMove = True
            allowNullMove = False,
            showUCIInfo = showDebug
            }
   in do
        evaluate' cache params

iterateM' :: (a -> App a) -> App a -> Int -> App a
iterateM' f mx n = do
  if n == 0
    then mx
    else iterateM' f (mx >>= f) (n - 1)

evaluate :: (IORef EvaluateResult) -> ChessBoard -> Int -> IO EvaluateResult
evaluate evalResultRef board targetDepth = runReaderT evaluateInReader evalResultRef
  where
    evaluateInReader :: App EvaluateResult
    evaluateInReader = do
        (_, (eval, moves), _, nodesParsed) <- iterateM' computeNext firstEvaluation (targetDepth - startingDepth)
        let lastEvalInfo = collectEvaluationInfo (turn board) nodesParsed eval moves
        env <- ask
        result' <- liftIO $ readIORef env
        let result = result' { moves = moves, nodesParsed = nodesParsed, finished = True, evaluation = eval, latestEvaluationInfo = lastEvalInfo }
        liftIO $ writeIORef env result
        return result

    computeNext :: (Int, (PositionEval, [Move]), ChessCache, Int) -> App (Int, (PositionEval, [Move]), ChessCache, Int)
    computeNext current = do
      let (depth, lastDepthBest, cache, _) = current
      env <- ask
      result <- liftIO $ readIORef env
      (thisDepthBest, nodesParsed) <- evaluateIteration cache board lastDepthBest (depth + 1) (showDebug result)
      return $ (depth + 1, thisDepthBest, cache, nodesParsed)
    startingDepth = 1
    firstEvaluation :: App (Int, (PositionEval, [Move]), ChessCache, Int)
    firstEvaluation = do
      cache <- liftIO create
      computeNext ((startingDepth - 1), (PositionEval 0, []), cache, 0)

collectEvaluationInfo :: PlayerColor -> Int -> PositionEval -> [Move] -> [String]
collectEvaluationInfo player nodesParsed (PositionEval value) moves =
    let m = if player == White then 1 else -1
    in  [ "info nodes " ++ show nodesParsed
        , "info pv " ++ (intercalate " " (mapMaybe moveToString moves))
        , "info score cp " ++ show (value * m)]
