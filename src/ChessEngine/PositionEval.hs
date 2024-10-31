{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module ChessEngine.PositionEval
  ( PositionEval (..),
    evaluate,
    collectEvaluationInfo,
    EvaluationContext (..),
  )
where

import ChessEngine.Board
import ChessEngine.EvaluatorData
import ChessEngine.HeuristicEvaluator
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.IORef
import Data.List (intercalate, partition, sortBy)
import Data.Maybe (isJust, mapMaybe)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Control.Concurrent.Async (race)
import Data.Foldable (foldlM, toList)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Data.Vector as VI
import qualified Data.Vector.Mutable as V
import qualified VectorBuilder.MVector as VM
import qualified VectorBuilder.Vector as VB
import GHC.IO (stToIO)

type BestMove = (PositionEval, [Move])

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
    showUCIInfo :: !Bool,
    rootBoostCandidateIndex :: !([(Move, ChessBoard)] -> [(Move, ChessBoard)]), -- TODO rename field
    threadIndex :: !Int
  }

data EvaluationContext = EvaluationContext
  { nodesParsed :: !Int,
    finished :: !Bool,
    evaluation :: !PositionEval,
    moves :: ![Move],
    latestEvaluationInfo :: [String],
    showDebug :: !Bool,
    workerThreadCount :: !Int
  }
  deriving (Show)

data CandidatesFold = CandidatesFold
  { raisedAlpha :: Bool,
    bestMoveValue :: (PositionEval, [Move]),
    alpha :: PositionEval,
    beta :: PositionEval,
    siblingIndex :: !Int,
    nodesParsed :: !Int
  }

type App = ReaderT (IORef EvaluationContext) IO

evaluate' :: ChessCache -> EvaluateParams -> App (BestMove, Int)
evaluate' cache params@EvaluateParams {board, threadIndex, depth = depth', maxDepth, rootBoostCandidateIndex, nodesParsed, alpha, beta, ply} =
  if is3foldRepetition board
    then return ((PositionEval 0, []), 0)
    else do
      let depth = max depth' 0
      tableHit <- liftIO $ getValue cache board
      let doSearch = do
            candidateMovesVec <- liftIO $ stToIO $ VM.build $ (pseudoLegalCandidateMoves board)
            liftIO $ sortCandidates cache board ply threadIndex candidateMovesVec
            sortedCandidates <- liftIO $ V.foldr (:) [] candidateMovesVec
            let sortedCandidatesWithBoards = mapMaybe (\move -> (\board' -> (move, board')) <$> candidateMoveLegal board move) sortedCandidates
            let sortedCandidatesWithBoards' = 
                    if depth == maxDepth
                    then rootBoostCandidateIndex sortedCandidatesWithBoards
                    else sortedCandidatesWithBoards
            ((eval', moves'), nodes, bound) <- evaluate'' cache params sortedCandidatesWithBoards'
            when (bound == LowerBound) $
              liftIO $
                case moves' of
                  move : _ -> unless (isJust $ getCaptureInfo board move) $ putKillerMove cache (ply, threadIndex) move
                  _ -> return ()
            liftIO $ putValue cache board depth eval' bound moves'
            let result = ((eval', moves'), nodes + 1)
            return result
      case tableHit of
        Just (TranspositionValue bound eval cachedDepth bestMoveLine) ->
          if (cachedDepth < depth)
            then doSearch
            else
              if ( (bound == Exact)
                     || (bound == LowerBound && eval > beta)
                     || (bound == UpperBound && eval < alpha)
                 )
                then return ((eval, bestMoveLine), nodesParsed)
                else doSearch
        Nothing -> doSearch

sortCandidates :: ChessCache -> ChessBoard -> Int -> Int -> V.IOVector Move -> IO ()
sortCandidates cache board ply threadIndex candidates = do
    offset <- partitionAndSortCacheMoves candidates
    offset <- partitionAndSortCaptureMoves board $ V.slice offset (V.length candidates - offset) candidates
    _ <- partitionKillerMoves $ V.slice offset (V.length candidates - offset) candidates
    return ()
  where
    partitionAndSortCacheMoves :: V.IOVector Move -> IO Int
    partitionAndSortCacheMoves moves = do
      maybeCachedMove <- getValue cache board
      case maybeCachedMove of
        Just (TranspositionValue _ _ _ (move : _)) -> partitionVector (\m -> move == m) moves
        _ -> return 0

    partitionKillerMoves :: V.IOVector Move -> IO Int
    partitionKillerMoves moves = do
      killers <- getKillerMoves cache (ply, threadIndex)
      partitionVector (\m -> elem m killers) moves

partitionAndSortCaptureMoves :: ChessBoard -> V.IOVector Move -> IO Int
partitionAndSortCaptureMoves board moves =
  {-# SCC "m_partitionAndSortCaptureMoves" #-}
  do
  captureMovesOffset <- partitionVector (\m -> case augmentWithCaptureInfo m of (_, Just n) -> n >= 0; _ -> False) moves
  return captureMovesOffset
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
            (capturingType, capturedType) <- getCaptureInfo board move
            return $ captureScore capturedType - captureScore capturingType
       in (move, diff)

partitionVector :: forall a. (a -> Bool) -> V.IOVector a -> IO Int
partitionVector test vec = do
    matched <- go 0 0
    return matched
    where
      size = V.length vec
      go matched unmatched
        | matched + unmatched == size = return matched
        | otherwise = do
            el <- V.read vec matched
            if test el
            then go (matched + 1) unmatched
            else do
                V.swap vec matched (size - unmatched - 1) 
                go matched (unmatched + 1)
        


horizonEval :: ChessCache -> ChessBoard -> PositionEval -> PositionEval -> IO PositionEval
horizonEval cache board alpha beta =
  {-# SCC "m_horizonEval" #-}
  if playerInCheck board
    then
      {-# SCC "m_horizonEval_incheck" #-}
      let moves = sortMoves $ filter (\move -> (isJust $ candidateMoveLegal board move)) (pseudoLegalCandidateMovesList board)
       in if (null moves)
            then return $ outOfMovesEval board
            else foldHorizonEval cache board moves alpha beta
    else
      {-# SCC "m_horizonEval_not_incheck" #-}
      do
        pat <- finalDepthEval cache board
        let alpha' = max alpha pat
        let capturingMoves = sortMoves $ (filter (\move -> (examineCaptureMove pat move) && (isJust $ candidateMoveLegal board move)) (pseudoLegalCandidateMovesList board))
        if pat >= beta
          then return pat
          else foldHorizonEval cache board capturingMoves alpha' beta
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

    sortMoves moves = moves
        -- TODO
        {-
      let (goodCaptures, badCaptures, other) = partitionAndSortCaptureMoves board moves
       in goodCaptures ++ badCaptures ++ other
       -}

{-
foldHorizonEval :: ChessCache -> ChessBoard -> [Move] -> PositionEval -> PositionEval -> IO PositionEval
foldHorizonEval cache board (move : rest) alpha beta = do
  value <- negateEval <$> horizonEval cache (applyMoveUnsafe board move) (negateEval beta) (negateEval alpha)
  let alpha' = max alpha value
  if value >= beta
    then return value
    else foldHorizonEval cache board rest alpha' beta
foldHorizonEval _ _ [] alpha _ = return alpha
-}
foldHorizonEval :: ChessCache -> ChessBoard -> [Move] -> PositionEval -> PositionEval -> IO PositionEval
foldHorizonEval cache board moves alpha beta = do
    result <- runExceptT $ foldlM foldStep (alpha, beta) moves
    return $ case result of
                Left value -> value
                Right (alpha, _) -> alpha
    where
        foldStep :: (PositionEval, PositionEval) -> Move -> ExceptT PositionEval IO (PositionEval, PositionEval)
        foldStep (alpha, beta) move = do
            value <- liftIO $ negateEval <$> horizonEval cache (applyMoveUnsafe board move) (negateEval beta) (negateEval alpha)
            let alpha' = max alpha value
            if value >= beta
              then except $ Left value
              else return (alpha', beta)

evaluate'' :: ChessCache -> EvaluateParams -> [(Move, ChessBoard)] -> App (BestMove, Int, TableValueBound)
evaluate'' cache params@EvaluateParams {alpha, beta, depth, maxDepth, ply, board, nodesParsed, allowNullMove, showUCIInfo, threadIndex} candidates
  | null candidates =
      let eval = outOfMovesEval board
       in return ((eval, []), nodesParsed, Exact)
  | depth <= 0 = do
      eval <- liftIO $ horizonEval cache board alpha beta
      let bound =
            if eval <= alpha
              then UpperBound
              else
                if eval >= beta
                  then LowerBound
                  else Exact
      return ((eval, []), nodesParsed, bound)
  | otherwise = do
      -- try null move if there is sufficient depth left & null move is allowed (ie., wasn't done on previous move)
      -- only run when we're in a null window context (TODO why not set null window here?)
      -- don't use null move in end game (when side to move has rook or less) to avoig zugzwang
      tryNullMove <-
        if allowNullMove
          && isNullWindow alpha beta
          && depth > 3
          && (not $ playerInCheck board)
          && (quickMaterialCount board (turn board) > 5)
          then do
            pat <- liftIO $ finalDepthEval cache board
            return $ pat > beta
          else return False
      if tryNullMove
        then do
          (newNodesParsed, isCut) <- evaluateNullMove
          if isCut
            then return ((beta, []), newNodesParsed, LowerBound)
            else foldCandidates
        else foldCandidates
  where
    evaluateNullMove :: App (Int, Bool)
    evaluateNullMove = do
      let params' =
            params
              { allowNullMove = False,
                depth = depth - 3, -- R = 2
                board = board {turn = otherPlayer (turn board)},
                alpha = negateEval beta,
                beta = negateEval alpha
              }
      ((moveValue, moves), newNodesParsed) <- do
        ((v, moves), nodes) <- evaluate' cache params'
        return ((negateEval v, moves), nodes)
      return (newNodesParsed, moveValue > beta)

    foldCandidates :: App ((PositionEval, [Move]), Int, TableValueBound)
    foldCandidates = do
      let candidatesFold = CandidatesFold {raisedAlpha = False, bestMoveValue = (PositionEval $ (-10000), []), alpha = alpha, beta = beta, siblingIndex = 0, nodesParsed = nodesParsed}
      result <- runExceptT $ foldlM foldCandidatesStep candidatesFold candidates
      return $ case result of
                Left value -> value
                Right CandidatesFold { raisedAlpha, bestMoveValue, nodesParsed, alpha, beta } ->
                    if alpha >= beta 
                    then (bestMoveValue, nodesParsed, LowerBound)
                    else (bestMoveValue, nodesParsed, if raisedAlpha then Exact else UpperBound)

    foldCandidatesStep :: CandidatesFold -> (Move, ChessBoard)-> ExceptT (BestMove, Int, TableValueBound) App CandidatesFold
    foldCandidatesStep candidatesFold@CandidatesFold {raisedAlpha, bestMoveValue = bestMoveValue@(bestEval, _), alpha, beta, siblingIndex, nodesParsed} (candidateMove, candidateBoard)
        | alpha >= beta = except $ Left (bestMoveValue, nodesParsed, LowerBound)
        | tryNullWindow = executeNullWindow tryLmr
        | tryLmr = executeLmr
        | otherwise = executeDefault

        where 
            tryNullWindow = siblingIndex > 0 && not (isNullWindow alpha beta)

            executeNullWindow :: Bool -> ExceptT (BestMove, Int, TableValueBound) App CandidatesFold
            executeNullWindow tryLmr = do
              let nullBeta = case alpha of PositionEval v -> PositionEval (v + 1)
                  params' =
                    params
                      { depth = (depth - (if tryLmr then 3 else 1)),
                        ply = ply + 1,
                        board = candidateBoard,
                        nodesParsed = nodesParsed,
                        alpha = negateEval nullBeta,
                        beta = negateEval alpha
                      }
              evaluated' <- lift $ evaluate' cache params'
              let (moveValue@(eval, _), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, candidateMove : moves), nodes)
              -- found better eval with lower depth and/or window -- retry
              if eval > alpha
                then
                  if tryLmr
                    then -- retry with null window but without lmr
                      executeNullWindow False
                    else -- retry full search
                      executeDefault
                else return candidatesFold { siblingIndex = (siblingIndex + 1), nodesParsed = newNodesParsed}

            executeDefault :: ExceptT (BestMove, Int, TableValueBound) App CandidatesFold
            executeDefault = do
              let params' =
                    params
                      { depth = (depth - 1),
                        ply = ply + 1,
                        board = candidateBoard,
                        nodesParsed = nodesParsed,
                        alpha = negateEval beta,
                        beta = negateEval alpha
                      }
              evaluated' <- lift $ evaluate' cache params'
              let (moveValue@(eval, moveLine), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, candidateMove : moves), nodes)
              newBestMoveValue <-
                if (siblingIndex == 0 || eval > bestEval)
                  then do
                    when (depth == maxDepth) $ do
                      let lastEvalInfo = collectEvaluationInfo (turn board) nodesParsed eval moveLine
                      when (threadIndex == 1) $ do
                          env <- lift ask
                          result <- liftIO $ readIORef env
                          let newResult = result {nodesParsed = newNodesParsed, evaluation = eval, moves = moveLine, latestEvaluationInfo = lastEvalInfo}
                          liftIO $ writeIORef env newResult
                      when showUCIInfo $
                        liftIO $
                          forM_ lastEvalInfo putStrLn
                    return moveValue
                  else return bestMoveValue
              let (alpha', raisedAlpha') =
                    let v = max eval bestEval
                     in if (v > alpha)
                          then (v, True)
                          else (alpha, raisedAlpha)
              return candidatesFold {raisedAlpha = raisedAlpha', bestMoveValue = newBestMoveValue, alpha = alpha', siblingIndex = (siblingIndex + 1), nodesParsed = newNodesParsed}
            
            tryLmr = siblingIndex > 1 && depth > 2

            executeLmr :: ExceptT (BestMove, Int, TableValueBound) App CandidatesFold
            executeLmr = do
              let params' =
                    params
                      { depth = (depth - 3), -- subtract 2 to lower depth search in fringe node
                        ply = ply + 1,
                        board = candidateBoard,
                        nodesParsed = nodesParsed,
                        alpha = negateEval beta,
                        beta = negateEval alpha
                      }
              evaluated' <- lift $ evaluate' cache params'
              let (moveValue@(eval, _), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, candidateMove : moves), nodes)
              if eval > bestEval
                then -- if found better move, re-evaluate with proper depth
                  executeDefault
                else
                  return candidatesFold { siblingIndex = (siblingIndex + 1), nodesParsed = newNodesParsed}





{-

      | alpha >= beta = return (bestMoveValue, nodesParsed, LowerBound)
      | (not nullWindowTried) && siblingIndex > 0 && not (isNullWindow alpha beta) = do
          let nullBeta = case alpha of PositionEval v -> PositionEval (v + 1)
              params' =
                params
                  { depth = (depth - (if tryLmr then 3 else 1)),
                    ply = ply + 1,
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval nullBeta,
                    beta = negateEval alpha
                  }
          evaluated' <- evaluate' cache params'
          let (moveValue@(eval, _), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, candidateMove : moves), nodes)
          -- found better eval with lower depth and/or window -- retry
          if eval > alpha
            then
              if tryLmr
                then -- retry with null window but without lmr
                  foldCandidates' candidatesFold {lmrTried = True, nodesParsed = newNodesParsed}
                else -- retry full search
                  foldCandidates' candidatesFold {lmrTried = True, nullWindowTried = True, nodesParsed = newNodesParsed}
            else foldCandidates' candidatesFold {candidates = restCandidates, siblingIndex = (siblingIndex + 1), lmrTried = False, nullWindowTried = False, nodesParsed = newNodesParsed}

      -- if this is 3rd+ candidate move under consideration in a depth of 3+ from start,
      -- evaluate with reduced depth (LMR).
      | tryLmr = do
          let params' =
                params
                  { depth = (depth - 3), -- subtract 2 to lower depth search in fringe node
                    ply = ply + 1,
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha
                  }
          evaluated' <- evaluate' cache params'
          let (moveValue@(eval, _), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, candidateMove : moves), nodes)
          if eval > bestEval
            then -- if found better move, re-evaluate with proper depth
              foldCandidates' candidatesFold {lmrTried = True, nodesParsed = newNodesParsed}
            else
              foldCandidates' candidatesFold {candidates = restCandidates, siblingIndex = (siblingIndex + 1), lmrTried = False, nullWindowTried = False, nodesParsed = newNodesParsed}
      | otherwise = do
          let params' =
                params
                  { depth = (depth - 1),
                    ply = ply + 1,
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha
                  }
          evaluated' <- evaluate' cache params'
          let (moveValue@(eval, moveLine), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, candidateMove : moves), nodes)
          newBestMoveValue <-
            if (siblingIndex == 0 || eval > bestEval)
              then do
                when (depth == maxDepth) $ do
                  let lastEvalInfo = collectEvaluationInfo (turn board) nodesParsed eval moveLine
                  when (threadIndex == 1) $ do
                      env <- ask
                      result <- liftIO $ readIORef env
                      let newResult = result {nodesParsed = newNodesParsed, evaluation = eval, moves = moveLine, latestEvaluationInfo = lastEvalInfo}
                      liftIO $ writeIORef env newResult
                  when showUCIInfo $
                    liftIO $
                      forM_ lastEvalInfo putStrLn
                return moveValue
              else return bestMoveValue
          let (alpha', raisedAlpha') =
                let v = max eval bestEval
                 in if (v > alpha)
                      then (v, True)
                      else (alpha, raisedAlpha)
          foldCandidates' candidatesFold {raisedAlpha = raisedAlpha', bestMoveValue = newBestMoveValue, candidates = restCandidates, alpha = alpha', siblingIndex = (siblingIndex + 1), lmrTried = False, nullWindowTried = False, nodesParsed = newNodesParsed}
      where
        tryLmr = not lmrTried && siblingIndex > 1 && depth > 2
    foldCandidates' CandidatesFold {raisedAlpha, bestMoveValue, nodesParsed}
      | alpha >= beta = return (bestMoveValue, nodesParsed, LowerBound)
      | otherwise = return (bestMoveValue, nodesParsed, if raisedAlpha then Exact else UpperBound)
-}

isNullWindow :: PositionEval -> PositionEval -> Bool
isNullWindow (PositionEval alpha) (PositionEval beta) = (beta - alpha) <= 1

evaluateIterationThread :: ChessCache -> ChessBoard -> (PositionEval, [Move]) -> Int -> Bool -> ([(Move, ChessBoard)] -> [(Move, ChessBoard)]) -> Int -> App (BestMove, Int)
evaluateIterationThread cache board lastDepthBest depth showDebug rootBoostCandidateIndex threadIndex =
  let params =
        EvaluateParams
          { alpha = PositionEval (-10000),
            beta = PositionEval 10000,
            depth = depth,
            maxDepth = depth,
            ply = 0,
            board = board,
            nodesParsed = 0,
            currentBest = lastDepthBest,
            allowNullMove = True,
            showUCIInfo = showDebug,
            rootBoostCandidateIndex = rootBoostCandidateIndex,
            threadIndex = threadIndex }
   in do
        evaluate' cache params

evaluateIteration :: ChessCache -> ChessBoard -> (PositionEval, [Move]) -> Int -> Int -> Bool -> App (BestMove, Int)
evaluateIteration cache board lastDepthBest depth nodes showDebug = do
    env <- ask
    context <- liftIO $ readIORef env
    let threadCount = workerThreadCount context
    -- don't use extra threading for low depth
    let workerThreadCount = if depth > 5 then [2..threadCount] else []
    let threadActions = makeThreadAction env threadCount <$> 1 :| workerThreadCount
    (bestMove@(eval, moveLine), newNodes') <- liftIO $ raceMany threadActions
    -- (bestMove@(eval, moveLine), newNodes') <- liftIO $ parallelWaitForFirst threadActions
    let newNodes = nodes + newNodes' -- since thread returns only its nodes, not global
    let lastEvalInfo = collectEvaluationInfo (turn board) newNodes eval moveLine
    let newResult = context {nodesParsed = newNodes, evaluation = eval, moves = moveLine, latestEvaluationInfo = lastEvalInfo}
    liftIO $ writeIORef env newResult
    return (bestMove, newNodes)
    where
        makeThreadAction :: IORef EvaluationContext -> Int -> Int -> IO (BestMove, Int)
        makeThreadAction evalResultRef threadCount threadIndex = runReaderT (do
                let scrambleCandidates :: [(Move, ChessBoard)] -> [(Move, ChessBoard)]
                    scrambleCandidates candidates = case splitAt (threadCount * 2) candidates  of
                                                        (xs, rest) -> if ((threadIndex - 1) * 2) < length xs
                                                                      then (rotate ((threadIndex - 1) * 2) xs) ++ rest
                                                                      else candidates
                                                    
                evaluateIterationThread cache board lastDepthBest depth showDebug scrambleCandidates threadIndex)
            evalResultRef

rotate :: Int -> [a] -> [a]
rotate 0 els = els
rotate count els = drop count els ++ take count els

raceMany :: NonEmpty (IO a) -> IO a
raceMany (a :| []) = a
raceMany (a :| b : rest) = do
    result <- race a (raceMany $ b :| rest)
    return $ case result of 
        Left l -> l
        Right r -> r

iterateM' :: (a -> App a) -> App a -> Int -> App a
iterateM' f mx n = do
  if n == 0
    then mx
    else iterateM' f (mx >>= f) (n - 1)

evaluate :: IORef EvaluationContext -> ChessBoard -> Int -> IO EvaluationContext
evaluate evalResultRef board targetDepth = runReaderT evaluateInReader evalResultRef
  where
    evaluateInReader :: App EvaluationContext
    evaluateInReader = do
      (_, (eval, moves), _, nodesParsed) <- iterateM' computeNext firstEvaluation (targetDepth - startingDepth)
      let lastEvalInfo = collectEvaluationInfo (turn board) nodesParsed eval moves
      env <- ask
      result' <- liftIO $ readIORef env
      let result = result' {moves = moves, nodesParsed = nodesParsed, finished = True, evaluation = eval, latestEvaluationInfo = lastEvalInfo}
      liftIO $ writeIORef env result
      return result

    computeNext :: (Int, BestMove, ChessCache, Int) -> App (Int, BestMove, ChessCache, Int)
    computeNext current = do
      let (depth, lastDepthBest, cache, nodesParsed) = current
      env <- ask
      result <- liftIO $ readIORef env
      (thisDepthBest, nodesParsed) <- evaluateIteration cache board lastDepthBest (depth + 1) nodesParsed (showDebug result)
      return $ (depth + 1, thisDepthBest, cache, nodesParsed)
    startingDepth = 1
    firstEvaluation :: App (Int, (PositionEval, [Move]), ChessCache, Int)
    firstEvaluation = do
      cache <- liftIO create
      computeNext ((startingDepth - 1), (PositionEval 0, []), cache, 0)

collectEvaluationInfo :: PlayerColor -> Int -> PositionEval -> [Move] -> [String]
collectEvaluationInfo player nodesParsed (PositionEval value) moves =
  let m = if player == White then 1 else -1
   in [ "info nodes " ++ show nodesParsed,
        "info pv " ++ (intercalate " " (mapMaybe moveToString moves)),
        "info score cp " ++ show (value * m)
      ]
