{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module ChessEngine.PositionEval
  ( PositionEval (..),
    evaluate,
    collectEvaluationInfo,
    EvaluationContext (..),

    -- exports for testing
    staticExchangeEvalWinning
  )
where

import ChessEngine.Board
import ChessEngine.EvaluatorData
import ChessEngine.HeuristicEvaluator
import Control.Concurrent.Async (race)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import Control.Monad.Trans.Reader
import Data.Foldable (foldlM)
import Data.IORef
import Data.List (intercalate, partition, sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isJust, isNothing, mapMaybe, fromJust, fromMaybe)
import Debug.Trace (trace)
import qualified ChessEngine.MutableBuffer as MB
import qualified Data.Vector.Strict as V
import GHC.IO (unsafePerformIO)
import ChessEngine.MutableBuffer (setBufferSize)

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
    threadIndex :: !Int,
    moveBuffers :: !(V.Vector (MB.MutableBuffer Move))
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
    nodesParsed :: !Int,
    futilityPruneContext :: !(Maybe (PositionEval, Int))
  }

type App = ReaderT (IORef EvaluationContext) IO

evaluate' :: ChessCache -> EvaluateParams -> App (BestMove, Int)
evaluate' cache params@EvaluateParams { board, threadIndex, depth = depth', maxDepth, rootBoostCandidateIndex, nodesParsed, alpha, beta, ply, moveBuffers } =
  if ((is3foldRepetition board || insufficientMaterial board) && ply > 0)
    then return ((PositionEval 0, []), 0)
    else do
      let depth = max depth' 0
      tableHit <- liftIO $ getValue cache board
      let doSearch = do
            let buffer = moveBuffers V.! ply
            liftIO $ MB.reset buffer
            liftIO $ MB.pushMany buffer $ pseudoLegalCandidateMoves board
            liftIO $ sortCandidates cache board ply threadIndex buffer
            {-
            let sortedCandidatesWithBoards = mapMaybe (\move -> (\board' -> (move, board')) <$> candidateMoveLegal board move) sortedCandidates
            let sortedCandidatesWithBoards' =
                  if ply == 0
                    then rootBoostCandidateIndex sortedCandidatesWithBoards
                    else sortedCandidatesWithBoards
            -}
            hasCandidates <- (> 0) <$> (liftIO $ MB.bufferSize buffer)
            ((eval', moves'), nodes, bound) <- evaluate'' cache params buffer hasCandidates
            when (bound == LowerBound) $
              liftIO $
                case moves' of
                  move : _ -> unless (isJust $ getCaptureInfo move) $ putKillerMove cache (ply, threadIndex) move
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
                     || (bound == LowerBound && eval >= beta)
                     || (bound == UpperBound && eval < alpha)
                 )
                then return ((eval, bestMoveLine), nodesParsed)
                else doSearch
        Nothing -> doSearch

sortCandidates :: ChessCache -> ChessBoard -> Int -> Int -> MB.MutableBuffer Move -> IO ()
sortCandidates cache board ply threadIndex candidates = do
    partitionValidMoves
    (_, otherMoves) <- partitionCacheMoves
    (captures, otherMoves) <- partitionCaptures otherMoves
    (winningCaptures, losingCaptures) <- partitionWinningCaptures captures
    sortCaptures winningCaptures
    sortCaptures losingCaptures
    partitionKillerMoves otherMoves
    return ()
  where
    
    partitionValidMoves = do
        ((_, end), _) <- MB.partition (\m -> isJust $ candidateMoveLegal board m) candidates
        MB.setBufferSize candidates end

    partitionCacheMoves = do
      maybeCachedMove <- getValue cache board
      case maybeCachedMove of
        Just (TranspositionValue _ _ _ (move : _)) -> MB.partition (== move) candidates
        _ -> do
                r <- MB.range candidates
                return ((0, 0), r)

    partitionCaptures range = do
        MB.partition' (\m -> isJust $ getCaptureInfo m) candidates range

    partitionWinningCaptures range = do
        MB.partition' (\m -> staticExchangeEvalWinning board m >= 0) candidates range

    moveHasBetterCapture move1 move2 =
        let (capturingType1, capturedType1) = fromJust $ getCaptureInfo move1
            (capturingType2, capturedType2) = fromJust $ getCaptureInfo move2
            captureDiffScore1 = captureScore capturedType1 - captureScore capturingType1
            captureDiffScore2 = captureScore capturedType2 - captureScore capturingType2
        in compare captureDiffScore1 captureDiffScore2
            
    sortCaptures range =
        MB.sort (flip moveHasBetterCapture) candidates range

    partitionKillerMoves range = do
      killers <- getKillerMoves cache (ply, threadIndex)
      MB.partition' (\m -> elem m killers) candidates range

{-
sortCandidates ::  ChessCache -> ChessBoard -> Int -> Int -> [Move] -> IO [Move]
sortCandidates cache board ply threadIndex candidates =
  do
    (goodMovesFromCache, otherMoves) <- partitionAndSortCacheMoves candidates
    let (goodCaptureMoves, badCaptureMoves, otherMoves') = partitionAndSortCaptureMoves board otherMoves
    (killerMoves, otherMoves'') <- partitionKillerMoves otherMoves'
    return $ goodMovesFromCache ++ goodCaptureMoves ++ killerMoves ++ badCaptureMoves ++ otherMoves''
  where
    partitionAndSortCacheMoves :: [Move] -> IO ([Move], [Move])
    partitionAndSortCacheMoves moves = do
      maybeCachedMove <- getValue cache board
      return $ case maybeCachedMove of
        Just (TranspositionValue _ _ _ (move : _)) -> partition (\m -> move == m) moves
        _ -> ([], moves)

    partitionKillerMoves :: [Move] -> IO ([Move], [Move])
    partitionKillerMoves moves = do
      killers <- getKillerMoves cache (ply, threadIndex)
      return $ partition (\m -> elem m killers) moves
-}

partitionAndSortCaptureMoves ::  ChessBoard ->  [Move] -> ([Move], [Move], [Move])
partitionAndSortCaptureMoves board moves =
  {-# SCC "m_partitionAndSortCaptureMoves" #-}
  let augmentedMoves = augmentWithCaptureInfo <$> moves
      (captureMoves, otherMoves) = partition (\(_, capture) -> isJust capture) augmentedMoves
      (goodCaptures, badCaptures) = partition (\(_, Just (sse, _)) -> sse >= 0) captureMoves
      removeCaptureInfo (move, _) = move
      captureMoveComparator (_, Just (_, captureDiff1)) (_, Just (_, captureDiff2)) = compare captureDiff1 captureDiff2
      goodCaptures' = removeCaptureInfo <$> (sortBy (flip captureMoveComparator) goodCaptures)
      badCaptures' = removeCaptureInfo <$> (sortBy (flip captureMoveComparator) badCaptures)
      otherMoves' = removeCaptureInfo <$> otherMoves
   in (goodCaptures', badCaptures', otherMoves')
  where
    -- if it's a capture, second element is Just (sse score, mva-llv score)
    augmentWithCaptureInfo :: Move -> (Move, Maybe (Int, Int))
    augmentWithCaptureInfo move =
      let score = do
            (capturingType, capturedType) <- getCaptureInfo move
            let sseScore = staticExchangeEvalWinning board move
            let captureDiffScore = captureScore capturedType - captureScore capturingType
            return (sseScore, captureDiffScore)
       in (move, score)

captureScore :: ChessPieceType -> Int
captureScore Pawn = 100
captureScore Horse = 300
captureScore Bishop = 300
captureScore Rock = 500
captureScore Queen = 900
captureScore King = 9999

staticExchangeEvalWinning ::  ChessBoard -> Move -> Int
staticExchangeEvalWinning board move =
  let chessPiece = pieceOnSquare board x y
      attackerPiece = pieceOnSquare board (fromCol move) (fromRow move)
  in scoreUnderAttack chessPiece (fromCol move) (fromRow move) board - see (applyMoveUnsafe board move) attackerPiece

  where
    x = toCol move
    y = toRow move
    scoreUnderAttack maybeChessPiece attackerX attackerY board = case maybeChessPiece of
                                Just (ChessPiece _ pieceType) -> captureScore pieceType
                                -- when no piece is found and attacker is a pawn, assume it's first capture and it's an en pessant
                                -- otherwise raise sanity error
                                Nothing -> case pieceOnSquare board attackerX attackerY of
                                                Just (ChessPiece _ Pawn) -> captureScore Pawn
                                                _ -> error $ "SEE attempted in invalid position; board: " ++ (boardToFen board) ++ "; position: " ++ (show (x, y))
    see :: ChessBoard -> Maybe ChessPiece -> Int
    see board threatened =
        let threatener = squareThreatenedBy board (otherPlayer (turn board)) x y
        in case (threatened, threatener) of
                    (Just (ChessPiece _ threatenedType), Just (x', y', threatener@(ChessPiece _ threatenerType))) ->
                        let isPromotion = threatenerType == Pawn && (y == 1 || y == 8)
                            captureMove = if isPromotion
                                          then createMove x' y' x y PromoQueen (Just (threatenerType, threatenedType))
                                          else createMove x' y' x y NoPromo (Just (threatenerType, threatenedType))
                            promotionAdjustment = if isPromotion then 800 else 0
                            newBoard = applyMoveUnsafe board captureMove
                            capturedValue = captureScore threatenedType
                        in max 0 (capturedValue + promotionAdjustment - see newBoard (Just threatener))
                    _ -> 0

horizonEval ::  ChessCache -> V.Vector (MB.MutableBuffer Move) -> ChessBoard -> Int -> PositionEval -> PositionEval -> IO PositionEval
horizonEval cache moveBuffers board ply alpha beta =
  {-# SCC "m_horizonEval" #-}
  do
    let buffer = moveBuffers V.! ply
    MB.reset buffer
    MB.pushMany buffer $ pseudoLegalCandidateMoves board
    filterValidMoves buffer
    goodCapturesSize <- sortMoves buffer
    hasCandidates <- (> 0) <$> (liftIO $ MB.bufferSize buffer)
    if not hasCandidates
    then return $ outOfMovesEval board
    else if playerInCheck board
          then
            foldHorizonEval cache moveBuffers board buffer (const True) ply alpha beta
            {-
            {-# SCC "m_horizonEval_incheck" #-}
            let moves = sortMoves $ (filter (\move -> (isJust $ candidateMoveLegal board move)) (pseudoLegalCandidateMoves board))
             in if (null moves)
                  then return $ outOfMovesEval board
                  else foldHorizonEval cache board moves alpha beta
            -}
          else
            do
              setBufferSize buffer goodCapturesSize
              pat <- finalDepthEval cache board
              let alpha' = max alpha pat
              if pat >= beta
                then return pat
                else foldHorizonEval cache moveBuffers board buffer (examineCaptureMove pat) ply alpha' beta
            {-
            {-# SCC "m_horizonEval_not_incheck" #-}
            do
              pat <- finalDepthEval cache board
              let alpha' = max alpha pat
              let capturingMoves = sortMoves $ (filter (\move -> (examineCaptureMove pat move) && (isJust $ candidateMoveLegal board move)) (pseudoLegalCandidateMoves board))
              if pat >= beta
                then return pat
                else foldHorizonEval cache board capturingMoves alpha' beta
            -}
  where
    -- examine only captures when not in check
    -- apply "delta pruning", only consider captures where captured piece + buffer > alpha (buffer recommended to be 200 centipawns)
    -- apply SEE, only consider winning exchanges
    deltaBuffer = 200
    
    {-
    examineCaptureMove :: PositionEval -> Move -> Bool
    examineCaptureMove pat move =
        case getCaptureInfo move of
            Just (_, pieceType) ->
                evalAdd pat ((captureScore pieceType) + deltaBuffer) > alpha
                    && staticExchangeEvalWinning board move >= 0
            _ -> False
    -}
    examineCaptureMove :: PositionEval -> Move -> Bool
    examineCaptureMove pat move =
        case getCaptureInfo move of
            Just (_, pieceType) ->
                evalAdd pat ((captureScore pieceType) + deltaBuffer) > alpha 
            _ -> False

    filterValidMoves moves = do
        ((_, end), _) <- MB.partition (\m -> isJust $ candidateMoveLegal board m) moves
        MB.setBufferSize moves end
        

    sortMoves moves = do
        (captures, _) <- MB.partition (\m -> isJust $ getCaptureInfo m) moves
        (goodCaptures@(_, goodCapturesEnd), badCaptures) <- MB.partition' (\m -> staticExchangeEvalWinning board m >= 0) moves captures
        MB.sort (flip compareMoves) moves goodCaptures
        MB.sort (flip compareMoves) moves badCaptures
        return goodCapturesEnd
      where
        compareMoves move1 move2 =
            let (capturingType1, capturedType1) = fromJust $ getCaptureInfo move1
                (capturingType2, capturedType2) = fromJust $ getCaptureInfo move2
                captureDiffScore1 = captureScore capturedType1 - captureScore capturingType1
                captureDiffScore2 = captureScore capturedType2 - captureScore capturingType2
            in compare captureDiffScore1 captureDiffScore2

    {-
    sortMoves moves =
      let (goodCaptures, badCaptures, other) = partitionAndSortCaptureMoves board moves
       in goodCaptures ++ badCaptures ++ other
    -}

foldHorizonEval :: ChessCache -> V.Vector (MB.MutableBuffer Move) -> ChessBoard -> MB.MutableBuffer Move -> (Move -> Bool) -> Int -> PositionEval -> PositionEval -> IO PositionEval
foldHorizonEval cache moveBuffers board moves examineMove ply alpha beta = do
  result <- runExceptT $ MB.fold foldStep (alpha, beta) moves
  return $ case result of
    Left value -> value
    Right (alpha, _) -> alpha
  where
    foldStep :: (PositionEval, PositionEval) -> Move -> ExceptT PositionEval IO (PositionEval, PositionEval)
    foldStep (alpha, beta) move = 
        if examineMove move
        then do
          let board' = applyMoveUnsafe board move
          value <- liftIO $ negateEval <$> horizonEval cache moveBuffers board' (ply + 1) (negateEval beta) (negateEval alpha)
          let alpha' = max alpha value
          if value >= beta
            then except $ Left value
            else return (alpha', beta)
        else return (alpha, beta)

evaluate'' :: ChessCache -> EvaluateParams -> MB.MutableBuffer Move -> Bool -> App (BestMove, Int, TableValueBound)
evaluate'' cache params@EvaluateParams {alpha, beta, depth, maxDepth, ply, board, nodesParsed, allowNullMove, showUCIInfo, threadIndex, moveBuffers } candidates hasCandidates
  | not hasCandidates =
      let eval = outOfMovesEval board
       in return ((eval, []), nodesParsed, Exact)
  | depth <= 0 = do
      eval <- liftIO $ horizonEval cache moveBuffers board ply alpha beta
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
      -- don't use null move in end game (when side to move has only pawns) to avoig zugzwang
      tryNullMove <-
        if allowNullMove
          && isNullWindow alpha beta
          && depth > 3
          && (not $ playerInCheck board)
          && (quickMaterialCount board (turn board) >= 3)
          then do
            pat <- liftIO $ finalDepthEval cache board
            return $ pat > beta
          else return False
      if tryNullMove
        then do
          (newNodesParsed, isCut) <- evaluateNullMove
          if isCut
            then return ((beta, []), newNodesParsed, LowerBound)
            else pruneOrFoldCandidates
        else pruneOrFoldCandidates
  where
    evaluateNullMove :: App (Int, Bool)
    evaluateNullMove = do
      let params' =
            params
              { allowNullMove = True,
                depth = depth - 3,
                ply = ply + 1,
                board = applyNullMove board,
                alpha = negateEval beta,
                beta = negateEval alpha
              }
      ((moveValue, moves), newNodesParsed) <- do
        ((v, moves), nodes) <- evaluate' cache params'
        return ((negateEval v, moves), nodes)
      return (newNodesParsed, moveValue >= beta)

    pruneOrFoldCandidates :: App ((PositionEval, [Move]), Int, TableValueBound)
    pruneOrFoldCandidates = do
      let futilityMargin = case depth of
                            1 -> Just 100
                            2 -> Just 200
                            3 -> Just 400
                            _ -> Nothing
      case futilityMargin of
        Just m -> do
            staticEval <- liftIO $ finalDepthEval cache board
            let reverseFutilePrune = shouldReversePrune staticEval m
            if reverseFutilePrune
            then return ((staticEval, []), nodesParsed, LowerBound)
            else foldCandidates (Just (staticEval, m))
        Nothing -> foldCandidates Nothing


    shouldReversePrune :: PositionEval -> Int -> Bool
    shouldReversePrune staticEval futilityMargin =
      let isNotInCheck = not $ playerInCheck board
      in if isNotInCheck
          then evalAdd staticEval (- futilityMargin) > beta
          else False

    foldCandidates :: Maybe (PositionEval, Int) -> App ((PositionEval, [Move]), Int, TableValueBound)
    foldCandidates futilityPruneContext = do
      let candidatesFold = CandidatesFold {raisedAlpha = False, bestMoveValue = (PositionEval $ (-10000), []), alpha = alpha, beta = beta, siblingIndex = 0, nodesParsed = nodesParsed, futilityPruneContext = futilityPruneContext }
      result <- runExceptT $ MB.fold foldCandidatesStep' candidatesFold candidates
      return $ case result of
        Left value -> value
        Right CandidatesFold {raisedAlpha, bestMoveValue, nodesParsed, alpha, beta} ->
          if alpha >= beta
            then (bestMoveValue, nodesParsed, LowerBound)
            else (bestMoveValue, nodesParsed, if raisedAlpha then Exact else UpperBound)

    foldCandidatesStep' :: CandidatesFold -> Move -> ExceptT (BestMove, Int, TableValueBound) App CandidatesFold
    foldCandidatesStep' fold move =
        foldCandidatesStep fold (move, applyMoveUnsafe board move)

    foldCandidatesStep :: CandidatesFold -> (Move, ChessBoard) -> ExceptT (BestMove, Int, TableValueBound) App CandidatesFold
    foldCandidatesStep candidatesFold@CandidatesFold {raisedAlpha, bestMoveValue = bestMoveValue, alpha, beta, siblingIndex, nodesParsed, futilityPruneContext } (candidateMove, candidateBoard)
      | alpha >= beta = except $ Left (bestMoveValue, nodesParsed, LowerBound)
      | otherwise =
        if futilePrune futilityPruneContext
        then return candidatesFold {siblingIndex = (siblingIndex + 1)}
        else if tryNullWindow
             then executeNullWindow tryLmr
             else
               if (tryLmr && isNullWindow alpha beta)
                 then executeLmr
                 else executeDefault
      where

        -- extend search by one in case of pawn promotion, pawn reaching second to last rank, or giivng check
        depthExtension =
            let pawnBeforePromo = (if (turn board) == White then 7 else 2) == (toRow candidateMove)
                                    && hasPieceOnSquare candidateBoard (toCol candidateMove) (toRow candidateMove) (ChessPiece (turn board) Pawn)
                pawnPromo = promotion candidateMove /= NoPromo
                givesCheck = playerInCheck candidateBoard
            in if givesCheck || pawnBeforePromo || pawnPromo
               then 1
               else 0

        tryNullWindow = siblingIndex > 0 && not (isNullWindow alpha beta)

        executeNullWindow :: Bool -> ExceptT (BestMove, Int, TableValueBound) App CandidatesFold
        executeNullWindow tryLmr = do
          let nullBeta = case alpha of PositionEval v -> PositionEval (v + 1)
              params' =
                params
                  { depth = depth - (if tryLmr then lmrReduction else 0) - 1 + depthExtension,
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
            else return candidatesFold {siblingIndex = (siblingIndex + 1), nodesParsed = newNodesParsed}

        executeDefault :: ExceptT (BestMove, Int, TableValueBound) App CandidatesFold
        executeDefault = do
          let params' =
                params
                  { depth = depth - 1 + depthExtension,
                    ply = ply + 1,
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha
                  }
          evaluated' <- lift $ evaluate' cache params'
          let (moveValue@(eval, moveLine), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, candidateMove : moves), nodes)
          newBestMoveValue <-
            if (siblingIndex == 0 || eval > alpha)
              then do
                when (ply == 0) $ do
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
                let v = max eval alpha
                 in if (v > alpha)
                      then (v, True)
                      else (alpha, raisedAlpha)
          return candidatesFold {raisedAlpha = raisedAlpha', bestMoveValue = newBestMoveValue, alpha = alpha', siblingIndex = (siblingIndex + 1), nodesParsed = newNodesParsed}

        tryLmr = siblingIndex > 2 && depth > 3
        lmrReduction = if depth < 6 then 1 else (depth `div` 3)

        executeLmr :: ExceptT (BestMove, Int, TableValueBound) App CandidatesFold
        executeLmr = do
          let params' =
                params
                  { depth = depth - lmrReduction - 1 + depthExtension,
                    ply = ply + 1,
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha
                  }
          evaluated' <- lift $ evaluate' cache params'
          let (moveValue@(eval, _), newNodesParsed) = case evaluated' of ((v, moves), nodes) -> ((negateEval v, candidateMove : moves), nodes)
          if eval > alpha
            then -- if found better move, re-evaluate with proper depth
              executeDefault
            else return candidatesFold {siblingIndex = (siblingIndex + 1), nodesParsed = newNodesParsed}

        futilePrune :: Maybe (PositionEval, Int) -> Bool
        futilePrune futilityPruneContext =
          case futilityPruneContext of
            Just (staticEval, futilityMargin) ->
              let isNotInCheck = not $ playerInCheck board
                  doesNotGiveCheck = not $ playerInCheck candidateBoard
                  isNotCapture = isNothing $ getCaptureInfo candidateMove
                  isNotPromo = promotion candidateMove == NoPromo
               in siblingIndex > 0 && (evalAdd staticEval futilityMargin) < alpha && isNotCapture && isNotPromo && isNotInCheck && doesNotGiveCheck
            Nothing -> False

isNullWindow :: PositionEval -> PositionEval -> Bool
isNullWindow (PositionEval alpha) (PositionEval beta) = (beta - alpha) <= 1

evaluateIterationThread :: ChessCache -> V.Vector (MB.MutableBuffer Move) -> ChessBoard -> (PositionEval, [Move]) -> Int -> Bool -> ([(Move, ChessBoard)] -> [(Move, ChessBoard)]) -> Int -> App (BestMove, Int)
evaluateIterationThread cache moveBuffers board lastDepthBest depth showDebug rootBoostCandidateIndex threadIndex =
  do
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
              threadIndex = threadIndex,
              moveBuffers = moveBuffers
            }
    evaluate' cache params

evaluateIteration :: ChessCache -> V.Vector (MB.MutableBuffer Move) -> ChessBoard -> (PositionEval, [Move]) -> Int -> Int -> Bool -> App (BestMove, Int)
evaluateIteration cache moveBuffers board lastDepthBest depth nodes showDebug = do
  env <- ask
  context <- liftIO $ readIORef env
  let threadCount = workerThreadCount context
  -- don't use extra threading for low depth
  let workerThreadCount = if depth > 5 then [2 .. threadCount] else []
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
    makeThreadAction evalResultRef threadCount threadIndex =
      runReaderT
        ( do
            let scrambleCandidates :: [(Move, ChessBoard)] -> [(Move, ChessBoard)]
                scrambleCandidates candidates = case splitAt (threadCount * 2) candidates of
                  (xs, rest) ->
                    if ((threadIndex - 1) * 2) < length xs
                      then (rotate ((threadIndex - 1) * 2) xs) ++ rest
                      else candidates

            evaluateIterationThread cache moveBuffers board lastDepthBest depth showDebug scrambleCandidates threadIndex
        )
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
      moveBuffers <- do
        buffersLst <- liftIO $ mapM (const MB.create) [0..100]
        return $ V.fromList buffersLst
      (_, (eval, moves), _, nodesParsed) <- iterateM' (computeNext moveBuffers) (firstEvaluation moveBuffers) (targetDepth - startingDepth)
      let lastEvalInfo = collectEvaluationInfo (turn board) nodesParsed eval moves
      env <- ask
      result' <- liftIO $ readIORef env
      let result = result' {moves = moves, nodesParsed = nodesParsed, finished = True, evaluation = eval, latestEvaluationInfo = lastEvalInfo}
      liftIO $ writeIORef env result
      return result

    computeNext :: V.Vector (MB.MutableBuffer Move) -> (Int, BestMove, ChessCache, Int) -> App (Int, BestMove, ChessCache, Int)
    computeNext moveBuffers current = do
      let (depth, lastDepthBest, cache, nodesParsed) = current
      env <- ask
      result <- liftIO $ readIORef env
      (thisDepthBest, nodesParsed) <- evaluateIteration cache moveBuffers board lastDepthBest (depth + 1) nodesParsed (showDebug result)
      return $ (depth + 1, thisDepthBest, cache, nodesParsed)
    startingDepth = 1
    firstEvaluation :: V.Vector (MB.MutableBuffer Move) -> App (Int, (PositionEval, [Move]), ChessCache, Int)
    firstEvaluation moveBuffers = do
      cache <- liftIO create
      computeNext moveBuffers ((startingDepth - 1), (PositionEval 0, []), cache, 0)

collectEvaluationInfo :: PlayerColor -> Int -> PositionEval -> [Move] -> [String]
collectEvaluationInfo player nodesParsed (PositionEval value) moves =
  let m = if player == White then 1 else -1
   in [ "info nodes " ++ show nodesParsed,
        "info pv " ++ (intercalate " " (mapMaybe moveToString moves)),
        "info score cp " ++ show (value * m)
      ]
