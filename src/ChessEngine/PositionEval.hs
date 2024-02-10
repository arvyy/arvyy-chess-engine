{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module ChessEngine.PositionEval
  ( PositionEval(..), 
    evaluate,
    EvaluateResult (..),
  )
where

import ChessEngine.Board
import qualified Data.HashMap.Strict as Map
import Data.List (sortBy, partition)
import Data.Maybe (fromJust)
import Data.Foldable
import Data.Ord
import Debug.Trace

newtype PositionEval = PositionEval Float
    deriving (Eq, Show, Ord)

negateEval (PositionEval v) = PositionEval (- v)

whiteWin = PositionEval (1 / 0)
blackWin = PositionEval ((-1) / 0)
draw = PositionEval 0

data TranspositionValue = TranspositionValue PositionEval Int

type TranspositionTable = Map.HashMap ChessBoard TranspositionValue

type CandidateMoveTable = Map.HashMap ChessBoard [(Move, ChessBoard)]

data ChessCache = ChessCache TranspositionTable CandidateMoveTable

putValue :: ChessCache -> ChessBoard -> Int -> PositionEval -> ChessCache
putValue cache@(ChessCache table candidates) board depth value =
    case Map.lookup board table of
        Just (TranspositionValue eval depth') ->
            if depth > depth'
            then ChessCache (Map.insert board (TranspositionValue value depth) table) candidates
            else cache
        Nothing -> ChessCache (Map.insert board (TranspositionValue value depth) table) candidates

getValue :: ChessCache -> ChessBoard -> Maybe TranspositionValue
getValue (ChessCache table _) board = Map.lookup board table

putCandidates :: ChessCache -> ChessBoard -> [(Move, ChessBoard)] -> ChessCache
putCandidates cache@(ChessCache table candidates) board moves =
    ChessCache table (Map.insert board moves candidates)

getCandidates :: ChessCache -> ChessBoard -> Maybe [(Move, ChessBoard)]
getCandidates (ChessCache _ candidates) board = Map.lookup board candidates

create :: ChessCache
create = ChessCache Map.empty Map.empty

finalDepthEval :: ChessBoard -> PositionEval
finalDepthEval board =
  PositionEval $ foldl' (\score piece -> score + scorePiece piece) 0 $ boardPositions board
  where
    pieceMul :: PlayerColor -> Float
    pieceMul color = if color == White then 1 else -1

    scorePiece :: (Int, Int, ChessPiece) -> Float
    scorePiece (_, _, ChessPiece player King) = 0
    scorePiece piece@(_, _, ChessPiece player Queen) = (9 + scorePieceThreats board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Bishop) = (3 + scorePieceThreats board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Horse) = (3 + scorePieceThreats board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Rock) = (5 + scorePieceThreats board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Pawn) = (1 + scorePieceThreats board piece) * pieceMul player

    scorePieceThreats :: ChessBoard -> (Int, Int, ChessPiece) -> Float
    scorePieceThreats board piece =
        let isOwnSide y = case piece of
                (_, _, ChessPiece White _) -> y < 5
                _ -> y > 4
            (ownSide, opponentSide) = foldl'
                (\(own, opponent) (_, y) -> if isOwnSide y then (own + 1.0, opponent) else (own, opponent + 1.0))
                (0.0, 0.0)
                (pieceThreats board piece)
        in log (ownSide + 1.0) * 0.2 + log (opponentSide + 1.0) * 0.5



-- end of the game
outOfMovesEval :: ChessBoard -> PositionEval
outOfMovesEval board =
  let player = (turn board)
      inCheck = playerInCheck board player
   in if (not inCheck)
        then draw
        else
          if (player == White)
            then blackWin
            else whiteWin

data EvaluateParams = EvaluateParams
  { cache :: !ChessCache,
    moves :: ![Move],
    firstChoice :: ![Move],
    alpha :: !PositionEval,
    beta :: !PositionEval,
    depth :: !Int,
    maxDepth :: !Int,
    board :: !ChessBoard,
    nodesParsed :: !Int,
    currentBest :: !(PositionEval, [Move])
  }

instance Show EvaluateParams where
    show EvaluateParams{ cache = (ChessCache table candidates), depth, maxDepth} =
        "Transposition cache size: " ++ (show (Map.size table)) ++ ", candidates cache size: " ++ (show (Map.size candidates)) ++ " , depth: " ++ (show depth) ++ ", maxdepth: " ++ (show maxDepth)

data EvaluateResult = EvaluateResult
  { nodesParsed :: !Int,
    finished :: !Bool,
    evaluation :: !PositionEval,
    moves :: ![Move],
    continuation :: EvaluateResult
  } deriving Show

evaluate' :: EvaluateParams -> ((PositionEval, [Move]), ChessCache, Int)
evaluate' params@EvaluateParams {cache, board} =
    let (candidates, newParams) = case getCandidates cache board of
                                    Just moves -> (moves, params)
                                    Nothing -> let candidates' = candidateMoves board
                                               in (candidates', params { cache = putCandidates cache board candidates' })
        result = evaluate'' newParams candidates
    in result

evaluate'' :: EvaluateParams -> [(Move, ChessBoard)] -> ((PositionEval, [Move]), ChessCache, Int)
evaluate'' params@EvaluateParams {cache, moves, firstChoice, alpha, beta, depth, maxDepth, board, nodesParsed} unsortedCandidates
  | Just (TranspositionValue eval cachedDepth) <- getValue cache board
  , cachedDepth >= depth = ((eval, moves), cache, nodesParsed)
  | null candidates =
      let
        !eval = outOfMovesEval board
        !cache' = putValue cache board depth eval
        !nodesParsed' = nodesParsed + 1
      in ((eval, moves), cache', nodesParsed')
  | depth <= 0 =
      let
        !eval = performHorizonEval
        !cache' = putValue cache board 0 eval
        !nodesParsed' = nodesParsed + 1
      in ((eval, moves), cache', nodesParsed')
  | otherwise = foldCandidates cache candidates alpha beta
  where
    
    performHorizonEval =
        if (turn board) == White
        then horizonEval 3 board alpha beta
        else negateEval (horizonEval 3 board (negateEval beta) (negateEval alpha))
        

    horizonEval depth board alpha beta
        | depth <= 0 = finalDepthEval board
        | otherwise =
            let pat = finalDepthEval board
                alpha' = max alpha pat
                capturingMoves = (filter (\(move, board') -> isCaptureMove board move) (candidateMoves board))
            in if pat >= beta
               then beta
               else foldHorizonEval depth capturingMoves alpha' beta

    foldHorizonEval depth ((move, board'):rest) alpha beta =
        let value = negateEval (horizonEval (depth - 1) board' (negateEval beta) (negateEval alpha))
            alpha' = max alpha value
        in if value >= beta
           then beta
           else foldHorizonEval depth rest alpha' beta
                
    foldHorizonEval _ [] alpha _ = alpha


    foldCandidates :: ChessCache -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> ((PositionEval, [Move]), ChessCache, Int)
    foldCandidates cache candidates alpha beta =
      if (turn board) == White
      then foldCandidatesWhite cache True (blackWin, []) candidates alpha beta 0 True nodesParsed
      else foldCandidatesBlack cache True (whiteWin, []) candidates alpha beta 0 True nodesParsed

    foldCandidatesWhite = foldCandidatesHelper (>) (\a b new -> (new, b)) (\a@(PositionEval v) b -> (a, PositionEval (v + 0.001)))
    foldCandidatesBlack = foldCandidatesHelper (<) (\a b new -> (a, new)) (\a b@(PositionEval v) -> (PositionEval (v - 0.001), b))
    foldCandidatesHelper isBetter getNewAlphaBeta makeNullAlphaBetaWindow cache first bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex allowLMR nodesParsed
        | alpha >= beta = (bestMoveValue, putValue cache board depth (fst bestMoveValue), nodesParsed)

          -- if this is 3rd+ candidate move under consideration in a depth of 3+ from start,
          -- evaluate with reduced depth (LMR).
        | siblingIndex > 1 && (maxDepth - depth) > 1 && allowLMR && not (isCaptureMove board candidateMove) =
          let newMovesPath = moves ++ [candidateMove]
              (moveValue, newCache, newNodesParsed) = evaluate' params {
                moves = newMovesPath,
                firstChoice = (followUpFirstChoice candidateMove),
                cache = cache,
                depth = (depth - 2), -- subtract 2 to lower depth search in fringe node
                board = candidateBoard,
                nodesParsed = nodesParsed,
                alpha = alpha,
                beta = beta }
          in
            if isBetter (fst moveValue) (fst bestMoveValue)
            -- if found better move, re-evaluate with proper depth
            then (foldCandidatesHelper isBetter getNewAlphaBeta makeNullAlphaBetaWindow newCache False bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex False newNodesParsed)
            else (foldCandidatesHelper isBetter getNewAlphaBeta makeNullAlphaBetaWindow newCache False bestMoveValue restCandidates alpha beta (siblingIndex + 1) True newNodesParsed)

        | otherwise = 
          let newMovesPath = moves ++ [candidateMove]
              params' =  params {
                    moves = newMovesPath,
                    firstChoice = (followUpFirstChoice candidateMove),
                    cache = cache,
                    depth = (depth - 1),
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = alpha,
                    beta = beta }
              (moveValue, newCache, newNodesParsed) = evaluate' params'
              newBestMoveValue =
                if (isBetter (fst moveValue) (fst bestMoveValue))
                  then moveValue
                  else bestMoveValue
              (newAlpha, newBeta) = getNewAlphaBeta alpha beta (fst newBestMoveValue)
              fullSearchContinuation = foldCandidatesHelper isBetter getNewAlphaBeta makeNullAlphaBetaWindow newCache False newBestMoveValue restCandidates newAlpha newBeta (siblingIndex + 1) True newNodesParsed
          in if first
             then fullSearchContinuation
             -- not a first move: try using null-window pruning
             else let (nullAlpha, nullBeta) = makeNullAlphaBetaWindow alpha beta
                      params' =  params {
                            moves = newMovesPath,
                            firstChoice = (followUpFirstChoice candidateMove),
                            cache = cache,
                            depth = (depth - 1),
                            board = candidateBoard,
                            nodesParsed = nodesParsed,
                            alpha = nullAlpha,
                            beta = nullBeta }
                      (moveValue, _, _) = evaluate' params'
                  in if isBetter (fst moveValue) (fst bestMoveValue)
                     -- found better with reduced window: do full search
                     then (fullSearchContinuation)
                     -- cut off
                     else (foldCandidatesHelper isBetter getNewAlphaBeta makeNullAlphaBetaWindow newCache False bestMoveValue restCandidates alpha beta (siblingIndex + 1) True newNodesParsed)

    foldCandidatesHelper _ _ _ cache _ bestMoveValue [] _ _ _ _ nodesParsed = (bestMoveValue, putValue cache board depth (fst bestMoveValue), nodesParsed + 1)

    candidates = 
      let newCandidatesList = case firstChoice of
            [] -> sortCandidates unsortedCandidates (turn board)
            (m : _) -> case partition (\c -> m == (fst c)) unsortedCandidates of
              ([], _) -> sortCandidates unsortedCandidates (turn board)
              ((candidate : _), others) -> candidate : (sortCandidates others (turn board))
       in newCandidatesList

    sortCandidates lst player =
        let extractValue board = do
                (TranspositionValue eval depth) <- getValue cache board
                return eval
            augmentWithScore (move, board) = (move, board, extractValue board)
            removeScore (move, board, score) = (move, board)
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
            debugPartition = partition (\(_, _, score) -> case score of
                Just _ -> True
                Nothing -> False) 
                (fmap augmentWithScore lst)
            debugLog = show (length (fst debugPartition)) ++ ", " ++ show (length (snd debugPartition))
            sortedCandidates = fmap removeScore (sortBy comparator (fmap augmentWithScore lst))
            --sortedCandidates' = if depth > 1 then (trace debugLog sortedCandidates) else sortedCandidates
        in sortedCandidates

    followUpFirstChoice :: Move -> [Move]
    followUpFirstChoice move =
      case firstChoice of
        [] -> []
        (m : rest) -> if m == move then rest else []

evaluateIteration :: ChessCache -> ChessBoard -> (PositionEval, [Move]) -> Int -> ((PositionEval, [Move]), ChessCache, Int)
evaluateIteration cache board lastDepthBest depth =
  let (lastEval, firstChoice) = lastDepthBest
      params =
        EvaluateParams
          { cache = cache,
            moves = [],
            firstChoice = firstChoice,
            alpha = blackWin,
            beta = whiteWin,
            depth = depth,
            maxDepth = depth,
            board = board,
            nodesParsed = 0,
            currentBest = lastDepthBest
          }
  in evaluate' params --(trace (show params) params)

evaluate :: ChessBoard -> Int -> EvaluateResult
evaluate board finalDepth =
  let
    (_, (eval, moves), _, nodesParsed) = iterate computeNext firstEvaluation !! (finalDepth - startingDepth)
    result =
        EvaluateResult
          { moves = moves,
            nodesParsed = nodesParsed,
            finished = True,
            evaluation = eval,
            continuation = result
          }
  in result
  where
    computeNext :: (Int, (PositionEval, [Move]), ChessCache, Int) -> (Int, (PositionEval, [Move]), ChessCache, Int)
    computeNext current =
        let (depth, lastDepthBest, cache, _) = current
            (thisDepthBest, newCache', nodesParsed) = evaluateIteration cache board lastDepthBest (depth + 1)
        in (depth + 1, thisDepthBest, newCache', nodesParsed)
    startingDepth = 2
    firstEvaluation :: (Int, (PositionEval, [Move]), ChessCache, Int)
    firstEvaluation = computeNext ((startingDepth - 1), (draw, []), create, 0)
