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
import ChessEngine.Heatmaps
import qualified Data.HashMap.Strict as Map
import Data.List (sortBy, partition)
import Data.Maybe (fromJust)
import Data.Foldable
import Data.Ord
import Debug.Trace

newtype PositionEval = PositionEval Float
    deriving (Eq, Show, Ord)

negateEval (PositionEval v) = PositionEval (- v)

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

-- returns current value as negamax (ie, score is multipled for -1 if current player is black)
finalDepthEval :: ChessBoard -> PositionEval
finalDepthEval board =
    let score = foldl' (\score piece -> score + scorePiece piece) 0 $ boardPositions board
    in PositionEval score
  where
    pieceMul color = if color == (turn board) then 1 else -1

    scorePiece :: (Int, Int, ChessPiece) -> Float
    scorePiece piece@(_, _, ChessPiece player King) = (0 + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Queen) = (9 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Bishop) = (3 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Horse) = (3 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Rock) = (5 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player
    scorePiece piece@(_, _, ChessPiece player Pawn) = (1 + scorePieceThreats board piece + scorePiecePosition board piece) * pieceMul player

    scorePieceThreats :: ChessBoard -> (Int, Int, ChessPiece) -> Float
    scorePieceThreats board piece =
        let isOwnSide y = case piece of
                (_, _, ChessPiece White _) -> y < 5
                _ -> y > 4
            (ownSide, opponentSide) = foldl'
                (\(own, opponent) (_, y) -> if isOwnSide y then (own + 1.0, opponent) else (own, opponent + 1.0))
                (0.0, 0.0)
                (pieceThreats board piece)
            typeMultiplier = case piece of
                    -- due to queen range, it needs reduced reward otherwise bot is very eager to play with queen
                    -- without developing other pieces
                    (_, _, ChessPiece _ Queen) -> 0.1
                    _ -> 1.0
        in log (ownSide + 1.0) * 0.2 + log (opponentSide + 1.0) * 0.3

    scorePiecePosition :: ChessBoard -> (Int, Int, ChessPiece) -> Float
    scorePiecePosition board (x, y, piece@(ChessPiece _ pieceType)) =
        let squareRating = piecePositionBonus x y piece -- 0. - 1. rating, which needs to be first curved and then mapped onto range
            maxBonus = case pieceType of
                Pawn -> 0.5
                King -> 2.0
                Bishop -> 1.5
                Horse -> 1.5
                Rock -> 1.0
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
  { cache :: !ChessCache,
    moves :: ![Move],
    firstChoice :: ![Move],
    alpha :: !PositionEval,
    beta :: !PositionEval,
    depth :: !Int,
    maxDepth :: !Int,
    board :: !ChessBoard,
    nodesParsed :: !Int,
    currentBest :: !(PositionEval, [Move]),
    allowNullMove :: !Bool
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
evaluate'' params@EvaluateParams {cache, moves, firstChoice, alpha, beta, depth, maxDepth, board, nodesParsed, allowNullMove} unsortedCandidates
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
        !eval =  horizonEval 3 board alpha beta
        !cache' = putValue cache board 0 eval
        !nodesParsed' = nodesParsed + 1
      in ((eval, moves), cache', nodesParsed')
  | otherwise = foldCandidates cache candidates alpha beta
  where

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
        foldCandidates' cache True (PositionEval $ (-1) / 0, []) candidates alpha beta 0 (False, False, False) nodesParsed

    foldCandidates' cache first bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, nullWindowTried) nodesParsed

        | alpha >= beta = (bestMoveValue, putValue cache board depth (fst bestMoveValue), nodesParsed)

          -- try null move if there is sufficient depth left & null move is allowed (ie., wasn't done on previous move)
        | not nullMoveTried && allowNullMove && depth > 3 =
          let params' = params {
                allowNullMove = False,
                moves = [],
                firstChoice = [],
                cache = cache,
                depth = depth - 3, -- R = 2
                board = candidateBoard { turn = if (turn board) == White then Black else White },
                nodesParsed = nodesParsed,
                alpha = negateEval beta,
                beta = negateEval alpha }
              (moveValue, _, _) = case evaluate' params' of ((v, moves), cache, nodes) -> ((negateEval v, moves), cache, nodes)
          in
            if (fst moveValue) > beta
            -- passing up move still causes it to exceed beta -- cut off
            then ((beta, moves ++ [candidateMove]), putValue cache board depth beta, nodesParsed)
            else (foldCandidates' cache first bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (True, lmrTried, nullWindowTried) nodesParsed)
          
          -- if this is 3rd+ candidate move under consideration in a depth of 3+ from start,
          -- evaluate with reduced depth (LMR).
        | not lmrTried && siblingIndex > 1 && (maxDepth - depth) > 1 && not (isCaptureMove board candidateMove) =
          let newMovesPath = moves ++ [candidateMove]
              params' = params {
                allowNullMove = True,
                moves = newMovesPath,
                firstChoice = (followUpFirstChoice candidateMove),
                cache = cache,
                depth = (depth - 2), -- subtract 2 to lower depth search in fringe node
                board = candidateBoard,
                nodesParsed = nodesParsed,
                alpha = negateEval beta,
                beta = negateEval alpha }
              (moveValue, newCache, newNodesParsed) = case evaluate' params' of ((v, moves), cache, nodes) -> ((negateEval v, moves), cache, nodes)
          in
            if (fst moveValue) > (fst bestMoveValue)
            -- if found better move, re-evaluate with proper depth
            then (foldCandidates' newCache False bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, True, nullWindowTried) newNodesParsed)
            else (foldCandidates' newCache False bestMoveValue restCandidates alpha beta (siblingIndex + 1) (False, False, False) newNodesParsed)
        | (not nullWindowTried) && not first =
          let nullBeta = case alpha of PositionEval v -> PositionEval (v + 0.0001)
              newMovesPath = moves ++ [candidateMove]
              params' =  params {
                    allowNullMove = True,
                    moves = newMovesPath,
                    firstChoice = (followUpFirstChoice candidateMove),
                    cache = cache,
                    depth = (depth - 1),
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval nullBeta,
                    beta = negateEval alpha }
              moveValue = case evaluate' params' of ((v, moves), _, _) -> (negateEval v, moves)
          in if (fst moveValue) > (fst bestMoveValue)
             -- found better with reduced window: do full search
             then (foldCandidates' cache False bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta siblingIndex (nullMoveTried, lmrTried, True) nodesParsed)
             else (foldCandidates' cache False bestMoveValue restCandidates alpha beta (siblingIndex + 1) (False, False, False) nodesParsed)
        | otherwise = 
          let newMovesPath = moves ++ [candidateMove]
              params' =  params {
                    allowNullMove = True,
                    moves = newMovesPath,
                    firstChoice = (followUpFirstChoice candidateMove),
                    cache = cache,
                    depth = (depth - 1),
                    board = candidateBoard,
                    nodesParsed = nodesParsed,
                    alpha = negateEval beta,
                    beta = negateEval alpha }
              (moveValue, newCache, newNodesParsed) = case evaluate' params' of ((v, moves), cache, nodes) -> ((negateEval v, moves), cache, nodes)
              newBestMoveValue =
                if (first || (fst moveValue) > (fst bestMoveValue))
                  then moveValue
                  else bestMoveValue
              newAlpha = max alpha (fst newBestMoveValue)
          in foldCandidates' newCache False newBestMoveValue restCandidates newAlpha beta (siblingIndex + 1) (False, False, False) newNodesParsed

    foldCandidates' cache _ bestMoveValue [] _ _ _ _ nodesParsed = (bestMoveValue, putValue cache board depth (fst bestMoveValue), nodesParsed + 1)

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
            alpha = PositionEval $ (-1) / 0,
            beta = PositionEval $ 1 / 0,
            depth = depth,
            maxDepth = depth,
            board = board,
            nodesParsed = 0,
            currentBest = lastDepthBest,
            allowNullMove = True
          }
  in evaluate' params --(trace (show params) params)

evaluate :: ChessBoard -> Int -> EvaluateResult
evaluate board targetDepth =
  let
    candidateCount = length $ candidateMoves board
    finalDepth = if candidateCount >= 20
                 then targetDepth
                 else targetDepth + 1
    (_, (eval, moves), _, nodesParsed) = iterate computeNext firstEvaluation !! (finalDepth - startingDepth)
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
    computeNext :: (Int, (PositionEval, [Move]), ChessCache, Int) -> (Int, (PositionEval, [Move]), ChessCache, Int)
    computeNext current =
        let (depth, lastDepthBest, cache, _) = current
            (thisDepthBest, newCache', nodesParsed) = evaluateIteration cache board lastDepthBest (depth + 1)
        in (depth + 1, thisDepthBest, newCache', nodesParsed)
    startingDepth = 2
    firstEvaluation :: (Int, (PositionEval, [Move]), ChessCache, Int)
    firstEvaluation = computeNext ((startingDepth - 1), (PositionEval 0, []), create, 0)
