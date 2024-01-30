{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module ChessEngine.PositionEval
  ( PositionEval,
    evaluate,
    EvaluateResult (..),
  )
where

import ChessEngine.Board
import Control.Monad.Trans.Cont
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type BoardCache = Map.Map ChessBoard (PositionEval, [Move])

data PositionEval = Draw | WhiteWin | BlackWin | Score !Float deriving (Eq, Show)

instance Ord PositionEval where
  compare Draw Draw = EQ
  compare Draw WhiteWin = LT
  compare Draw BlackWin = GT
  compare Draw (Score v) = if v < 0 then GT else if v == 0 then EQ else LT
  compare WhiteWin WhiteWin = EQ
  compare WhiteWin _ = GT
  compare BlackWin BlackWin = EQ
  compare BlackWin _ = LT
  compare (Score a) (Score b) = compare a b
  compare (Score v) Draw = if v < 0 then LT else if v == 0 then EQ else GT
  compare (Score v) WhiteWin = LT
  compare (Score v) BlackWin = GT

finalDepthEval :: ChessBoard -> PositionEval
finalDepthEval board =
  Score $ foldr (\piece score -> score + scorePiece piece) 0 $ boardPositions board
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
            (ownSide, opponentSide) = foldr 
                (\(_, y) (own, opponent) -> if isOwnSide y then (own + 1.0, opponent) else (own, opponent + 1.0))
                (0.0, 0.0)
                (pieceThreats board piece)
        in log (ownSide + 1.0) * 0.2 + log (opponentSide + 1.0) * 0.5
            


-- end of the game
outOfMovesEval :: ChessBoard -> PositionEval
outOfMovesEval board =
  let player = (turn board)
      inCheck = playerInCheck board player
   in if (not inCheck)
        then Draw
        else
          if (player == White)
            then BlackWin
            else WhiteWin

data EvaluateParams = EvaluateParams
  { cache :: !BoardCache,
    moves :: ![Move],
    firstChoice :: ![Move],
    alpha :: !PositionEval,
    beta :: !PositionEval,
    depth :: !Int,
    board :: !ChessBoard,
    nodesParsed :: !Int,
    currentBest :: !(PositionEval, [Move])
  }

data EvaluateResult = EvaluateResult
  { nodesParsed :: !Int,
    finished :: !Bool,
    evaluation :: !PositionEval,
    moves :: ![Move],
    continuation :: EvaluateResult
  } deriving Show

evaluate' :: EvaluateParams -> Cont EvaluateResult ((PositionEval, [Move]), BoardCache, Int)
evaluate' params = do
  if depth `mod` 1000 == 0
    then shift $ \k ->
      return
        EvaluateResult
          { nodesParsed = nodesParsed,
            finished = False,
            evaluation = (fst currentBest),
            moves = (snd currentBest),
            continuation = (k ())
          }
    else return ()
  resumeEvaluate' params
  where
    EvaluateParams {depth = depth, nodesParsed = nodesParsed, cache = cache, currentBest = currentBest} = params

-- TODO store cache hits; periodically clean up entries without hits
resumeEvaluate' :: EvaluateParams -> Cont EvaluateResult ((PositionEval, [Move]), BoardCache, Int)
resumeEvaluate' params@EvaluateParams {cache, moves, firstChoice, alpha, beta, depth, board, nodesParsed}
  | Just cachedValue <- Map.lookup board cache = return (cachedValue, cache, nodesParsed + 1)
  | null candidates =
      let eval = (outOfMovesEval board, moves)
       in return (eval, {- Map.insert board eval -} cache, nodesParsed + 1)
  | depth == 0 =
      let eval = (finalDepthEval board, moves)
       in return (eval, {- Map.insert board eval -} cache, nodesParsed + 1)
  | otherwise = foldCandidates cache candidates alpha beta
  where
    foldCandidates :: BoardCache -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> Cont EvaluateResult ((PositionEval, [Move]), BoardCache, Int)
    foldCandidates cache candidates alpha beta =
      if (turn board) == White
        then foldCandidatesWhite nodesParsed cache True (BlackWin, []) candidates alpha beta
        else foldCandidatesBlack nodesParsed cache True (WhiteWin, []) candidates alpha beta

    foldCandidatesWhite :: Int -> BoardCache -> Bool -> (PositionEval, [Move]) -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> Cont EvaluateResult ((PositionEval, [Move]), BoardCache, Int)
    foldCandidatesWhite nodesParsed cache first bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta =
      if alpha >= beta
        then return (bestMoveValue, Map.insert board bestMoveValue cache, nodesParsed)
        else do
          let newMovesPath = moves ++ [candidateMove]
          (moveValue, newCache, newNodesParsed) <- evaluate' params {
            moves = newMovesPath, 
            firstChoice = (followUpFirstChoice candidateMove),
            depth = (depth - 1), 
            board = candidateBoard, 
            nodesParsed = nodesParsed,
            alpha = alpha,
            beta = beta }
          let newBestMoveValue =
                if (first || compare (fst bestMoveValue) (fst moveValue) == LT)
                  then moveValue
                  else bestMoveValue
          foldCandidatesWhite newNodesParsed newCache False newBestMoveValue restCandidates (fst newBestMoveValue) beta
    foldCandidatesWhite nodesParsed cache _ bestMoveValue [] _ _ = return (bestMoveValue, Map.insert board bestMoveValue cache, nodesParsed)

    foldCandidatesBlack :: Int -> BoardCache -> Bool -> (PositionEval, [Move]) -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> Cont EvaluateResult ((PositionEval, [Move]), BoardCache, Int)
    foldCandidatesBlack nodesParsed cache first bestMoveValue ((candidateMove, candidateBoard) : restCandidates) alpha beta =
      if alpha >= beta
        then return (bestMoveValue, Map.insert board bestMoveValue cache, nodesParsed)
        else do
          let newMovesPath = moves ++ [candidateMove]
          (moveValue, newCache, newNodesParsed) <- evaluate' params {
            moves = newMovesPath, 
            firstChoice = (followUpFirstChoice candidateMove), 
            depth = (depth - 1), 
            board = candidateBoard, 
            nodesParsed = nodesParsed,
            alpha = alpha,
            beta = beta }
          let newBestMoveValue =
                if (first || compare (fst bestMoveValue) (fst moveValue) == GT)
                  then moveValue
                  else bestMoveValue
          foldCandidatesBlack newNodesParsed newCache False newBestMoveValue restCandidates alpha (fst newBestMoveValue)
    foldCandidatesBlack nodesParsed cache _ bestMoveValue [] _ _ = return (bestMoveValue, Map.insert board bestMoveValue cache, nodesParsed)

    candidates =
      let candidatesList = candidateMoves board
          newCandidatesList = case firstChoice of
            [] -> candidatesList
            (m : _) -> case filter (\c -> m == (fst c)) candidatesList of
              [] -> candidatesList
              (candidate : _) -> candidate : candidatesList
       in newCandidatesList

    followUpFirstChoice :: Move -> [Move]
    followUpFirstChoice move =
      case firstChoice of
        [] -> []
        (m : rest) -> if m == move then rest else []

evaluateIteration :: ChessBoard -> (PositionEval, [Move]) -> Int -> Cont EvaluateResult ((PositionEval, [Move]), Int)
evaluateIteration board lastDepthBest depth = do
  let (lastEval, firstChoice) = lastDepthBest
  let params =
        EvaluateParams
          { cache = Map.empty,
            moves = [],
            firstChoice = firstChoice,
            alpha = BlackWin,
            beta = WhiteWin,
            depth = depth,
            board = board,
            nodesParsed = 0,
            currentBest = lastDepthBest
          }
  (best, cache, nodesParsed) <- evaluate' params
  return (best, nodesParsed)

evaluate :: ChessBoard -> Int -> EvaluateResult
evaluate board finalDepth = evalCont . reset $ do
  (_, (eval, moves), nodesParsed) <- iterate computeNext firstEvaluation !! (finalDepth - startingDepth)
  let result =
        EvaluateResult
          { moves = moves,
            nodesParsed = nodesParsed,
            finished = True,
            evaluation = eval,
            continuation = result
          }
  return result
  where
    computeNext :: Cont EvaluateResult (Int, (PositionEval, [Move]), Int) -> Cont EvaluateResult (Int, (PositionEval, [Move]), Int)
    computeNext current = do
      (depth, lastDepthBest, _) <- current
      (thisDepthBest, nodesParsed) <- evaluateIteration board lastDepthBest (depth + 1)
      return (depth + 1, thisDepthBest, nodesParsed)
    startingDepth = 3
    firstEvaluation :: Cont EvaluateResult (Int, (PositionEval, [Move]), Int)
    firstEvaluation = computeNext (return ((startingDepth - 1), (Draw, []), 0))
