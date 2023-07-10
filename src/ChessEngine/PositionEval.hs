{-# LANGUAGE BangPatterns #-}
module ChessEngine.PositionEval(
    PositionEval,
    evaluate
)
where

import ChessEngine.Board
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace

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
        scorePiece (_, _, ChessPiece player Queen) = 9 * pieceMul player
        scorePiece (_, _, ChessPiece player Bishop) = 3 * pieceMul player
        scorePiece (_, _, ChessPiece player Horse) = 3 * pieceMul player
        scorePiece (_, _, ChessPiece player Rock) = 5 * pieceMul player
        scorePiece (_, _, ChessPiece player Pawn) = 1 * pieceMul player


-- end of the game
outOfMovesEval :: ChessBoard -> PositionEval
outOfMovesEval board =
    let player = (turn board)
        inCheck = playerInCheck board player
    in if (not inCheck)
       then Draw
       else if (player == White)
       then BlackWin
       else WhiteWin

evaluate' :: BoardCache -> [Move] -> [Move] -> PositionEval -> PositionEval -> Int -> ChessBoard -> ((PositionEval, [Move]), BoardCache)
evaluate' cache moves firstChoice alpha beta depth board
    | Just cachedValue <- Map.lookup board cache = (cachedValue, cache)
    | (null candidates) =
        let eval = (outOfMovesEval board, moves)
        in (eval, Map.insert board eval cache)
    | depth == 0 = 
        let eval = (finalDepthEval board, moves)
        in (eval, Map.insert board eval cache)
    | otherwise = foldCandidates cache candidates alpha beta
    where foldCandidates :: BoardCache -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> ((PositionEval, [Move]), BoardCache)
          foldCandidates cache candidates alpha beta = if (turn board) == White
                                                       then foldCandidatesWhite cache True (BlackWin, []) candidates alpha beta
                                                       else foldCandidatesBlack cache True (WhiteWin, []) candidates alpha beta

          foldCandidatesWhite :: BoardCache -> Bool -> (PositionEval, [Move]) -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> ((PositionEval, [Move]), BoardCache)
          foldCandidatesWhite cache first bestMoveValue ((candidateMove, candidateBoard):restCandidates) alpha beta = 
            if alpha >= beta
            then (bestMoveValue, Map.insert board bestMoveValue cache)
            else let newMovesPath = moves ++ [candidateMove]
                     (moveValue, newCache) = evaluate' cache newMovesPath (followUpFirstChoice candidateMove) alpha beta (depth - 1) candidateBoard
                     newBestMoveValue = if (first || compare (fst bestMoveValue) (fst moveValue) == LT)
                                        then moveValue
                                        else bestMoveValue
                 in foldCandidatesWhite newCache False newBestMoveValue restCandidates (fst newBestMoveValue) beta
          foldCandidatesWhite cache _ bestMoveValue [] _ _ =  (bestMoveValue, Map.insert board bestMoveValue cache)

          foldCandidatesBlack :: BoardCache -> Bool -> (PositionEval, [Move]) -> [(Move, ChessBoard)] -> PositionEval -> PositionEval -> ((PositionEval, [Move]), BoardCache)
          foldCandidatesBlack cache first bestMoveValue ((candidateMove, candidateBoard):restCandidates) alpha beta = 
            if alpha >= beta
            then (bestMoveValue, Map.insert board bestMoveValue cache)
            else let newMovesPath = moves ++ [candidateMove]
                     (moveValue, newCache) = evaluate' cache newMovesPath (followUpFirstChoice candidateMove) alpha beta (depth - 1) candidateBoard
                     newBestMoveValue = if (first || compare (fst bestMoveValue) (fst moveValue) == GT)
                                        then moveValue
                                        else bestMoveValue
                 in foldCandidatesBlack newCache False newBestMoveValue restCandidates alpha (fst newBestMoveValue)
          foldCandidatesBlack cache _ bestMoveValue [] _ _ =  (bestMoveValue, Map.insert board bestMoveValue cache)

          candidates = let candidatesList = candidateMoves board
                           newCandidatesList = case firstChoice of
                                                   [] -> candidatesList
                                                   (m:_) -> case filter (\c -> m == (fst c)) candidatesList of
                                                                [] -> candidatesList
                                                                (candidate:_) -> candidate:candidatesList
                       in newCandidatesList

          followUpFirstChoice :: Move -> [Move]
          followUpFirstChoice move = 
            case firstChoice of
                [] -> []
                (m:rest) -> if m == move then rest else []

evaluateIteration :: ChessBoard -> [Move] -> Int -> (PositionEval, [Move])
evaluateIteration board firstChoice depth =
    let (eval, cache) = evaluate' Map.empty [] firstChoice BlackWin WhiteWin depth board
    in eval
            
evaluate :: ChessBoard -> (Int, PositionEval, [Move])
evaluate board = 
    iterate computeNext firstEvaluation !! (finalDepth - startingDepth)
    where computeNext :: (Int, PositionEval, [Move]) -> (Int, PositionEval, [Move])
          computeNext (depth, posEval, bestMove) = 
              let !(nextPosEval, nextBestMove) = trace ("Running iteration for depth " ++ (show (depth + 1))) evaluateIteration board bestMove (depth + 1)
              in (depth + 1, nextPosEval, nextBestMove)
          startingDepth = 4
          finalDepth = 11
          firstEvaluation :: (Int, PositionEval, [Move])
          firstEvaluation = computeNext ((startingDepth - 1), Draw, [])
