{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import ChessEngine.Board
import ChessEngine.PositionEval
import ChessEngine.PrecomputedCandidateMoves
import ChessEngine.UCI
import Data.Maybe
import Debug.Trace
import Test.QuickCheck (allProperties)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.List.Fusion.Probe (fuseThis)

candidateMoves board = mapMaybe mapper $ pseudoLegalCandidateMoves board
  where
    mapper move = do
      board' <- candidateMoveLegal board move
      return (move, board')

-- makes random valid move or returns itself if no moves available
applyRandomMove :: ChessBoard -> Gen ChessBoard
applyRandomMove board =
  let moves :: [(Move, ChessBoard)]
      moves = candidateMoves board
      boards = map snd moves
   in if (null boards)
        then return board
        else fmap (\index -> boards !! index) $ chooseInt (0, (length boards) - 1)

instance Arbitrary ChessBoard where
  arbitrary = do
    moveCount <- chooseInt (0, 100)
    iterate (>>= applyRandomMove) (pure initialBoard) !! moveCount

-- PROPERTY TESTS -------------------------------------

prop_changeturn =
  \board ->
    all
      ( \candidate -> case candidate of
          (move, board') -> (turn board) /= (turn board')
      )
      $ candidateMoves board

prop_fenexport =
  \board ->
    let Just (recreatedBoard, _) = loadFen $ boardToFen board
     in (pieces board) === (pieces recreatedBoard)
          .&&. (turn board) === (turn recreatedBoard)
          .&&. (enPassant board) === (enPassant recreatedBoard)
          .&&. (whiteKingCastle board) === (whiteKingCastle recreatedBoard)
          .&&. (whiteQueenCastle board) === (whiteQueenCastle recreatedBoard)
          .&&. (blackKingCastle board) === (blackKingCastle recreatedBoard)
          .&&. (blackQueenCastle board) === (blackQueenCastle recreatedBoard)

prop_zebrahashconsistent =
  \board ->
    let Just (recreatedBoard, _) = loadFen $ boardToFen board
     in (zebraHash board) === (zebraHash recreatedBoard)

-- EXAMPLE TESTS --------------------------------------

createBoard :: String -> ChessBoard
createBoard fen =
    let command = "position fen " ++ fen
        Just (Position board) = parseUCICommand command
    in board

candidateExists :: String -> Move -> Bool
candidateExists uci move =
  let Just (Position board) = parseUCICommand uci
      candidates :: [Move]
      candidates = map fst $ candidateMoves board
   in elem move candidates

testFusion :: TestTree
testFusion = testCase "Test fusion" $ do
    (length $ fuseThis $ pseudoLegalCandidateMoves initialBoard) @?= 20
    (length $ fuseThis $ boardPositions initialBoard) @?= 32
    (length $ fuseThis $ boardPawnPositions initialBoard) @?= 16
    (length $ fuseThis $ boardNonPawnPositions initialBoard) @?= 16
    (length $ fuseThis $ pieceThreats initialBoard (1, 2, (ChessPiece White Pawn))) @?= 1

testSse :: TestTree
testSse = testCase "Test sse" $ do
    staticExchangeEvalWinning (createBoard "rnbqk2r/pppp1ppp/3b2n1/4p3/3PPB2/5N2/PPP2PPP/RN1QKB1R w KQkq - 3 5") (createMove 4 4 5 5 NoPromo (Just (Pawn, Pawn))) @?= 100
    staticExchangeEvalWinning (createBoard "rnbqk1nr/pppp2pp/3b1p2/4p3/3PP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 0 4") (createMove 4 4 5 5 NoPromo (Just (Pawn, Pawn))) @?= 0
    staticExchangeEvalWinning (createBoard "rnbqkbnr/ppp2ppp/3p4/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 3") (createMove 6 3 5 5 NoPromo (Just (Horse, Pawn))) @?= (-200)
    

unitTests =
  testGroup
    "Unit tests"
    [ testCase "Parse UCI command" $ do
        parseUCICommand "uci" @?= Just UCI
        parseUCICommand "quit" @?= Just Quit
        parseUCICommand "position startpos" @?= Just (Position initialBoard)
        parseUCICommand "position startpos moves" @?= Just (Position initialBoard)
        parseUCICommand "position startpos moves e2e4" @?= fmap Position (applyMove initialBoard (createMove 5 2 5 4 NoPromo Nothing))
        parseUCICommand "go depth 6" @?= Just (Go emptyGoProps {depth = Just 6}),
      testCase "3fold detection" $
        let Just (Position board1) = parseUCICommand "position startpos moves b1a3 b8a6 a3b1 a6b8 b1a3 b8a6 a3b1 a6b8"
            Just (Position board2) = parseUCICommand "position startpos moves b1a3 b8a6 a3b1 a6b8 b1a3 b8a6"
         in do
              is3foldRepetition board1 @?= True
              is3foldRepetition board2 @?= False,
      testCase "prev not held after irreversible move" $
        let Just (Position (ChessBoard {prev})) = parseUCICommand "position startpos moves b1a3 b8a6 a3b1 a6b8 b1a3 b8a6 e2e4"
         in (isJust prev) @?= False,
      testCase "Candidates moves" $ do
        -- test en pessant works
        candidateExists "position startpos moves e2e4 a7a5 e4e5 d7d5" (createMove 5 5 4 6 NoPromo (Just (Pawn, Pawn))) @?= True
        -- regression test, king trying to capture protected pieces
        candidateExists "position fen 8/8/8/7p/1P2pB2/4Kb2/2k5/8 w - - 0 46" (createMove 5 3 6 3 NoPromo (Just (King, Bishop))) @?= False
        -- regression test, check if it knows how to promote with capture
        candidateExists "position fen 2r3k1/3P1p2/5Bp1/8/4P3/5p2/3K3P/8 w - - 0 50" (createMove 4 7 3 8 PromoQueen (Just (Pawn, Rock))) @?= True
        -- regression test, check if king doesn't try to castle after having rook captured
        candidateExists "position startpos moves g1f3 g8h6 f3e5 g7g6 e5g6 f8g7 g6h8" (createMove 5 8 7 8 NoPromo Nothing) @?= False
        ,
      testCase "Precomputed rays" $ do
        elem [(1, 2)] (emptyBoardRockRays 2 2) @?= True
        elem [(3, 2), (4, 2), (5, 2), (6, 2), (7, 2), (8, 2)] (emptyBoardRockRays 2 2) @?= True
        elem [(2, 1)] (emptyBoardRockRays 2 2) @?= True
        elem [(2, 3), (2, 4), (2, 5), (2, 6), (2, 7), (2, 8)] (emptyBoardRockRays 2 2) @?= True

        elem [(1, 1)] (emptyBoardBishopRays 2 2) @?= True
        elem [(3, 3), (4, 4), (5, 5), (6, 6), (7, 7), (8, 8)] (emptyBoardBishopRays 2 2) @?= True
        elem [(3, 1)] (emptyBoardBishopRays 2 2) @?= True
        elem [(1, 3)] (emptyBoardBishopRays 2 2) @?= True,

      testFusion,
      testSse
    ]

return []

main =
  defaultMain $
    testGroup
      "Tests"
      [testProperties "Property tests" $allProperties, unitTests]
