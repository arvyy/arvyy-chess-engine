module ChessEngine.UCI
  ( GoProps (..),
    UCICommand (..),
    parseUCICommand,
    emptyGoProps,
  )
where

import ChessEngine.Board
import ChessEngine.CandidateMoves
import Control.Applicative
import Data.List
import Text.Read (readMaybe)
import Text.Regex.PCRE

data GoProps = GoProps
  { searchMoves :: ![Move],
    ponder :: !Bool,
    whiteTime :: !(Maybe Int),
    blackTime :: !(Maybe Int),
    whiteIncrement :: !(Maybe Int),
    blackIncrement :: !(Maybe Int),
    movesToGo :: !(Maybe Int),
    depth :: !(Maybe Int),
    nodes :: !(Maybe Int),
    mate :: !(Maybe Int),
    moveTime :: !(Maybe Int),
    infinite :: !Bool
  }
  deriving (Eq, Show)

emptyGoProps :: GoProps
emptyGoProps =
  GoProps
    { searchMoves = [],
      ponder = False,
      whiteTime = Nothing,
      blackTime = Nothing,
      whiteIncrement = Nothing,
      blackIncrement = Nothing,
      movesToGo = Nothing,
      depth = Nothing,
      nodes = Nothing,
      mate = Nothing,
      moveTime = Nothing,
      infinite = False
    }

data UCICommand
  = UCI
  | Debug !Bool
  | IsReady
  | SetOption !String !(Maybe String)
  | UCINewGame
  | Position !ChessBoard
  | Go GoProps
  | Stop
  | PonderHit
  | Quit
  deriving (Eq, Show)

parseUCICommand :: String -> Maybe UCICommand
parseUCICommand input
  | "uci" == input = Just UCI
  | "isready" == input = Just IsReady
  | "ucinewgame" == input = Just UCINewGame
  | "stop" == input = Just Stop
  | "ponderhit" == input = Just PonderHit
  | "quit" == input = Just Quit
  | "debug off" == input = Just $ Debug False
  | "debug on" == input = Just $ Debug True
  | otherwise = tryParseGo <|> tryParsePosition
  where
    tryParseGo :: Maybe UCICommand
    tryParseGo =
      if isPrefixOf "go" input
        then do
          props <-
            tryParseGoParts
              (drop 3 input)
              emptyGoProps
          return $ Go props
        else Nothing

    tryParseGoParts input props = tryParseGoParts' (words input) props

    tryParseGoParts' ("depth" : depthStr : rest) props = do
      depth <- readMaybe depthStr
      let props' = props {depth = Just depth}
      tryParseGoParts' rest props'
    tryParseGoParts' ("wtime" : whiteTimeStr : rest) props = do
      wtime <- readMaybe whiteTimeStr
      let props' = props {whiteTime = Just wtime}
      tryParseGoParts' rest props'
    tryParseGoParts' ("btime" : blackTimeStr : rest) props = do
      btime <- readMaybe blackTimeStr
      let props' = props {blackTime = Just btime}
      tryParseGoParts' rest props'
    tryParseGoParts' ("winc" : whiteIncStr : rest) props = do
      winc <- readMaybe whiteIncStr
      let props' = props {whiteIncrement = Just winc}
      tryParseGoParts' rest props'
    tryParseGoParts' ("binc" : blackIncStr : rest) props = do
      binc <- readMaybe blackIncStr
      let props' = props {blackIncrement = Just binc}
      tryParseGoParts' rest props'
    tryParseGoParts' ("movetime" : movetimeStr : rest) props = do
      movetime' <- readMaybe movetimeStr
      let props' = props {moveTime = Just movetime'}
      tryParseGoParts' rest props'

    -- skip unrecognized values
    tryParseGoParts' (key : value : rest) props =
      tryParseGoParts' rest props
    tryParseGoParts' _ props = Just props

    tryParsePosition =
      let posWithMoves = case (input =~ "^position (.+) moves ?(.*)$") :: (AllSubmatches [] (Int, Int)) of
            (AllSubmatches (all : positionStr : moveStrs : [])) -> Just (positionStr, moveStrs)
            _ -> Nothing
          posWithoutMoves = case (input =~ "^position (.+)$") :: (AllSubmatches [] (Int, Int)) of
            (AllSubmatches (all : positionStr : [])) -> Just (positionStr, (0, 0))
            _ -> Nothing
       in do
            pos <- posWithMoves <|> posWithoutMoves
            tryParsePosition' pos

    tryParsePosition' ((posStart, posLen), (moveStart, moveLen)) = do
      let positionStr = take posLen $ drop posStart input
      board <-
        if positionStr == "startpos"
          then Just initialBoard
          else
            if isPrefixOf "fen " positionStr
              then fmap fst (loadFen $ drop 4 positionStr)
              else Nothing
      let moveStrs =
            if moveLen == 0
              then []
              else words $ take moveLen $ drop moveStart input
      moves <- sequence (map parseMove moveStrs)
      board <- foldl' (\maybeBoard move -> maybeBoard >>= (\board -> applyMove board move)) (Just board) moves
      return $ Position board
