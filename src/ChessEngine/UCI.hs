module ChessEngine.UCI
  ( GoProps (..),
    UCICommand (..),
    parseUCICommand,
    emptyGoProps,
  )
where

import ChessEngine.Board
import Control.Applicative
import Control.Monad
import Data.List
import Debug.Trace
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

    -- skip unrecognized values
    tryParseGoParts' (key : value : rest) props =
      tryParseGoParts' rest props
    tryParseGoParts' [] props = Just props

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
