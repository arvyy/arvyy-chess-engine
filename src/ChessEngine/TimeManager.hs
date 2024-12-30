module ChessEngine.TimeManager
  ( computeDeadline,
  )
where

import ChessEngine.Board (PlayerColor (..))

computeDeadline :: PlayerColor -> Int -> Int -> Int -> Maybe Int -> Maybe Int -> Int
computeDeadline player fullMoves whiteTime blackTime whiteInc blackInc =
  -- allocate 1/20 of remaining time
  -- with a minimum of 0.5sec
  let myTime =
        if player == White
          then whiteTime
          else blackTime
      multiplier
        | fullMoves <= 3 = 0.02
        | fullMoves > 25 = 0.04
        | otherwise = 0.10
   in floor $ max 50.0 ((fromIntegral myTime) * multiplier)
