module ChessEngine.TimeManager
  ( computeDeadline, interp
  )
where

import ChessEngine.Board (PlayerColor (..))

computeDeadline :: PlayerColor -> Int -> Int -> Int -> Maybe Int -> Maybe Int -> Int
computeDeadline player fullMoves whiteTime blackTime whiteInc blackInc =
  -- allocate more time towards mid game of 20-25 moves
  let myTime =
        if player == White
          then whiteTime
          else blackTime
      multiplier = interp fullMoves
      -- multiplier = 0.08
   in floor $ max 50.0 ((fromIntegral myTime) * multiplier)


-- return points of allocation line to be interpolated
-- [(fullMoveNumber, allocationFraction)]
timeAllocation :: [(Int, Float)]
timeAllocation =
    [ (0, 0.05)
    , (5, 0.05) -- don't overthink opening; unlikely to matter when playing with opening book
    , (10, 0.10) -- ramp up towards early mid game
    , (20, 0.20) -- ramp up more towards peak mid game
    , (25, 0.12) -- reduce towards late mid game
    , (30, 0.10)
    , (300, 0.10)
    ]

interp :: Int -> Float
interp move = go timeAllocation
  where
    go :: [(Int, Float)] -> Float
    go ((move1, mul1) : (move2, mul2) : _)
        | move >= move1 && move < move2
        = mul1 + (mul2 - mul1) * (fromIntegral (move - move1) / fromIntegral (move2 - move1))
    go (x:rest) = go rest
    go [] = 0.1
