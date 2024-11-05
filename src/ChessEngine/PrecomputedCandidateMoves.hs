module ChessEngine.PrecomputedCandidateMoves
  ( emptyBoardBishopRays,
    emptyBoardRockRays,
  )
where

import Data.Array.IArray

bishopRays :: Array Int [[(Int, Int)]]
bishopRays =
  let squares = do
        x <- [1 .. 8]
        y <- [1 .. 8]
        return (coordsToIndex x y, computeBishopRays x y)
   in array (0, 63) squares

rockRays :: Array Int [[(Int, Int)]]
rockRays =
  let squares = do
        x <- [1 .. 8]
        y <- [1 .. 8]
        return (coordsToIndex x y, computeRockRays x y)
   in array (0, 63) squares

coordsToIndex :: Int -> Int -> Int
coordsToIndex x y = (x - 1) * 8 + y - 1

emptyBoardBishopRays :: Int -> Int -> [[(Int, Int)]]
emptyBoardBishopRays x y = bishopRays ! coordsToIndex x y

emptyBoardRockRays :: Int -> Int -> [[(Int, Int)]]
emptyBoardRockRays x y = rockRays ! coordsToIndex x y

computeBishopRays :: Int -> Int -> [[(Int, Int)]]
computeBishopRays x y =
  [ computeRay x y (-1) (-1),
    computeRay x y 1 (-1),
    computeRay x y (-1) 1,
    computeRay x y 1 1
  ]

computeRockRays :: Int -> Int -> [[(Int, Int)]]
computeRockRays x y =
  [ computeRay x y (-1) 0,
    computeRay x y 0 (-1),
    computeRay x y 1 0,
    computeRay x y 0 1
  ]

computeRay :: Int -> Int -> Int -> Int -> [(Int, Int)]
computeRay x y dx dy = do
  offset <- [1 .. 8]
  let x' = x + offset * dx
  let y' = y + offset * dy
  if inBounds x' y'
    then return (x', y')
    else mempty

inBounds :: Int -> Int -> Bool
inBounds x y = x >= 1 && x <= 8 && y >= 1 && y <= 8
