module ChessEngine.PrecomputedCandidateMoves
  ( emptyBoardBishopRays,
    emptyBoardRockRays,
    emptyBoardHorseHops,
    emptyBoardKingHops
  )
where

import Data.Array.IArray
import ChessEngine.Board

bishopRays :: Array Int [[Coord]]
bishopRays =
  let squares = do
        x <- [1 .. 8]
        y <- [1 .. 8]
        return (coordsToIndex x y, computeBishopRays x y)
   in array (0, 63) squares

rockRays :: Array Int [[Coord]]
rockRays =
  let squares = do
        x <- [1 .. 8]
        y <- [1 .. 8]
        return (coordsToIndex x y, computeRockRays x y)
   in array (0, 63) squares

horseHops :: Array Int [Coord]
horseHops =
  let squares = do
        x <- [1 .. 8]
        y <- [1 .. 8]
        return (coordsToIndex x y, computeHorseHops x y)
   in array (0, 63) squares

kingHops :: Array Int [Coord]
kingHops =
  let squares = do
        x <- [1 .. 8]
        y <- [1 .. 8]
        return (coordsToIndex x y, computeKingHops x y)
   in array (0, 63) squares

coordsToIndex :: Int -> Int -> Int
coordsToIndex x y = (x - 1) * 8 + y - 1

emptyBoardBishopRays :: Int -> Int -> [[Coord]]
emptyBoardBishopRays x y = bishopRays ! coordsToIndex x y

emptyBoardRockRays :: Int -> Int -> [[Coord]]
emptyBoardRockRays x y = rockRays ! coordsToIndex x y

emptyBoardHorseHops :: Int -> Int -> [Coord]
emptyBoardHorseHops x y = horseHops ! coordsToIndex x y

emptyBoardKingHops :: Int -> Int -> [Coord]
emptyBoardKingHops x y = kingHops ! coordsToIndex x y

computeBishopRays :: Int -> Int -> [[Coord]]
computeBishopRays x y =
  [ computeRay x y (-1) (-1),
    computeRay x y 1 (-1),
    computeRay x y (-1) 1,
    computeRay x y 1 1
  ]

computeRockRays :: Int -> Int -> [[Coord]]
computeRockRays x y =
  [ computeRay x y (-1) 0,
    computeRay x y 0 (-1),
    computeRay x y 1 0,
    computeRay x y 0 1
  ]

computeRay :: Int -> Int -> Int -> Int -> [Coord]
computeRay x y dx dy = do
  offset <- [1 .. 8]
  let x' = x + offset * dx
  let y' = y + offset * dy
  if inBounds x' y'
    then return $ makeCoord x' y'
    else mempty

computeHorseHops :: Int -> Int -> [Coord]
computeHorseHops x y =
    let hops = [ (x + 1, y + 2)
               , (x + 1, y - 2)
               , (x - 1, y + 2)
               , (x - 1, y - 2)
               , (x + 2, y + 1)
               , (x + 2, y - 1)
               , (x - 2, y + 1)
               , (x - 2, y - 1)]
    in
        map (uncurry makeCoord)
        . filter (uncurry inBounds)
        $ hops

computeKingHops :: Int -> Int -> [Coord]
computeKingHops x y =
    let hops = [(x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1]]
    in 
        map (uncurry makeCoord)
        . filter (\(x', y') -> inBounds x' y' && not (x == x' && y == y'))
        $ hops
