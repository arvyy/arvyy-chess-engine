module Main where

import Data.Maybe
import ChessEngine.Board
import ChessEngine.PositionEval

-- fen = "5r2/2r1bkpQ/1q2p3/p3Pp2/1p1P4/4P1R1/6PP/R5K1 b - - 3 32"
fen = "6k1/8/2p4p/3p4/P5B1/6K1/P4r2/6q1 w - - 2 46"

main :: IO ()
main = print (show $ evaluate $ fromJust (loadFen fen))


