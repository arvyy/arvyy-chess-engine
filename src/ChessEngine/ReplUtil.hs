module ChessEngine.ReplUtil
where

import ChessEngine.Board
import ChessEngine.HeuristicEvaluator (finalDepthEvalExplained)
import Control.Monad (forM_)

heuristicEvalExplainPosition :: String -> IO ()
heuristicEvalExplainPosition fen = do
  let (board, _) = case loadFen fen of
                    Just v -> v
                    _ -> error "Invalid fen"
  let (eval, explanation) = finalDepthEvalExplained board
  putStrLn $ "Evalation: " ++ show eval
  forM_ explanation putStrLn
