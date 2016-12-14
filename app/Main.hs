module Main where

import Lib
import System.Environment
import Text.Printf

main :: IO ()
main = do
  (for:en:n:_) <- getArgs
  corpus <- readCorpus for en
  let startProbs = initProbs corpus
      resultProbs = iterations (read n::Int) corpus startProbs
  putStrLn $ (showProbs resultProbs)++(printf "%.4f" $ perplexity corpus resultProbs)
