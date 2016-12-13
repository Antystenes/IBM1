module Main where

import Lib

main :: IO ()
main = print $ iterateTest 10000 testProbs
