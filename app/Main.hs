module Main where

import System.TimeIt
import Text.Printf

import QAlgos (runRetroGrover', runRetroShor)

------------------------------------------------------------------------------
-- Example runs

main :: IO ()
main = do
  mapM_ grover [0..20]

grover n = do
  printf "n = %d\t" n
  timeIt (runRetroGrover' n 0)

------------------------------------------------------------------------------
