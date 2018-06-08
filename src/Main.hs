module Main where

import Control.Monad (void)

import qualified Basic.Sample as Basic
import qualified ParMonad.Sample as Par

main :: IO ()
main = do
  -- Basic.sample
  print $ Par.fib 40
