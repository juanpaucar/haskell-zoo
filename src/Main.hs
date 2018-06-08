module Main where

import Control.Monad (void)

import qualified Basic.Sample as Basic
import qualified ParMonad.Sample as Par
import qualified Eval.Sample as Eval

main :: IO ()
main = do
  -- Basic.sample
  -- print $ Par.fib 40
  print $ Eval.fibo 50
