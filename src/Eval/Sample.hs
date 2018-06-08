module Eval.Sample (fibo) where

import Control.Parallel.Strategies

fib :: Int -> Int
fib n
  | n <= 2 = 1
  | otherwise = fib (n-1) + fib (n-2)

evalfib :: Int -> Eval Int
evalfib n
  | n <= 2 = return 1
  | otherwise = do
    -- x <- rpar . fib $ n - 1
    -- y <- rpar . fib $ n - 2
    x <- evalfib $ n - 1
    y <- evalfib $ n - 2
    rseq x
    rseq y
    return $ x + y

fibo :: Int -> Int
fibo = runEval . evalfib
