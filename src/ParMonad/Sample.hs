module ParMonad.Sample (fib) where

import Control.Monad.Par

parfib :: Int -> Par Int
parfib n
  | n <= 2 = return 1
  | otherwise = do
    x  <- spawn $ parfib (n-1)
    y  <- spawn $ parfib (n-2)
    x' <- get x
    y' <- get y
    return (x' + y')

fib :: Int -> Int
fib = runPar . parfib
