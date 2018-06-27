module Catch.Sample where

import Control.Exception (ArithException(..))
import Control.Monad.Catch

-- main :: IO ()
-- main = do
--   let x = 10 `safeDiv` 0
--   case x of
--     Left a -> print (a :: SomeException)
--     Right b -> print b

main :: IO ()
main = do
  let Right x = 10 `safeDiv` 0 `catch` ten
  print x
  where
    ten :: SomeException -> Either SomeException Int
    -- ten :: ArithException -> Either SomeException Int
    ten _ = return 10

safeDiv :: MonadThrow m => Int -> Int -> m Int
safeDiv x y = if y == 0
              then throwM RatioZeroDenominator
              else return $ x `div` y
