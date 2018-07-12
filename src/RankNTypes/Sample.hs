{-# LANGUAGE RankNTypes #-}

module RankNTypes.Sample where

import System.Random
import Prelude hiding (length, id)
import Control.Monad.ST
import Data.STRef


length :: forall a. [a] -> Int
length [] = 0
length (x:xs) = 1 + (length xs)


type IdFunction = forall a. a -> a

-- id :: forall a. a -> a
id :: IdFunction
id x = x

sumOne :: (forall n. Num n => n -> n) -> (Int, Double)
sumOne f = (f 1, f 1.0)

applyToTuple :: (forall a. [a] -> Int) -> ([b], [c]) -> (Int, Int)
applyToTuple f (x,y) = (f x, f y)

type ShowFunc = forall a. Show a => a -> String

type SumFunc = forall a. Num a => a -> a

carryTheOne:: Int -> ST s Int
carryTheOne z = do
  r <- newSTRef z
  modifySTRef r (+1)
  readSTRef r

