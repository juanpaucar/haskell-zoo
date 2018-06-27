{-# LANGUAGE RankNTypes #-}

module RankNTypes.Sample where

import System.Random
import Control.Monad.State
import Prelude hiding (length, id)

length :: forall a. [a] -> Int
length [] = 0
length (x:xs) = 1 + (length xs)

-- id :: forall a. a -> a
-- id x = x

type IdFunc = forall a. a -> a

id :: IdFunc
id x = x




data Player = Player {
  playerName :: String,
  playerPos  :: (Double, Double)
} deriving (Eq, Ord, Show)

type GenAction m = forall a. (Random a) => m a

type GenActionR m = forall a. (Random a) => (a, a) -> m a

genRandom :: (RandomGen g) => GenAction (State g)
genRandom = state random

genRandomR :: (RandomGen g) => GenActionR (State g)
genRandomR range = state (randomR range)

randomPlayer :: (MonadIO m) => GenActionR m -> m Player
randomPlayer genR = do
    liftIO (putStrLn "Generating random player...")

    len <- genR (8, 12)
    name <- replicateM len (genR ('a', 'z'))
    x <- genR (-100, 100)
    y <- genR (-100, 100)

    liftIO (putStrLn "Done.")
    return (Player name (x, y))

main :: IO ()
main = randomPlayer randomRIO >>= print


--------------------------------------------------------
--------------------------------------------------------

data List a
    = Cons a (List a)
    | Nil

uncons :: (a -> List a -> r) -> r -> List a -> r
uncons co ni (Cons x xs) = co x xs
uncons co ni Nil         = ni

listNull :: List a -> Bool
listNull = uncons (\_ _ -> False) True

listMap :: (a -> b) -> List a -> List b
listMap f = uncons (\x xs -> Cons (f x) (listMap f xs)) Nil

newtype ListS a = ListS {
  unconsS :: forall r. (a -> ListS a -> r) -> r -> r
}

nilS :: ListS a
nilS = ListS (\co ni -> ni)

consS :: a -> ListS a -> ListS a
consS x xs = ListS (\co ni -> co x xs)

unconsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
unconsS' co ni (ListS f) = f co ni
