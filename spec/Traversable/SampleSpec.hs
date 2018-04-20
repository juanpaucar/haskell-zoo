{-# LANGUAGE FlexibleContexts #-}

module Traversable.SampleSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Functor.Identity

import Basic.Sample
import Traversable.Sample
import MonoidAndSemigroup.Sample ()

arbitraryMyList :: Arbitrary a => Gen (MyList a)
arbitraryMyList = fromList <$> sized (\n ->  choose (0, n) >>= flip vectorOf arbitrary)
  where fromList [] = Nil
        fromList (x:xs) = Cons x (fromList xs)

myReverse :: MyList a -> MyList a
myReverse Nil = Nil
myReverse (Cons x xs) = myReverse xs `mappend` (Cons x Nil)

-------------------------------------------------------------------------------

naturality :: (Eq (t b), Traversable t) => t b -> Bool
naturality a = (t . traverse f $ a) == (traverse (t . f) a)
  where
    f = pure
    t = myReverse
-------------------------------------------------------------------------------

identity :: (Eq (t b), Traversable t) => t b -> Bool
identity a = (traverse Identity a) == (Identity a)
-------------------------------------------------------------------------------

newtype Compose f g a = Compose (f (g a))

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

composition :: (Eq (Compose MyList MyList (t b)), Traversable t) => 
  t (MyList (MyList b)) -> Bool
composition a = (traverse (Compose . fmap g . f) a) == (Compose . fmap (traverse g) . traverse f $ a)
  where
    g = myReverse
    f = myReverse

spec :: Spec
spec =
  describe "Traversable" $ do
    prop "satisfies the naturality property" $ forAll arbitraryMyList $ \list ->
      naturality (list :: MyList Int)

    prop "satisfies the identity property" $ forAll arbitraryMyList $ \list ->
      identity (list :: MyList Int)
