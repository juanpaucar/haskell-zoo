module MonoidAndSemigroup.SampleSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Control.Monad
import Data.Semigroup

import Basic.Sample
import MonoidAndSemigroup.Sample ()


arbitraryMyList :: Arbitrary a => Gen (MyList a)
arbitraryMyList = fromList <$> sized (\n ->  choose (0, n) >>= flip vectorOf arbitrary)
  where fromList [] = Nil
        fromList (x:xs) = Cons x (fromList xs)

associativity :: (Eq a, Monoid a) => a -> a -> a -> Bool
associativity a b c = (a `mappend` (b `mappend` c)) == ((a `mappend` b) `mappend` c)

identity :: (Eq a, Monoid a) => a -> Bool
identity a = (a `mappend` mempty) == a &&
             (mempty `mappend` a) == a

concatSemigroup :: (Eq a, Semigroup a, Monoid a) => a -> a -> Bool
concatSemigroup a b = (a `mappend` b) == (a <> b)


spec :: Spec
spec = do
  describe "Monoid" $ do
    it "satisfies the associative property for mappedn" $
      quickCheck (associativity :: MyList Int -> MyList Int -> MyList Int -> Bool)

    prop "satisfies identity property for monoids" $ forAll arbitraryMyList $ \list ->
      identity (list :: MyList Int)

  describe "Semigroup" $
    it "satisfies that <> is mappend for monoids" $
      quickCheck (concatSemigroup :: MyList Int -> MyList Int -> Bool)

-- TODO: use arbitrary for functions
