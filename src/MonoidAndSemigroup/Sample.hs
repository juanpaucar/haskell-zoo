module MonoidAndSemigroup.Sample where

import Data.Semigroup

import Basic.Sample (MyList(..))


instance Monoid (MyList a) where
  mempty = Nil
  mappend Nil cons = cons
  mappend (Cons x xs) cons = Cons x (xs `mappend` cons)

instance Semigroup (MyList a) where
  (<>) = mappend

