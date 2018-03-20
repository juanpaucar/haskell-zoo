module MonoidAndSemigroup.Sample where

import Data.Semigroup

import Basic.Sample (MyList(..))


instance Monoid (MyList a) where
  mempty = Nil
  mappend Nil Nil = Nil
  mappend cons Nil = cons
  mappend Nil cons = cons
  mappend (Cons x xs) (Cons y ys) = (Cons x Nil) `mappend`
                                    xs           `mappend`
                                    (Cons y Nil) `mappend`
                                    ys

instance Semigroup (MyList a) where
  (<>) = mappend

