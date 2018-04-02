module Traversable.Sample where

import Basic.Sample (MyList(..))
import MonoidAndSemigroup.Sample

instance Functor MyList where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable MyList where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = (f x) `mappend` (foldMap f xs)

instance Applicative MyList where
  pure x = Cons x Nil
  (Cons f _) <*> xs = fmap f xs
  Nil <*> xs = Nil

instance Traversable MyList where
  traverse _ Nil = pure Nil
  traverse g (Cons x xs) = Cons <$> g x <*> traverse g xs

