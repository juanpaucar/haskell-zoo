module Basic.Sample 
  ( sample
  ) where

import Prelude hiding (map, filter, foldl, foldr)
---
--- Recursion: tail recursive
--- https://trello.com/c/IGvDFeQc/2-recursion-tail-recursive
---
--- Lists and standard list functions (including being able to reimplement them).
--- https://trello.com/c/QZliDxNA/3-lists-and-standard-list-functions-including-being-able-to-reimplement-them
---


data MyList a =  Cons a (MyList a) | Nil deriving (Show, Eq) 

map :: (a -> b) -> MyList a -> MyList b
map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)


filter :: (a -> Bool) -> MyList a -> MyList a
filter _ Nil = Nil
filter p (Cons x xs)
  | p x = Cons x (filter p xs)
  | otherwise = filter p xs

foldl :: (b -> a -> b) -> b -> MyList a -> b
foldl _ b  Nil = b
foldl f b (Cons x xs) = foldl f (f b x) xs

foldr :: (a -> b -> b) -> b -> MyList a -> b
foldr _ b Nil = b
foldr f b (Cons x xs) = f x (foldr f b xs)

sample :: IO ()
sample =
  let testList = Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 $ Cons 5 Nil
      filterer x = (== 0) $ x `mod` 2 
      folder = (+)
  in do
    putStrLn $ "Original List:\t" ++ show testList
    putStrLn $ "Filtered List:\t" ++ (show . filter filterer) testList
    putStrLn $ "Folded left:\t" ++ (show . foldl folder 0) testList
    putStrLn $ "Folded right:\t" ++ (show . foldr folder 0) testList

