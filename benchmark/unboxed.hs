{-# LANGUAGE TypeOperators #-}

import Criterion.Main

import GHC.Exts


sum :: Int -> Int -> Int
sum = undefined

sum' :: Int# -> Int# -> Int#
sum' = undefined

main = defaultMain []


