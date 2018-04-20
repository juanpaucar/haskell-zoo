{-# LANGUAGE MagicHash #-}

import GHC.Prim
import GHC.Exts

import Criterion.Main



sum' :: Int -> Int -> Int
sum' = (+)

sum'' :: Int# -> Int# -> Int -> Int
sum'' a b _ = I# (a +# b)

main = defaultMain [
        bgroup "sum" [
          bench "million" $ nf (sum' 1123345) 2123987
        , bench "billion" $ nf (sum' 1123345478) 2123987192
        , bench "trillion" $ nf (sum' 1123345478109) 2123987192098
        , bench "zillion" $ nf (sum' 1611686018427387903) 2611686018427387903
        ],

        bgroup "sum primitive" [
          bench "million" $ nf (sum'' 1123345# 2123987#) 1
        , bench "billion" $ nf (sum'' 1123345478# 2123987192#) 1
        , bench "trillion" $ nf (sum'' 1123345478109# 2123987192098#) 1
        , bench "zillion" $ nf (sum'' 1611686018427387903# 2611686018427387903#) 1
        ]
       ]


