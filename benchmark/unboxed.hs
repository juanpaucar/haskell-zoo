{-# LANGUAGE MagicHash #-}

import GHC.Prim
import GHC.Exts

import Criterion.Main



mathBox :: Float -> Float -> Float
mathBox a b = (((a / b) * (b * 1.123)) - a) * ((b / a) * (a * 1.0123 / b))

mathUnboxed :: Float# -> Float# -> Float -> Float
mathUnboxed a b _ = F# ((((a `divideFloat#` b) `timesFloat#` (b `timesFloat#` (1.123#))) `minusFloat#` a) `timesFloat#` ((b `divideFloat#` a) `timesFloat#` (a `timesFloat#` (1.0123#) `divideFloat#` b)))

main = defaultMain [
        bgroup "boxed" [
          bench "million" $ nf (mathBox 1123345.0123123) 2123987.009123123
        , bench "billion" $ nf (mathBox 1123345478.897123) 2123987192.87512301
        , bench "trillion" $ nf (mathBox 1123345478109.2345671) 2123987192098.98732312
        , bench "zillion" $ nf (mathBox 1611686018427387903.76554123) 2611686018427387903.578617263
        ],

        bgroup "unboxed" [
          bench "million" $ nf (mathUnboxed (1123345.0123123#) (2123987.009123123#)) 1
        , bench "billion" $ nf (mathUnboxed (1123345478.897123#) (2123987192.87512301#)) 1
        , bench "trillion" $ nf (mathUnboxed (1123345478109.2345671#) (2123987192098.98732312#)) 1
        , bench "zillion" $ nf (mathUnboxed (1611686018427387903.76554123#) (2611686018427387903.578617263#)) 1
        ]
       ]


