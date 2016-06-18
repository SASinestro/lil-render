module Bench where

import Bench.Macro

import Criterion.Main

width = 800
height = 800

main :: IO ()
main = defaultMain [
    bgroup "Macro benchmarks" [
          benchLoadModel
        , benchLoadTexture
        , benchDrawImage width height
        ]
    ]
