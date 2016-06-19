module Bench where

import Bench.Macro
import Bench.Triangles

import Criterion.Main

width = 800
height = 800

main :: IO ()
main = defaultMain [
      macro width height
     , triangles
    ]
