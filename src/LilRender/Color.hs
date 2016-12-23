module LilRender.Color (
      Color(..)
    , YColor(..)
    , YAColor(..)
    , RGBColor(..)
    , RGBAColor(..)
    , scaleColor
    ) where

import Data.STBImage

{-# INLINE scaleColor #-}
scaleColor ∷ RGBColor -> Double -> RGBColor
scaleColor (RGBColor r g b) factor = RGBColor (round r') (round g') (round b')
    where
        factor' = max 0 $ min 1 factor
        r' = factor' * fromIntegral r
        g' = factor' * fromIntegral g
        b' = factor' * fromIntegral b
