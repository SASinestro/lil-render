module Main where

import Image
import Image.Color

red_color = RGBColor {
      _red = 255
    , _green = 0
    , _blue = 0
    , _alpha = 255
}

main :: IO ()
main = do
    let image = make_image 50 50 red_color
    write_image TGA "red.tga" image