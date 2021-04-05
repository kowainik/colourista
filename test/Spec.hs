module Main (main) where

import Test.Hspec (hspec)

import Test.Colourista (colouristaSpec)


main :: IO ()
main = hspec colouristaSpec
