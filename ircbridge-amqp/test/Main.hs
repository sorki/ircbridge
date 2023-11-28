module Main where

import Test.Hspec (hspec)
import qualified Serialize

main :: IO ()
main = hspec Serialize.spec
