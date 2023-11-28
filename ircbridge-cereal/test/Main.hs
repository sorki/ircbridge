{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Test.Hspec (Spec, describe, hspec, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Roundtrip (roundtrips)

import Data.Serialize (Serialize(..), runGet, runPut)
import Network.IRC.Bridge.Arbitrary ()
import Network.IRC.Bridge.Serialize ()
import Network.IRC.Bridge.Types (IRCInput, IRCOutput)

spec :: Spec
spec = describe "Cereal encoding" $ do
  prop "roundtrips IRCInput"
    $ roundtrips @IRCInput (runPut . put) (runGet get)

  prop "roundtrips IRCOutput"
    $ roundtrips @IRCOutput (runPut . put) (runGet get)

main :: IO ()
main = hspec spec
