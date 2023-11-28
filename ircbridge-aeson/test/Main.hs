{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Test.Hspec (Spec, describe, hspec, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Roundtrip (roundtrips)

import Data.Aeson (fromJSON, toJSON)

import Network.IRC.Bridge.Arbitrary ()
import Network.IRC.Bridge.Aeson ()
import Network.IRC.Bridge.Types (IRCInput, IRCOutput)

spec :: Spec
spec = describe "JSON encoding" $ do
  prop "roundtrips IRCInput"
    $ roundtrips @IRCInput toJSON fromJSON

  prop "roundtrips IRCOutput"
    $ roundtrips @IRCOutput toJSON fromJSON

main :: IO ()
main = hspec spec
