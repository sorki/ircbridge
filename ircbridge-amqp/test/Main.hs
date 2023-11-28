module Main (main) where

import Test.Hspec (Spec, describe, hspec, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Roundtrip (roundtrips)

import Network.IRC.Bridge.Arbitrary ()
import Network.IRC.Bridge.AMQP.Serialize

spec :: Spec
spec = describe "AMQP encoding" $ do
  prop "roundtrips IRCInput"
    $ roundtrips amqpEncodeIRCInput amqpDecodeIRCInput

  prop "roundtrips IRCOutput"
    $ roundtrips amqpEncodeIRCOutput amqpDecodeIRCOutput

main :: IO ()
main = hspec spec
