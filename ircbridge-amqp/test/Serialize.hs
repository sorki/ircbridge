module Serialize (spec) where

import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(..), oneof)
import Test.QuickCheck.Instances.Time
import Test.QuickCheck.Instances.Text

import Network.IRC.Bridge.Types
import Network.IRC.Bridge.AMQP.Serialize

instance Arbitrary IRCTarget where
  arbitrary = oneof [
      IRCUser <$> arbitrary
    , IRCChannel <$> arbitrary
    ]

instance Arbitrary IRCInput where
  arbitrary =
    IRCInput
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary IRCOutput where
  arbitrary =
    IRCOutput
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

spec :: Spec
spec = describe "amqp encoding" $ do
  prop "roundtrips IRCInput" $ \x ->
    (amqpDecodeIRCInput . amqpEncodeIRCInput $ x) `shouldBe` Just x

  prop "roundtrips IRCOutput" $ \x ->
    (amqpDecodeIRCOutput . amqpEncodeIRCOutput $ x) `shouldBe` Just x
