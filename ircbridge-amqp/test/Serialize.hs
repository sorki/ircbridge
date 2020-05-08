module Serialize where

import Test.Tasty.QuickCheck
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

prop_ircInput x =
  (amqpDecodeIRCInput . amqpEncodeIRCInput $ x) === Just x

prop_ircOutput x =
  (amqpDecodeIRCOutput . amqpEncodeIRCOutput $ x) === Just x
