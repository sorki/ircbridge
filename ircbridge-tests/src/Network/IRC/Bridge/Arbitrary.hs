{-# OPTIONS_GHC -Wno-orphans #-}
module Network.IRC.Bridge.Arbitrary () where

import Test.QuickCheck (Arbitrary(..), oneof)
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.Text ()

import Network.IRC.Bridge.Types

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
