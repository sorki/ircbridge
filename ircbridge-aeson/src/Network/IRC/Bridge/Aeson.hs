{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.IRC.Bridge.Aeson where

import Data.Aeson (FromJSON, ToJSON)

import Network.IRC.Bridge.Types

deriving instance ToJSON   IRCTarget
deriving instance FromJSON IRCTarget

deriving instance ToJSON   IRCInput
deriving instance FromJSON IRCInput

deriving instance ToJSON   IRCOutput
deriving instance FromJSON IRCOutput
