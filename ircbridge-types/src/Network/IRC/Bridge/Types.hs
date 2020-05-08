{-# LANGUAGE DeriveGeneric #-}

module Network.IRC.Bridge.Types (
    IRCTarget(..)
  , IRCInput(..)
  , IRCOutput(..)
  ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import GHC.Generics

data IRCTarget = IRCUser Text | IRCChannel Text
  deriving (Eq, Show, Ord, Typeable, Generic)

-- | Input from IRC network
data IRCInput = IRCInput {
    inputFrom         :: IRCTarget  -- ^ Message received from user or channel (reply to)
  , inputBody         :: Text       -- ^ Message body
  , inputSender       :: Maybe Text -- ^ Nick of the user sending this message on channel, redundant if `IRCTarget` is `IRCUser`
  , inputHasCmdPrefix :: Bool       -- ^ Message using bot prefix (#some_command)
  , inputBotAddressed :: Bool       -- ^ Message addressing bot directly (some_bot_name: hi)
  , inputTime         :: UTCTime    -- ^ Time when the message was received
  } deriving (Eq, Show, Ord, Typeable, Generic)

-- | Output to IRC network
data IRCOutput = IRCOutput {
    outputTo       :: IRCTarget     -- ^ Send message to user or channel
  , outputBody     :: Text          -- ^ Message body
  , outputIsNotice :: Bool          -- ^ Send a message as notice instead of default privmsg
  , outputTime     :: UTCTime       -- ^ Time when the message was created
  } deriving (Eq, Show, Ord, Typeable, Generic)
