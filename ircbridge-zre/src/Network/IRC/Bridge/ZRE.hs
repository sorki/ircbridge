{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Network.IRC.Bridge.ZRE (
    zreRun
  , zreRunWith
  , zreConsumeWith
  , ZREConfig(..)
  , defaultZREConfig
  ) where

import Data.ByteString (ByteString)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.Async.Lifted

import Network.IRC.Bridge.Serialize
import Network.IRC.Bridge.Types
import Network.ZRE
import Network.ZRE.Chan

data ZREConfig = ZREConfig {
    cfgIRCInGroup  :: ByteString
  , cfgIRCOutGroup :: ByteString
  } deriving (Eq, Show)

defaultZREConfig = ZREConfig "ircInput" "ircOutput"

zreRun :: IO ( TChan IRCOutput
             , TChan IRCInput)
zreRun = zreRunWith defaultZREConfig

zreRunWith :: ZREConfig
           -> IO ( TChan IRCOutput
                 , TChan IRCInput)
zreRunWith  ZREConfig{..} = do
  (zreIn, zreOut) <- zreChanWith runZreEnvConfig (mkGroup cfgIRCOutGroup) (mkGroup cfgIRCInGroup)
  -- our IRCOutput is input for ZRE channel
  return (zreOut, zreIn)

-- | Subscribe to ZRE groups
-- created by `zreRunWith`
zreConsumeWith :: ZREConfig
               -> IO ( TChan IRCOutput
                     , TChan IRCInput)
zreConsumeWith ZREConfig{..} = do
  -- in and out group names are swapped
  (zreIn, zreOut) <- zreChanWith runZreEnvConfig (mkGroup cfgIRCInGroup) (mkGroup cfgIRCOutGroup)
  return (zreIn, zreOut)
