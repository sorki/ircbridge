{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Bot.Part.ZRE (
    zreIRCBot
  , zreIRCBotWith
  , defaultZREConfig
  ) where

import Network.IRC.Bot.BotMonad

import Network.IRC.Bridge.ZRE
import Network.IRC.Bridge.IRC.Bot

-- | Start ZRE IRC bridge with custom `ZREConfig`
-- and return parts for ircbot
zreIRCBotWith :: (BotMonad m)
               => ZREConfig
               -> IO [m ()]
zreIRCBotWith conf = do
  (zreChan, zreFromChan) <- zreRunWith conf
  return [
      stmPart "zre" zreChan
    , stmFromPart "zre" zreFromChan
    ]

-- | Start ZRE IRC bridge with `defaultZREConfig`
-- and return parts for ircbot
zreIRCBot :: (BotMonad m)
           => IO [m ()]
zreIRCBot = zreIRCBotWith defaultZREConfig

