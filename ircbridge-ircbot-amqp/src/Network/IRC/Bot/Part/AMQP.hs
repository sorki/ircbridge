{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Bot.Part.AMQP (
    amqpIRCBot
  , amqpIRCBotWith
  , amqpRun
  , defaultAMQPConfig
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Time.Clock
import qualified Data.Maybe

import Network.IRC.Bot.BotMonad
import Network.IRC.Bot.Commands
import Network.IRC.Bot.Log

import Network.IRC.Bridge.Types
import Network.IRC.Bridge.AMQP
import Network.IRC.Bridge.AMQP.Serialize
import Network.IRC.Bridge.IRC.Bot

-- | Start AMQP with custom `AMQPConfig`
-- and return parts for ircbot
amqpIRCBotWith :: (BotMonad m)
               => AMQPConfig
               -> IO [m ()]
amqpIRCBotWith conf = do
  (amqpChan, amqpFromChan) <- amqpRunWith conf
  return [
      stmPart "amqp" amqpChan
    , stmFromPart "amqp" amqpFromChan
    ]

-- | Start AMQP with `defaultAMQPConfig`
-- and return parts for ircbot
amqpIRCBot :: (BotMonad m)
           => IO [m ()]
amqpIRCBot = amqpIRCBotWith defaultAMQPConfig
