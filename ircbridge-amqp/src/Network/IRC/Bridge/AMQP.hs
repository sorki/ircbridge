{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Bridge.AMQP (
    amqpRun
  , amqpRunWith
  , amqpRunTail
  , amqpRunTailWith
  , defaultAMQPConfig
  , AMQPConfig(..)
  , publishIRCOutputsWith
  , publishIRCOutputs
  ) where

import Control.Monad (forever, forM_, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception as X
import Data.Text (Text)

import Network.AMQP
import Network.IRC.Bridge.Types
import Network.IRC.Bridge.AMQP.Serialize

data AMQPConfig = AMQPConfig {
    cfgHostName       :: String
  , cfgVirtualHost    :: Text
  , cfgLoginName      :: Text
  , cfgLoginPass      :: Text
  , cfgQueueName      :: Text
  , cfgExchangeName   :: Text
  , cfgExchangeType   :: Text
  , cfgRoutingKey     :: Text
  , cfgRoutingKeyFrom :: Text
  } deriving (Eq, Show)

defaultAMQPConfig :: AMQPConfig
defaultAMQPConfig = AMQPConfig {
    cfgHostName        = "localhost"
  , cfgVirtualHost     = "/"
  , cfgLoginName       = "guest"
  , cfgLoginPass       = "guest"
  , cfgQueueName       = "" -- ad-hoc generated by rabbitmq
  , cfgExchangeName    = "ircExchange"
  , cfgExchangeType    = "direct"
  , cfgRoutingKey      = "amqp.irc"
  , cfgRoutingKeyFrom  = "irc.amqp"
  }

-- | Connect to AMQP and forward messages from and to IRC
amqpRun :: IO ( TChan IRCOutput
              , TChan IRCInput)
amqpRun = amqpRunWith defaultAMQPConfig

-- | Connect to AMQP and forward messages from and to IRC
-- Accepts `AMQPConfig`.
amqpRunWith :: AMQPConfig
             -> IO ( TChan IRCOutput
                   , TChan IRCInput)
amqpRunWith AMQPConfig{..} = do
  conn <- openConnection cfgHostName cfgVirtualHost cfgLoginName cfgLoginPass

  -- AMQP -> IRC

  chan <- openChannel conn
  (q, _, _) <- declareQueue chan newQueue {
      queueName = cfgQueueName
    }
  declareExchange chan newExchange {
      exchangeName = cfgExchangeName
    , exchangeType = cfgExchangeType
    }

  bindQueue chan q cfgExchangeName cfgRoutingKey

  tChan <- newTChanIO
  _ <- consumeMsgs chan q Ack (amqpCallback tChan)

  -- IRC -> AMQP

  chanFrom <- openChannel conn
  (qf, _, _) <- declareQueue chanFrom newQueue {
      queueName = cfgQueueName
    }
  declareExchange chanFrom newExchange {
      exchangeName = cfgExchangeName
    , exchangeType = cfgExchangeType
    }

  bindQueue chanFrom qf cfgExchangeName cfgRoutingKeyFrom
  tChanFrom <- newTChanIO

  void $ forkIO $ forever $ do
    ircInput <- atomically $ readTChan tChanFrom
    publishMsg
      chanFrom
      cfgExchangeName
      cfgRoutingKeyFrom
      (amqpEncodeIRCInput ircInput)

  void $ forkIO $ do
    X.catch
      (forever $ threadDelay 5000000)
      (\e -> putStrLn ("amqp exception" ++ show (e :: X.SomeException)))
    closeConnection conn

  return (tChan, tChanFrom)

-- | Forward messages from AMQP to TChan consumed by `amqpPart`.
-- Messages need "targetChannel" header.
amqpCallback :: TChan IRCOutput
             -> (Message, Envelope)
             -> IO ()
amqpCallback chan (msg, env) = do
    case amqpDecodeIRCOutput msg of
      Just ircOut -> do
        liftIO $ atomically $ writeTChan chan ircOut
        -- acknowledge receiving the message
        ackEnv env

      Nothing -> error $
        "Unable to decode AMQP message to IRCOutput, message was"
        ++ show msg

-- | Connect to AMQP and forward messages from IRC
amqpRunTail :: IO (TChan IRCInput)
amqpRunTail = amqpRunTailWith defaultAMQPConfig

-- | Connect to AMQP and forward messages from IRC
-- Accepts `AMQPConfig`.
amqpRunTailWith :: AMQPConfig -> IO (TChan IRCInput)
amqpRunTailWith AMQPConfig{..} = do
  conn <- openConnection cfgHostName cfgVirtualHost cfgLoginName cfgLoginPass

  chan <- openChannel conn
  (q, _, _) <- declareQueue chan newQueue {
      queueName = cfgQueueName
    }
  declareExchange chan newExchange {
      exchangeName = cfgExchangeName
    , exchangeType = cfgExchangeType
    }

  bindQueue chan q cfgExchangeName cfgRoutingKeyFrom

  tChan <- newTChanIO
  _ <- consumeMsgs chan q Ack (amqpTailCallback tChan)

  void $ forkIO $ do
    X.catch
      (forever $ threadDelay 5000000)
      (\e -> putStrLn ("amqp exception" ++ show (e :: X.SomeException)))
    closeConnection conn

  return tChan

-- | Forward messages from AMQP to TChan consumed by `amqpPart`.
amqpTailCallback
  :: TChan IRCInput
  -> (Message, Envelope)
  -> IO ()
amqpTailCallback chan (msg, env) = do
    case amqpDecodeIRCInput msg of
      Just ircIn -> do
        liftIO $ atomically $ writeTChan chan ircIn
        -- acknowledge receiving the message
        ackEnv env

      Nothing -> error $
        "Unable to decode AMQP message to IRCInput, message was"
        ++ show msg

-- | Connect to AMQP and send a single IRC message
publishIRCOutputs :: [IRCOutput] -> IO ()
publishIRCOutputs = publishIRCOutputsWith defaultAMQPConfig

-- | Connect to AMQP and send a single IRC message
-- Accepts `AMQPConfig`.
publishIRCOutputsWith :: AMQPConfig -> [IRCOutput] -> IO ()
publishIRCOutputsWith AMQPConfig{..} ircOutputs = do
  conn <- openConnection cfgHostName cfgVirtualHost cfgLoginName cfgLoginPass

  chan <- openChannel conn
  (qf, _, _) <- declareQueue chan newQueue {
      queueName = cfgQueueName
    }
  declareExchange chan newExchange {
      exchangeName = cfgExchangeName
    , exchangeType = cfgExchangeType
    }

  bindQueue chan qf cfgExchangeName cfgRoutingKey

  forM_ ircOutputs $ \ircOutput ->
    publishMsg
      chan
      cfgExchangeName
      cfgRoutingKey
      (amqpEncodeIRCOutput ircOutput)

  closeConnection conn
