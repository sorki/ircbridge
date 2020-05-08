{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Encoding of IRCInput and IRCOuput to and from AMQP Message
--
-- For users of this library the two functions
-- are useful
--  - amqpDecodeIRCInput for decoding AMQP to IRCInput - receiving messages
--  - amqpEncodeIRCInput for encoding IRCOutput to AMQP - sending messages

module Network.IRC.Bridge.AMQP.Serialize (
    amqpEncodeIRCInput
  , amqpDecodeIRCInput
  , amqpEncodeIRCOutput
  , amqpDecodeIRCOutput
  ) where

import Data.Text (Text)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map
import qualified Data.Time.Format.ISO8601

import Network.AMQP (Message(..), DeliveryMode(Persistent), newMsg)
import Network.AMQP.Types (FieldTable(FieldTable), FieldValue(..))
import Network.IRC.Bridge.Types
import Network.IRC.Bridge.IRC.Bot

-- | Encode `IRCOutput` to AMQP `Message`
amqpEncodeIRCOutput :: IRCOutput -> Message
amqpEncodeIRCOutput IRCOutput{..} = newMsg
  { msgBody = BL.fromStrict $ encode outputBody
  , msgDeliveryMode = Just Persistent
  , msgHeaders = Just $ FieldTable $
      Data.Map.fromList $ [
        ("isNotice", FVBool outputIsNotice)
      , ("time", FVString $ B.pack $ Data.Time.Format.ISO8601.iso8601Show outputTime)
      , channelOrUserVal outputTo
      ]
  }

-- | Decode AMQP `Message` to `IRCInput`
amqpDecodeIRCOutput :: Message -> Maybe IRCOutput
amqpDecodeIRCOutput msg = do
  (FieldTable headers)  <- msgHeaders msg
  (FVBool outputIsNotice) <- Data.Map.lookup "isNotice" headers
  (FVString outputTime') <- Data.Map.lookup "time" headers
  outputTime <- Data.Time.Format.ISO8601.iso8601ParseM $ B.unpack outputTime'

  outputTo <- case (Data.Map.lookup "user" headers, Data.Map.lookup "channel" headers) of
    (Just (FVString u), _) -> Just $ IRCUser    $ decode u
    (_, Just (FVString c)) -> Just $ IRCChannel $ decode c
    _ -> Nothing

  let outputBody = decode $ BL.toStrict $ msgBody msg
  Just IRCOutput{..}

-- | Encode `IRCInput` to AMQP `Message`
amqpEncodeIRCInput :: IRCInput -> Message
amqpEncodeIRCInput IRCInput{..} = newMsg
  { msgBody = BL.fromStrict $ encode inputBody
  , msgDeliveryMode = Just Persistent
  , msgHeaders = Just $ FieldTable $
     Data.Map.fromList $ [
       ("forBot", FVBool inputBotAddressed)
     , ("hasPrefix", FVBool inputHasCmdPrefix)
     , ("time", FVString $ B.pack $ Data.Time.Format.ISO8601.iso8601Show inputTime)
     , channelOrUserVal inputFrom
     ] ++ maybe [] (\u -> [("sender", FVString $ encode u)]) inputSender
  }

-- | Decode AMQP `Message` to `IRCInput`
amqpDecodeIRCInput :: Message -> Maybe IRCInput
amqpDecodeIRCInput msg = do
  (FieldTable headers)  <- msgHeaders msg
  (FVBool inputBotAddressed) <- Data.Map.lookup "forBot"    headers
  (FVBool inputHasCmdPrefix) <- Data.Map.lookup "hasPrefix" headers
  (FVString inputTime') <- Data.Map.lookup "time" headers
  inputTime <- Data.Time.Format.ISO8601.iso8601ParseM $ B.unpack inputTime'

  let inputBody = decode $ BL.toStrict $ msgBody msg
      inputSender = case Data.Map.lookup "sender" headers of
        Nothing -> Nothing
        Just (FVString u) -> Just $ decode u

  inputFrom <- case (Data.Map.lookup "user" headers, Data.Map.lookup "channel" headers) of
    (Just (FVString u), _) -> Just $ IRCUser    $ decode u
    (_, Just (FVString c)) -> Just $ IRCChannel $ decode c
    _ -> Nothing

  Just IRCInput{..}

channelOrUserVal :: IRCTarget -> (Text, FieldValue)
channelOrUserVal (IRCUser u)    = ("user",    FVString $ encode u)
channelOrUserVal (IRCChannel c) = ("channel", FVString $ encode c)
