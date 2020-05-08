{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Bridge.IRC.Bot (
    stmPart
  , stmFromPart
  , encode
  , decode
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe
import qualified Data.Text as T
import qualified Data.Time.Clock
import qualified Data.Text.Encoding

import Network.IRC.Bot.BotMonad
import Network.IRC.Bot.Commands
import Network.IRC.Bot.Log

import Network.IRC.Bridge.Types

-- | Forward message from TChan to IRC
stmPart :: (BotMonad m)
         => ByteString
         -> TChan IRCOutput
         -> m ()
stmPart partName chan = do
  evt@IRCOutput{..} <- liftIO $ atomically $ readTChan chan
  logM Debug $ partName <> "StmPart: got" <> (B.pack $ show evt)
  if outputIsNotice then sendCommand $ Notice  Nothing [toTarget outputTo] $ encode outputBody
                    else sendCommand $ PrivMsg Nothing [toTarget outputTo] $ encode outputBody
  where
    toTarget = encode . channelOrUser

-- | Forward message from IRC to TChan
stmFromPart :: (BotMonad m)
            => ByteString
            -> TChan IRCInput
            -> m ()
stmFromPart partName chan = do
  priv  <- privMsg
  let receiver = head (receivers priv)

  inputFrom <- case "#" `B.isPrefixOf` receiver of
    True  -> pure $ IRCChannel $ decode receiver -- channel
    False -> do
      nick <- askSenderNickName
      pure $ IRCUser $ decode $ Data.Maybe.fromJust nick

  inputBody <- decode . msg <$> privMsg
  inputSender <- fmap decode <$> askSenderNickName

  -- are we addressed directly?
  name <- decode . botName <$> askBotEnv
  let inputBotAddressed = name `T.isPrefixOf` inputBody

  -- detect our command prefix
  -- and also 'botname: #cmd' and 'botname #cmd'
  pref <- T.pack . cmdPrefix <$> askBotEnv
  let inputHasCmdPrefix =
        (pref `T.isPrefixOf` inputBody)
        || (pref `T.isPrefixOf` (tryStripPrefix (name <> " ") inputBody))
        || (pref `T.isPrefixOf` (tryStripPrefix (name <> ": ") inputBody))

  inputTime <- liftIO Data.Time.Clock.getCurrentTime

  let payload = IRCInput {..}

  logM Debug $ partName <> "StmFromPart: got" <> (B.pack $ show payload)

  liftIO $ atomically $ writeTChan chan payload

tryStripPrefix pfx inp = case T.stripPrefix pfx inp of
  Nothing -> inp
  Just stripped -> stripped

decode :: ByteString -> Text
decode = Data.Text.Encoding.decodeUtf8

encode :: Text -> ByteString
encode = Data.Text.Encoding.encodeUtf8

channelOrUser :: IRCTarget -> Text
channelOrUser (IRCUser u)    = u
channelOrUser (IRCChannel c) = c
