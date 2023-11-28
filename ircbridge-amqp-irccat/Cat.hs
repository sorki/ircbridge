{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Network.IRC.Bridge.Options (IRCCatOpts(..))
import Options.Applicative (execParser, helper, header, info, fullDesc, (<**>))

import qualified Control.Monad
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Time.Clock
import qualified Data.Maybe

import qualified Network.IRC.Bridge.AMQP
import qualified Network.IRC.Bridge.IRCCat
import qualified Network.IRC.Bridge.Pretty
import qualified Network.IRC.Bridge.Options

main :: IO ()
main = do
  IRCCatOpts{..} <- execParser opts

  now <- Data.Time.Clock.getCurrentTime

  let failIfDefaultNeeded =
        Data.Maybe.fromMaybe
          (error "No target in input message and no --chan or --user option specified")

  Network.IRC.Bridge.IRCCat.parseLikeIRCCat
    (failIfDefaultNeeded irccatTarget)
    now
    irccatNotice
      <$> Data.Text.IO.getContents
      >>= \case
        Left e -> error $ Data.Text.unpack e
        Right msgs -> do
          Data.Text.IO.putStrLn $ "Sending"
          Control.Monad.forM_
            msgs
            (Data.Text.IO.putStrLn . Network.IRC.Bridge.Pretty.renderOutput)

          Network.IRC.Bridge.AMQP.publishIRCOutputs msgs
  where
    opts =
      info
        (    Network.IRC.Bridge.Options.parseIRCCatOptions
        <**> helper
        )
        (  fullDesc
        <> header "ircbridge-amqp-irccat - send a message to IRC over AMQP, piped into stdin a la irccat"
        )
