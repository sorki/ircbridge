{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Options.Applicative
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Time.Clock
import qualified Data.Maybe

import Network.IRC.Bridge.Options
import Network.IRC.Bridge.Types (IRCTarget, forChannel)
import Network.IRC.Bridge.IRCCat
import Network.IRC.Bridge.AMQP

defTarget :: IRCTarget
defTarget = either undefined id $ forChannel "#bottest"

main :: IO ()
main = do
  IRCCatOpts{..} <- execParser opts

  now <- Data.Time.Clock.getCurrentTime

  let failIfDefaultNeeded =
        Data.Maybe.fromMaybe
          (error "No target in input message and no --chan or --user option specified")

  parseLikeIRCCat
    (failIfDefaultNeeded irccatTarget)
    now
    irccatNotice
      <$> Data.Text.IO.getContents
      >>= \case
        Left e -> error $ Data.Text.unpack e
        Right msgs -> do
          print msgs
          publishIRCOutputs msgs
  where
    opts =
      info
        (parseIRCCatOptions <**> helper)
        (  fullDesc
        <> header "ircbridge-amqp-irccat - send a message to IRC over AMQP, piped into stdin a la irccat"
        )
