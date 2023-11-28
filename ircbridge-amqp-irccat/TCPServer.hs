{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Options.Applicative

import Network.IRC.Bridge.Options
import Network.IRC.Bridge.Types
import Network.IRC.Bridge.AMQP

main :: IO ()
main = execParser opts >>= \CatOpts{..} -> do
  msg <- mkIRCOutput catTarget catBody catNotice
  publishIRCOutput msg
  where
    opts =
      info
        (parseCatOptions <**> helper)
        (  fullDesc
        <> header "ircbridge-amqp-irccat-tcpserver - send a message to IRC over AMQP, piped into TCP server a la irccat"
        )
