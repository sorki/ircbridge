{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Network.IRC.Bridge.Options (IRCCatServerOpts(..))
import Options.Applicative (execParser, helper, header, info, fullDesc, (<**>))

import qualified Control.Monad
import qualified Data.ByteString.Char8
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Time.Clock
import qualified Network.Run.TCP
import qualified Network.Socket.ByteString
import qualified Network.IRC.Bridge.AMQP
import qualified Network.IRC.Bridge.IRCCat
import qualified Network.IRC.Bridge.Options
import qualified Network.IRC.Bridge.Pretty

main :: IO ()
main = do
  sopts <- execParser opts
  Network.Run.TCP.runTCPServer (Just "localhost") "33000" (loop sopts)
  where
    loop sopts@IRCCatServerOpts{..} sock = do
        sockMsg <- Network.Socket.ByteString.recv sock 1024
        Control.Monad.unless (Data.ByteString.Char8.null sockMsg) $ do
          now <- Data.Time.Clock.getCurrentTime

          let textSockMsg =
                  Data.Text.pack
                $ Data.ByteString.Char8.unpack sockMsg

              emsgs =
                Network.IRC.Bridge.IRCCat.parseLikeIRCCat
                  irccatServerTarget
                  now
                  irccatServerNotice
                  textSockMsg

          case emsgs of
            Left e -> do
              Data.Text.IO.putStrLn e
              Data.Text.IO.putStrLn $ "Input was: '" <> textSockMsg <> "'"
            Right msgs -> do
              Data.Text.IO.putStrLn $ "Sending"

              Control.Monad.forM_
                msgs
                (Data.Text.IO.putStrLn . Network.IRC.Bridge.Pretty.renderOutput)

              Network.IRC.Bridge.AMQP.publishIRCOutputs msgs

          loop sopts sock

    opts =
      info
        (    Network.IRC.Bridge.Options.parseIRCCatServerOptions
        <**> helper
        )
        (  fullDesc
        <> header "ircbridge-amqp-irccat-tcpserver - send a message to IRC over AMQP, piped into TCP server a la irccat"
        )
