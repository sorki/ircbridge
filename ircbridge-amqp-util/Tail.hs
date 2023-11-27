module Main where

import Network.IRC.Bridge.Options
import Network.IRC.Bridge.AMQP.Util

import Options.Applicative

main :: IO ()
main = execParser opts >>= prettyIRCInputFromAMQP
  where
    opts =
      info
        (parseOutputMode <**> helper)
        (  fullDesc
        <> header "ircbridge-amqp-tail - tail -f for IRC over AMQP"
        )
