module Main where

import Options.Applicative
import Network.IRC.Bridge.Options
import Network.IRC.Bridge.ZRE.Util

main :: IO ()
main = execParser opts >>= prettyIRCInputFromZRE
  where
    opts =
      info
        (parseOutputMode <**> helper)
        (  fullDesc
        <> header "ircbridge-zre-tail - tail -f for IRC over ZRE"
        )
