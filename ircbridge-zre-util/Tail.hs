module Main where

import Network.IRC.Bridge.Options (parseOutputMode)
import Network.IRC.Bridge.ZRE.Util (prettyIRCInputFromZRE)
import Options.Applicative (execParser, helper, header, info, fullDesc, (<**>))

main :: IO ()
main = execParser opts >>= prettyIRCInputFromZRE
  where
    opts =
      info
        (parseOutputMode <**> helper)
        (  fullDesc
        <> header "ircbridge-zre-tail - tail -f for IRC over ZRE"
        )
