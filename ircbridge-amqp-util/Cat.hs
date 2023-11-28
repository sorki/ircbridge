{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Network.IRC.Bridge.Options (CatOpts(..), parseCatOptions)
import Network.IRC.Bridge.Types (mkIRCOutput)
import Options.Applicative (execParser, helper, header, info, fullDesc, (<**>))
import qualified Network.IRC.Bridge.AMQP

main :: IO ()
main = execParser opts >>= \CatOpts{..} -> do
  msg <- mkIRCOutput catTarget catBody catNotice
  Network.IRC.Bridge.AMQP.publishIRCOutputs (pure msg)
  where
    opts =
      info
        (parseCatOptions <**> helper)
        (  fullDesc
        <> header "ircbridge-amqp-cat - send a message to IRC over AMQP"
        )
