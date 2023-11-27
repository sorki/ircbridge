{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.IO.Class (liftIO)

import Network.IRC.Bridge.Options
import Network.IRC.Bridge.Serialize
import Network.IRC.Bridge.Types

import Network.ZRE

g :: Group
g = mkGroup "ircInput"

main :: IO ()
main = runZreParse parseCatOptions $ \CatOpts{..} -> do
  zjoin g
  liftIO (mkIRCOutput catTarget catBody catNotice)
  >>= zshout g . encodeIRCOutput
