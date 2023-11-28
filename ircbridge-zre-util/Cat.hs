{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.IO.Class (liftIO)

import Network.IRC.Bridge.Options (CatOpts(..), parseCatOptions)
import Network.IRC.Bridge.Serialize (encodeIRCOutput)
import Network.IRC.Bridge.Types (mkIRCOutput)
import Network.ZRE (Group)

import qualified Network.ZRE

g :: Group
g = Network.ZRE.mkGroup "ircInput"

main :: IO ()
main = Network.ZRE.runZreParse parseCatOptions $ \CatOpts{..} -> do
  Network.ZRE.zjoin g
  liftIO (mkIRCOutput catTarget catBody catNotice)
  >>= Network.ZRE.zshout g . encodeIRCOutput
