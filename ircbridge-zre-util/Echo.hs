module Main where

import Network.IRC.Bridge.ZRE.Util

-- | This app echoes the IRCInput
-- as IRCOutput on another ZRE channel
main :: IO ()
main = echoMap
