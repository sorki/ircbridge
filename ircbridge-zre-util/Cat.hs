{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

import Options.Applicative

import Network.IRC.Bridge.Types
import Network.IRC.Bridge.Serialize

import Network.ZRE

data CatOpts = CatOpts {
    catTarget :: IRCTarget
  , catBody   :: Text
  , catNotice :: Bool
  } deriving (Eq, Show, Ord)

parseCatOptions :: Parser CatOpts
parseCatOptions = CatOpts
  <$> parseTarget
  <*> strArgument (metavar "BODY")
  <*> switch (long "notice" <> short 'n')

parseTarget :: Parser IRCTarget
parseTarget =
      (IRCUser <$> strOption (long "user" <> short 'u'))
  <|> (IRCChannel <$> strOption (long "chan" <> short 'c'))

g :: Group
g = mkGroup "ircInput"

main :: IO ()
main = runZreParse parseCatOptions $ \CatOpts{..} -> do
  zjoin g
  liftIO (mkIRCOutput catTarget catBody catNotice)
  >>= zshout g . encodeIRCOutput
