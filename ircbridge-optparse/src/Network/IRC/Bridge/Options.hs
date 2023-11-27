{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.IRC.Bridge.Options 
  ( parseTarget
  , CatOpts(..)
  , parseCatOptions
  , TailOpts(..)
  , parseTailOptions
  , parseOutputMode
  ) where

import Data.Text (Text)

import Options.Applicative

import Network.IRC.Bridge.Pretty
import Network.IRC.Bridge.Types

parseTarget :: Parser IRCTarget
parseTarget =
      (IRCUser <$> strOption (long "user" <> short 'u'))
  <|> (IRCChannel <$> strOption (long "chan" <> short 'c'))

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

data TailOpts = TailOpts {
    tailTarget     :: IRCTarget
  , tailOutputMode :: OutputMode
  , tailOneShot    :: Bool -- ^ Exit after outputting a message
  } deriving (Eq, Show, Ord)

parseTailOptions :: Parser TailOpts
parseTailOptions = TailOpts
  <$> parseTarget
  <*> parseOutputMode
  <*> switch (long "oneshot" <> short '1')

parseOutputMode :: Parser OutputMode
parseOutputMode =
      flag' ShowOnly (long "show" <> short 'w')
  <|> flag' PrettySimple (long "simple" <> short 's')
  <|> flag' Pretty (long "pretty" <> short 'p')
  <|> flag' PrettyDull (long "pretty-dull" <> short 'd')
