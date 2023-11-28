{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.IRC.Bridge.Options
  ( parseTarget
  , CatOpts(..)
  , parseCatOptions
  , IRCCatOpts(..)
  , parseIRCCatOptions
  , TailOpts(..)
  , parseTailOptions
  , parseOutputMode
  ) where

import Data.Text (Text)
import qualified Data.Text

import Options.Applicative

import Network.IRC.Bridge.Pretty
import Network.IRC.Bridge.Types

parseTarget :: Parser IRCTarget
parseTarget =
      (forUser <$> strOption
        (  long "user"
        <> short 'u'
        <> help "Send to user (cat) or filter only messages from user (tail)"
        ))
  <|> (either (error . Data.Text.unpack) id . forChannel <$> strOption
        (  long "chan"
        <> short 'c'
        <> help "Send to channel (cat) or filter only messages from channel (tail)"
        ))

data CatOpts = CatOpts {
    catTarget :: IRCTarget
  , catBody   :: Text
  , catNotice :: Bool
  } deriving (Eq, Show, Ord)

parseCatOptions :: Parser CatOpts
parseCatOptions = CatOpts
  <$> parseTarget
  <*> strArgument
        (  metavar "BODY"
        <> help "Message body"
        )
  <*> parseNoticeSwitch

parseNoticeSwitch :: Parser Bool
parseNoticeSwitch =
  switch
    (  long "notice"
    <> short 'n'
    <> help "Send message as /notice"
    )

data IRCCatOpts = IRCCatOpts {
    irccatTarget :: Maybe IRCTarget
  , irccatNotice :: Bool
  } deriving (Eq, Show, Ord)

parseIRCCatOptions :: Parser IRCCatOpts
parseIRCCatOptions = IRCCatOpts
  <$> optional parseTarget
  <*> parseNoticeSwitch

data TailOpts = TailOpts {
    tailTarget     :: Maybe IRCTarget
  , tailOutputMode :: Maybe OutputMode
  , tailOneShot    :: Bool -- ^ Exit after receiving and printing one message
  } deriving (Eq, Show, Ord)

parseTailOptions :: Parser TailOpts
parseTailOptions = TailOpts
  <$> optional parseTarget
  <*> optional parseOutputMode
  <*> switch
        (  long "oneshot"
        <> short '1'
        <> help "Exit after receiving and printing one message"
        )

parseOutputMode :: Parser OutputMode
parseOutputMode =
      flag' ShowOnly
          (  long "show"
          <> help "Print messages using Haskell show, very boring"
          )
  <|> flag' PrettySimple
          (  long "simple"
          <> short 's'
          <> help "Print messages using pretty-simple, similar to show but with colors"
          )
  <|> flag' Pretty
          (  long "pretty"
          <> short 'p'
          <> help "Pretty colored output"
          )
  <|> flag' PrettyDull
          (  long "pretty-dull"
          <> short 'd'
          <> help "Like pretty but a bit dull (no colors)"
          )
