{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Network.IRC.Bridge.ZRE.Util (
    procIRCInputFromZRE
  , prettyIRCInputFromZRE
  , OutputMode(..)
  , echoInToOut
  , echoSTM
  , echoMap
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import qualified Control.Monad
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Text.Pretty.Simple

import Network.IRC.Bridge.Types
import Network.IRC.Bridge.Pretty
import Network.IRC.Bridge.ZRE

import Network.ZRE
import Network.ZRE.Chan

data OutputMode =
    ShowOnly
  | PrettySimple
  | Pretty
  | PrettyDull
  deriving (Eq, Show)

procIRCInputFromZRE fun = do
  (_ircOut, ircIn) <- zreConsumeWith defaultZREConfig
  Control.Monad.forever $ do
    msg <- atomically $ readTChan ircIn
    fun msg

prettyIRCInputFromZRE mode =
  procIRCInputFromZRE
    (putStrLn . modeFunction mode)

modeFunction :: OutputMode
             -> IRCInput
             -> String
modeFunction ShowOnly = show
modeFunction PrettySimple =
    Data.Text.unpack
  . Data.Text.Lazy.toStrict
  . Text.Pretty.Simple.pShow
modeFunction Pretty =
    Data.Text.unpack
  . renderInput

echoInToOut IRCInput{..} = IRCOutput
  { outputTo = inputFrom
  , outputBody = inputBody
  , outputTime = inputTime
  , outputIsNotice = False
  }

echoSTM = do
  (ircOut, ircIn) <- zreConsumeWith defaultZREConfig
  Control.Monad.forever $ do
    atomically $ do
      x <- readTChan ircIn
      writeTChan ircOut $ echoInToOut x

echoMap = do
  print "wot"
  runZre $ mapToGroup @"ircOutput" @"ircInput" echoInToOut
  --runZre $ mapToGroup @"ircOutput" @"ircDbg" @IRCInput @String show
{--
-- to utils
mkIRCOutput to body isNotice = do
  now <- Data.Time.Clock.getCurrentTime
  return $ IRCOutput
  { outputTo = to
  , outputBody = body
  , outputTime = now
  , outputIsNotice = isNotice
  }
--}
