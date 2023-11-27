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

import Control.Concurrent.STM
import qualified Control.Monad
import qualified Data.Text.IO

import Network.IRC.Bridge.Types
import Network.IRC.Bridge.Pretty
import Network.IRC.Bridge.ZRE

import Network.ZRE
import Network.ZRE.Chan

procIRCInputFromZRE :: (IRCInput -> IO a) -> IO ()
procIRCInputFromZRE fun = do
  (_ircOut, ircIn) <- zreConsumeWith defaultZREConfig
  Control.Monad.void $ Control.Monad.forever $ do
    msg <- atomically $ readTChan ircIn
    fun msg

prettyIRCInputFromZRE :: OutputMode -> IO ()
prettyIRCInputFromZRE mode =
  procIRCInputFromZRE
    (Data.Text.IO.putStrLn . renderInputMode mode)

echoInToOut :: IRCInput -> IRCOutput
echoInToOut IRCInput{..} = IRCOutput
  { outputTo = inputFrom
  , outputBody = inputBody
  , outputTime = inputTime
  , outputIsNotice = False
  }

echoSTM :: IO ()
echoSTM = do
  (ircOut, ircIn) <- zreConsumeWith defaultZREConfig
  Control.Monad.void $ Control.Monad.forever $ do
    atomically $ do
      x <- readTChan ircIn
      writeTChan ircOut $ echoInToOut x

echoMap :: IO ()
echoMap = do
  runZre $ mapToGroup @"ircOutput" @"ircInput" echoInToOut
  --runZre $ mapToGroup @"ircOutput" @"ircDbg" @IRCInput @String show
