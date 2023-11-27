module Network.IRC.Bridge.AMQP.Util (
    procIRCInputFromAMQP
  , prettyIRCInputFromAMQP
  ) where

import Control.Concurrent.STM
import qualified Control.Monad
import qualified Data.Text.IO

import Network.IRC.Bridge.AMQP
import Network.IRC.Bridge.Types
import Network.IRC.Bridge.Pretty

procIRCInputFromAMQP :: (IRCInput -> IO a) -> IO ()
procIRCInputFromAMQP fun = do
  ircIn <- amqpRunTail
  Control.Monad.void $ Control.Monad.forever $ do
    msg <- atomically $ readTChan ircIn
    fun msg

prettyIRCInputFromAMQP :: OutputMode -> IO ()
prettyIRCInputFromAMQP mode =
  procIRCInputFromAMQP
    (Data.Text.IO.putStrLn . renderInputMode mode)
