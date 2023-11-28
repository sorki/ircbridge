{-# LANGUAGE RecordWildCards #-}
module Main where

import Network.IRC.Bridge.Options (TailOpts(..), parseTailOptions)
import Network.IRC.Bridge.Pretty (OutputMode(..), renderInputMode)
import Network.IRC.Bridge.Types (IRCInput(inputFrom))
import Options.Applicative (execParser, helper, header, info, fullDesc, (<**>))

import qualified Control.Concurrent.STM
import qualified Control.Monad
import qualified Data.Maybe
import qualified Data.Text.IO
import qualified Network.IRC.Bridge.AMQP

run :: TailOpts -> IO ()
run TailOpts{..} = do
  ircIn <- Network.IRC.Bridge.AMQP.amqpRunTail
  let display =
        Data.Text.IO.putStrLn
        . renderInputMode
            (Data.Maybe.fromMaybe
              Pretty
              tailOutputMode)

      act = do
        msg <-
            Control.Concurrent.STM.atomically
          $ Control.Concurrent.STM.readTChan ircIn
        case tailTarget of
          Nothing -> display msg
          Just t | t == inputFrom msg -> display msg
          _ -> pure ()

  if tailOneShot
  then act
  else Control.Monad.forever act

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (parseTailOptions <**> helper)
        (  fullDesc
        <> header "ircbridge-amqp-tail - tail -f for IRC over AMQP"
        )
