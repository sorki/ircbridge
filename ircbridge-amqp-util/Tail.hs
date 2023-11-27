{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent.STM
import Network.IRC.Bridge.AMQP
import Network.IRC.Bridge.Options
import Network.IRC.Bridge.Pretty
import Network.IRC.Bridge.Types
import Options.Applicative

import qualified Control.Monad
import qualified Data.Maybe
import qualified Data.Text.IO

run :: TailOpts -> IO ()
run TailOpts{..} = do
  ircIn <- amqpRunTail
  let display =
        Data.Text.IO.putStrLn
        . renderInputMode
            (Data.Maybe.fromMaybe
              Pretty
              tailOutputMode)

      act = do
        msg <- atomically $ readTChan ircIn
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
