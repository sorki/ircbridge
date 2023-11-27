module Main where

import Control.Monad
import Network.IRC.Bot
import Network.IRC.Bot.Part.AMQP
import Network.IRC.Bot.Part.ZRE

main :: IO ()
main = runBotWithParts $ liftM2 (++) amqpIRCBot zreIRCBot
