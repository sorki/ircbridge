module Main where

import Network.IRC.Bot
import Network.IRC.Bot.Part.AMQP

main :: IO ()
main = runBotWithParts amqpIRCBot
