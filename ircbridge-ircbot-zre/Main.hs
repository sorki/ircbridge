module Main where

import Network.IRC.Bot
import Network.IRC.Bot.Part.ZRE

main = runBotWithParts zreIRCBot
