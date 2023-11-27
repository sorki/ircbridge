{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.IRC.Bridge.Serialize where

import Data.ByteString (ByteString)
import Data.Serialize
import Data.Serialize.Text ()
import Data.Time.Clock.Serialize ()

import Network.IRC.Bridge.Types

instance Serialize IRCTarget
instance Serialize IRCInput
instance Serialize IRCOutput

encodeIRCOutput :: IRCOutput
                -> ByteString
encodeIRCOutput = encode

decodeIRCOutput :: ByteString
                -> Either String IRCOutput
decodeIRCOutput = decode

encodeIRCInput :: IRCInput
               -> ByteString
encodeIRCInput = encode

decodeIRCInput :: ByteString
               -> Either String IRCInput
decodeIRCInput = decode
