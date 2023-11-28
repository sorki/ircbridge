{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.IRC.Bridge.IRCCat
  ( parseLikeIRCCat
  , parseTarget
  )  where

import Data.Text (Text)
import Data.Time (UTCTime)
import Network.IRC.Bridge.Types

import qualified Data.Either
import qualified Data.Text

-- | Shotgun parse IRCCat-like inputs
--
--     - #channel Some message
--     - @user Hi
--     - #channel,@user1,@user2,#channel3 Multiple recepients
--     - Message without target uses a default one
--
parseLikeIRCCat
  :: IRCTarget -- ^ Default target if not supplied
  -> UTCTime -- ^ Time when message was created
  -> Bool -- ^ Send as notice
  -> Text  -- ^ Input
  -> Either Text [IRCOutput]
parseLikeIRCCat defTarget utcTime isNotice =
  let makeOutput target words =
        IRCOutput
           { outputTo = target
           , outputBody = Data.Text.unwords words
           , outputTime = utcTime
           , outputIsNotice = isNotice
           }
  in
    (\case
        [] -> Left "Empty input"
        (x:[]) | hasChanOrUserChar x
                 && Data.Text.elem ',' x ->
            Left "Empty message"
        (x:xs) | hasChanOrUserChar x
                 && Data.Text.elem ',' x ->
          case map parseTarget (Data.Text.splitOn ":" x) of
            ts | any (Data.Either.isLeft) ts ->
                 Left
              $ "Failed to parse multiple targets, got "
              <> Data.Text.pack (show $ Data.Either.lefts ts)
            ts ->
              pure $ flip map ts $ \tgt ->
                makeOutput (either (error "absurd") id tgt) xs
        (x:[]) | hasChanOrUserChar x ->
            Left "Empty message"
        (x:xs) | hasChanOrUserChar x ->
          case parseTarget x of
            Left e -> Left e
            Right tgt ->
              pure $ pure $ makeOutput tgt xs
        xs | otherwise ->
          pure $ pure $ makeOutput defTarget xs
    )
  . Data.Text.words
  . Data.Text.unlines
  . filter (not . Data.Text.null)
  . Data.Text.lines
  where
    hasChanOrUserChar x =
         Data.Text.elem '#' x
      || Data.Text.elem '@' x

-- | Parse @user or #channel
parseTarget :: Text -> Either Text IRCTarget
parseTarget = \case
  x | Data.Text.elem ' ' x ->
      Left
    $ "Target can't contain spaces: '"
    <> x
    <> "'"
  x | not (Data.Text.isAscii x) ->
      Left
    $ "Target has to be ASCII string: '"
    <> x
    <> "'"
  x | Data.Text.length x < 2 ->
      Left
    $ "Not enough input for target parser: '"
    <> x
    <> "'"
  x | "#" `Data.Text.isPrefixOf` x -> pure $ IRCChannel (Data.Text.tail x)
  x | "@" `Data.Text.isPrefixOf` x -> pure $ IRCUser (Data.Text.tail x)
  x | otherwise ->
      Left
    $ "Failed to parse target, #channel or @user is required: '"
    <> x
    <> "' and the input has to start with # or @"
