{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Bridge.Pretty (
    prettyOutput
  , prettyInput
  , renderOutput
  , renderInput
  , renderTermInput
  , OutputMode(..)
  , renderInputMode
  ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601

import Prettyprinter
import Prettyprinter.Render.Text
import Prettyprinter.Render.Terminal (AnsiStyle, Color(..), bold, color)
import qualified Prettyprinter.Render.Terminal as Term

import Network.IRC.Bridge.Types

import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Text.Pretty.Simple

renderOutput :: IRCOutput -> Text
renderOutput =
    renderStrict
  . layoutPretty defaultLayoutOptions
  . prettyOutput

renderInput :: IRCInput -> Text
renderInput =
    renderStrict
  . layoutPretty defaultLayoutOptions
  . prettyInput

renderTermInput :: IRCInput -> Text
renderTermInput =
    Term.renderStrict
  . layoutPretty defaultLayoutOptions
  . prettyInput

prettyTarget :: IRCTarget -> Doc AnsiStyle
prettyTarget (IRCUser u) =
  annotate (color Cyan)
  "user:" <+> pretty u
prettyTarget (IRCChannel c) =
  annotate (color Magenta)
  "chan:" <+> pretty c

prettyTime :: UTCTime -> Doc AnsiStyle
prettyTime =
    brackets
  . annotate (color Blue)
  . pretty
  . iso8601Show

prettySender :: Pretty a => Maybe a -> Doc AnsiStyle
prettySender (Just u) =
    braces
  $ "sender:" <+>
     ( annotate (color Yellow)
       $ pretty u
     )
prettySender Nothing = mempty

prettyFlag :: Bool -> String -> Doc AnsiStyle
prettyFlag True  x = braces (pretty x)
prettyFlag False _ = mempty

prettyInput :: IRCInput -> Doc AnsiStyle
prettyInput IRCInput{..} =
      prettyTime inputTime
  <+> angles (prettyTarget inputFrom)
  <>  prettySender inputSender
  <>  prettyFlag inputHasCmdPrefix "cmdPrefix"
  <>  prettyFlag inputBotAddressed "addressed"
  <>  ":"
  <+> annotate bold (pretty inputBody)

prettyOutput :: IRCOutput -> Doc AnsiStyle
prettyOutput IRCOutput{..} =
      prettyTime outputTime
  <+> angles (prettyTarget outputTo)
  <>  prettyFlag outputIsNotice "notice"
  <>  ":"
  <>  pretty outputBody

data OutputMode =
    ShowOnly
  | PrettySimple
  | Pretty
  | PrettyDull
  deriving (Eq, Ord, Show)

renderInputMode
  :: OutputMode
  -> IRCInput
  -> Text
renderInputMode ShowOnly =
    Data.Text.pack . show
renderInputMode PrettySimple =
    Data.Text.Lazy.toStrict
  . Text.Pretty.Simple.pShow
renderInputMode Pretty = renderTermInput
renderInputMode PrettyDull = renderInput
