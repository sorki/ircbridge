{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Test.Hspec (Expectation, Spec, describe, hspec, it, shouldBe, shouldSatisfy)

import qualified Data.Either
import qualified Data.Time.Clock.POSIX

import Network.IRC.Bridge.IRCCat
import Network.IRC.Bridge.Types

defTarget :: IRCTarget
defTarget = either undefined id $ forChannel "#def"

testTime :: UTCTime
testTime = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0

testOk1'
  :: Text -- ^ Targets part of the message
  -> Text -- ^ Body
  -> Text -- ^ Expected body
  -> IRCTarget -- ^ Expected target
  -> Expectation
testOk1' msgpre msg msgExpect tgt =
  parseLikeIRCCat
    defTarget
    testTime
    False
    (msgpre <> " " <> msg)
  `shouldBe`
  Right [
    IRCOutput
      { outputTo = tgt
      , outputBody = msgExpect
      , outputTime = testTime
      , outputIsNotice = False
      }
  ]

testFail
  :: Text -- ^ Body
  -> Expectation
testFail msg =
  parseLikeIRCCat
    defTarget
    testTime
    False
    msg
  `shouldSatisfy`
  Data.Either.isLeft

testOk1 :: Text -> Text -> IRCTarget -> Expectation
testOk1 msgpre msg =
  testOk1' msgpre msg msg

testOkMany :: Text -> Text -> [IRCTarget] -> Expectation
testOkMany msgpre msg tgts =
  parseLikeIRCCat
    defTarget
    testTime
    False
    (msgpre <> " " <> msg)
  `shouldBe`
  (Right
    $ flip map tgts
    $ \t ->
      IRCOutput
        { outputTo = t
        , outputBody = msg
        , outputTime = testTime
        , outputIsNotice = False
        }
  )

spec :: Spec
spec = describe "IRCCat input decoding" $ do
  it "handles message without target"
    $ testOk1 mempty "Message without target" defTarget

  it "handles message with user target"
    $ testOk1 "@user" "Message with user target" (forUser "user")

  it "handles message with channel target"
    $ testOk1 "#chan" "Message with channel target" (either undefined id $ forChannel "#chan")

  it "handles multiple targets"
    $ testOkMany "#chan,@user,#chan2" "Message"
        $ Data.Either.rights
            [ forChannel "#chan"
            , pure $ forUser "user"
            , forChannel "#chan2" ]

  describe "degenerate cases" $ do
    it "ignores newlines"
      $ testOk1' mempty "\nAb\nCd\n" "Ab Cd" defTarget

    it "fails on empty" $ testFail mempty
    it "fails on empty message" $ testFail "@user"
    it "fails on empty message many targets" $ testFail "@user,#chan"
    it "fails on bad targets" $ testFail "# a,@ b msg"

main :: IO ()
main = hspec spec
