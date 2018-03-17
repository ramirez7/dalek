{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Dalek.Exts.Time.TheSpec (spec) where

import           Dalek.SpecUtils

import qualified Data.HashMap.Strict.InsOrd as HMI
import           Data.Time

import qualified Dhall.Context             as Dh
import qualified Dhall.Core                as Dh
import qualified Dhall.Parser              as Dh
import qualified Dhall.TypeCheck           as Dh

import           Dalek.Core
import           Dalek.Parser
import           Dalek.TypeCheck

import           Dalek.Exts.Time.Core      (DhTime (..))
import qualified Dalek.Exts.Time.Core
import qualified Dalek.Exts.Time.Parser
import qualified Dalek.Exts.Time.TypeCheck

spec :: Spec
spec = describe "end-to-end" $ do
  describe "UTCTime" $ do
    it "should parse literals" $ do
      expr <- checkedAndNormalized parser typer normalizer [i|
              -- Note the timezone
              $(1776-07-04T00:00:00Z)
            |]
      expr `shouldBe` sendEmbed (DhUTCTimeLit $ UTCTime (fromGregorian 1776 7 4) 0)
      explicitOffset <- checkedAndNormalized parser typer normalizer [i|
              $(1776-07-04T00:00:00+0000)
            |]
      explicitOffset `shouldBe` sendEmbed (DhUTCTimeLit $ UTCTime (fromGregorian 1776 7 4) 0)
    it "should typecheck literals" $ do
      good <- parsed parser [i|
              $(1776-07-04T00:00:00Z) : UTCTime
            |]
      shouldBeRight $ typeOf good

      bad <- parsed parser [i|
              $(1776-07-04T00:00:00Z) : LocalTime
            |]
      shouldBeLeft $ typeOf bad
  describe "LocalTime" $ do
    it "should parse literals" $ do
      expr <- checkedAndNormalized parser typer normalizer [i|
              -- Note the *lack* of timezone
              $(1776-07-04T00:00:00)
            |]
      expr `shouldBe` sendEmbed (DhLocalTimeLit $ LocalTime (fromGregorian 1776 7 4) midnight)
    it "should typecheck literals" $ do
      good <- parsed parser [i|
              $(1776-07-04T00:00:00) : LocalTime
            |]
      shouldBeRight $ typeOf good

      bad <- parsed parser [i|
              $(1776-07-04T00:00:00) : UTCTime
            |]
      shouldBeLeft $ typeOf bad
  describe "TimeZone" $ do
    it "should parse literals" $ do
      expr <- checkedAndNormalized parser typer normalizer [i|
              -- Note the *lack* of timezone
              $(+0300)
            |]
      expr `shouldBe` sendEmbed (DhTimeZoneLit $ hoursToTimeZone 3)
    it "should typecheck literals" $ do
      good <- parsed parser [i|
              $(-0700) : TimeZone
            |]
      shouldBeRight $ typeOf good

      bad <- parsed parser [i|
              $(-0700) : LocalTime
            |]
      shouldBeLeft $ typeOf bad
  describe "LocalTime/dayOfWeek" $ do
    it "should correctly calculate" $ do
      expr <- checkedAndNormalized parser typer normalizer [i|
        LocalTime/dayOfWeek $(2017-12-04T00:00:00)
      |]
      expr `shouldBe` Dh.IntegerLit 1
    it "should handle poorly-typed expressions" $ do
      expr <- parsed parser [i|
        LocalTime/dayOfWeek $(2017-12-04T00:00:00Z)
      |]
      shouldBeLeft $ typeOf expr
  describe "UTCTime/toLocalTime" $ do
    it "should correctly calculate" $ do
      expr <- checkedAndNormalized parser typer normalizer [i|
        UTCTime/toLocalTime $(+0000) $(2017-12-04T00:00:00Z)
      |]
      expr `shouldBe` sendEmbed (DhLocalTimeLit (LocalTime (fromGregorian 2017 12 4) midnight))
    it "should handle poorly-typed expressions" $ do
      expr <- parsed parser [i|
        UTCTime/toLocalTime +0 $(2017-12-04T00:00:00Z)
      |]
      shouldBeLeft $ typeOf expr
  describe "LocalTime/timeOfDay" $ do
    it "should correctly calculate" $ do
      expr <- checkedAndNormalized parser typer normalizer [i|
        LocalTime/timeOfDay $(2017-12-04T00:00:00)
      |]
      let expected = Dh.RecordLit $ HMI.fromList
           [ ("hour", Dh.IntegerLit 0)
           , ("minute", Dh.IntegerLit 0)
           ]
      expr `shouldBe` expected
    it "should handle poorly-typed expressions" $ do
      expr <- parsed parser [i|
        LocalTime/timeOfDay $(2017-12-04T00:00:00Z)
      |]
      shouldBeLeft $ typeOf expr

typeOf :: OpenExpr Dh.Src '[DhTime] -> Either (Dh.TypeError Dh.Src (Open '[DhTime])) (OpenExpr Dh.Src '[DhTime])
typeOf = Dh.typeWithA typer Dh.empty

parser :: OpenParser '[DhTime]
parser = Dalek.Exts.Time.Parser.parser

typer :: Dh.Typer (Open '[DhTime])
typer = toTyper $ sendTyper Dalek.Exts.Time.TypeCheck.typer

normalizer :: OpenNormalizer '[DhTime]
normalizer = Dalek.Exts.Time.Core.normalizer
