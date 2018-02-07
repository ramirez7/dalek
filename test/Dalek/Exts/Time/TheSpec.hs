{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Dalek.Exts.Time.TheSpec (spec) where

import           Dalek.SpecUtils

import qualified Data.Map                  as M
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
      expr <- checked parser typer [i|
              -- Note the timezone
              $(1776-07-04T00:00:00Z)
            |]
      normalize expr `shouldBe` sendEmbed (DhUTCTimeLit $ UTCTime (fromGregorian 1776 7 4) 0)
      explicitOffset <- checked parser typer [i|
              $(1776-07-04T00:00:00+0000)
            |]
      normalize explicitOffset `shouldBe` sendEmbed (DhUTCTimeLit $ UTCTime (fromGregorian 1776 7 4) 0)
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
      expr <- checked parser typer [i|
              -- Note the *lack* of timezone
              $(1776-07-04T00:00:00)
            |]
      normalize expr `shouldBe` sendEmbed (DhLocalTimeLit $ LocalTime (fromGregorian 1776 7 4) midnight)
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
      expr <- checked parser typer [i|
              -- Note the *lack* of timezone
              $(+0300)
            |]
      normalize expr `shouldBe` sendEmbed (DhTimeZoneLit $ hoursToTimeZone 3)
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
      expr <- checked parser typer [i|
        LocalTime/dayOfWeek $(2017-12-04T00:00:00)
      |]
      normalize expr `shouldBe` Dh.IntegerLit 1
    it "should handle poorly-typed expressions" $ do
      expr <- parsed parser [i|
        LocalTime/dayOfWeek $(2017-12-04T00:00:00Z)
      |]
      shouldBeLeft $ typeOf expr
  describe "UTCTime/toLocalTime" $ do
    it "should correctly calculate" $ do
      expr <- checked parser typer [i|
        UTCTime/toLocalTime $(+0000) $(2017-12-04T00:00:00Z)
      |]
      normalize expr `shouldBe` sendEmbed (DhLocalTimeLit (LocalTime (fromGregorian 2017 12 4) midnight))
    it "should handle poorly-typed expressions" $ do
      expr <- parsed parser [i|
        UTCTime/toLocalTime +0 $(2017-12-04T00:00:00Z)
      |]
      shouldBeLeft $ typeOf expr
  describe "LocalTime/timeOfDay" $ do
    it "should correctly calculate" $ do
      expr <- checked parser typer [i|
        LocalTime/timeOfDay $(2017-12-04T00:00:00)
      |]
      let expected = Dh.RecordLit $ M.fromList
           [ ("hour", Dh.IntegerLit 0)
           , ("minute", Dh.IntegerLit 0)
           ]
      normalize expr `shouldBe` expected
    it "should handle poorly-typed expressions" $ do
      expr <- parsed parser [i|
        LocalTime/timeOfDay $(2017-12-04T00:00:00Z)
      |]
      shouldBeLeft $ typeOf expr

normalize :: OpenExpr Dh.Src '[DhTime] -> OpenExpr Dh.Src '[DhTime]
normalize = Dh.normalizeWith normalizer

typeOf :: OpenExpr Dh.Src '[DhTime] -> Either (Dh.TypeError Dh.Src (Open Dh.Src '[DhTime])) (OpenExpr Dh.Src '[DhTime])
typeOf = Dh.typeWithA typer Dh.empty

parser :: OpenParser Dh.Src '[DhTime]
parser = Dalek.Exts.Time.Parser.parser

typer :: Dh.Typer Dh.Src (Open Dh.Src '[DhTime])
typer = toTyper $ sendTyper Dalek.Exts.Time.TypeCheck.typer

normalizer :: OpenNormalizer Dh.Src '[DhTime]
normalizer = Dalek.Exts.Time.Core.normalizer
