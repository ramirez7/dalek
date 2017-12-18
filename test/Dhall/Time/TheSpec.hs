{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Dhall.Time.TheSpec (spec) where

import           SpecUtils

import qualified Data.Map                as M
import           Data.String.Interpolate (i)
import           Data.Time

import qualified Dhall.Context           as Dh
import qualified Dhall.Core              as Dh
import qualified Dhall.Parser            as Dh
import qualified Dhall.TypeCheck         as Dh

import           Dhall.Time.Core         (DhTime (..))
import qualified Dhall.Time.Core         as DhTime
import qualified Dhall.Time.Parser       as DhTime
import qualified Dhall.Time.TypeCheck    as DhTime

spec :: Spec
spec = describe "end-to-end" $ do
  describe "UTCTime" $ do
    it "should parse literals" $ do
      expr <- checked DhTime.parser DhTime.typer [i|
              -- Note the timezone
              $(1776-07-04T00:00:00Z)
            |]
      normalize expr `shouldBe` Dh.Embed (DhUTCTimeLit $ UTCTime (fromGregorian 1776 7 4) 0)
      explicitOffset <- checked DhTime.parser DhTime.typer [i|
              $(1776-07-04T00:00:00+0000)
            |]
      normalize explicitOffset `shouldBe` Dh.Embed (DhUTCTimeLit $ UTCTime (fromGregorian 1776 7 4) 0)
    it "should typecheck literals" $ do
      good <- parsed DhTime.parser [i|
              $(1776-07-04T00:00:00Z) : UTCTime
            |]
      shouldBeRight $ typeOf good

      bad <- parsed DhTime.parser [i|
              $(1776-07-04T00:00:00Z) : LocalTime
            |]
      shouldBeLeft $ typeOf bad
  describe "LocalTime" $ do
    it "should parse literals" $ do
      expr <- checked DhTime.parser DhTime.typer [i|
              -- Note the *lack* of timezone
              $(1776-07-04T00:00:00)
            |]
      normalize expr `shouldBe` Dh.Embed (DhLocalTimeLit $ LocalTime (fromGregorian 1776 7 4) midnight)
    it "should typecheck literals" $ do
      good <- parsed DhTime.parser [i|
              $(1776-07-04T00:00:00) : LocalTime
            |]
      shouldBeRight $ typeOf good

      bad <- parsed DhTime.parser [i|
              $(1776-07-04T00:00:00) : UTCTime
            |]
      shouldBeLeft $ typeOf bad
  describe "TimeZone" $ do
    it "should parse literals" $ do
      expr <- checked DhTime.parser DhTime.typer [i|
              -- Note the *lack* of timezone
              $(+0300)
            |]
      normalize expr `shouldBe` Dh.Embed (DhTimeZoneLit $ hoursToTimeZone 3)
    it "should typecheck literals" $ do
      good <- parsed DhTime.parser [i|
              $(-0700) : TimeZone
            |]
      shouldBeRight $ typeOf good

      bad <- parsed DhTime.parser [i|
              $(-0700) : LocalTime
            |]
      shouldBeLeft $ typeOf bad
  describe "LocalTime/dayOfWeek" $ do
    it "should correctly calculate" $ do
      expr <- checked DhTime.parser DhTime.typer [i|
        LocalTime/dayOfWeek $(2017-12-04T00:00:00)
      |]
      normalize expr `shouldBe` Dh.IntegerLit 1
    it "should handle poorly-typed expressions" $ do
      expr <- parsed DhTime.parser [i|
        LocalTime/dayOfWeek $(2017-12-04T00:00:00Z)
      |]
      shouldBeLeft $ typeOf expr
  describe "UTCTime/toLocalTime" $ do
    it "should correctly calculate" $ do
      expr <- checked DhTime.parser DhTime.typer [i|
        UTCTime/toLocalTime $(+0000) $(2017-12-04T00:00:00Z)
      |]
      normalize expr `shouldBe` Dh.Embed (DhLocalTimeLit (LocalTime (fromGregorian 2017 12 4) midnight))
    it "should handle poorly-typed expressions" $ do
      expr <- parsed DhTime.parser [i|
        UTCTime/toLocalTime +0 $(2017-12-04T00:00:00Z)
      |]
      shouldBeLeft $ typeOf expr
  describe "LocalTime/timeOfDay" $ do
    it "should correctly calculate" $ do
      expr <- checked DhTime.parser DhTime.typer [i|
        LocalTime/timeOfDay $(2017-12-04T00:00:00)
      |]
      let expected = Dh.RecordLit $ M.fromList
           [ ("hour", Dh.IntegerLit 0)
           , ("minute", Dh.IntegerLit 0)
           ]
      normalize expr `shouldBe` expected
    it "should handle poorly-typed expressions" $ do
      expr <- parsed DhTime.parser [i|
        LocalTime/timeOfDay $(2017-12-04T00:00:00Z)
      |]
      shouldBeLeft $ typeOf expr

normalize :: Dh.Expr Dh.Src DhTime -> Dh.Expr Dh.Src DhTime
normalize = Dh.normalizeWith DhTime.normalizer

typeOf :: Dh.Expr Dh.Src DhTime -> Either (Dh.TypeError Dh.Src DhTime) (Dh.Expr Dh.Src DhTime)
typeOf = Dh.typeWithA DhTime.typer Dh.empty
