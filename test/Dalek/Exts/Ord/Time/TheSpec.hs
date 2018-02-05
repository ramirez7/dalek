{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}

module Dalek.Exts.Ord.Time.TheSpec (spec) where

import           Dalek.SpecUtils

import           Control.Applicative           ((<|>))

import qualified Dhall.Context
import qualified Dhall.Core                    as Dh
import qualified Dhall.Parser                  as Dh
import qualified Dhall.TypeCheck               as Dh

import           Dalek.Core
import           Dalek.Exts.Ord.Time.Core      (DhUTCTimeOrd (..))
import qualified Dalek.Exts.Ord.Time.Core
import qualified Dalek.Exts.Ord.Time.Parser
import qualified Dalek.Exts.Ord.Time.TypeCheck
import           Dalek.Exts.Time.Core          (DhTime)
import qualified Dalek.Exts.Time.Core
import qualified Dalek.Exts.Time.Parser
import qualified Dalek.Exts.Time.TypeCheck
import           Dalek.Parser
import           Dalek.TypeCheck


parser :: OpenParser Dh.Src '[DhTime, DhUTCTimeOrd]
parser = Dalek.Exts.Ord.Time.Parser.parser <|> Dalek.Exts.Time.Parser.parser

typer :: Dh.Typer Dh.Src (Open Dh.Src '[DhTime, DhUTCTimeOrd])
typer = toTyper $ typerUnion Dalek.Exts.Time.TypeCheck.typer (sendTyper Dalek.Exts.Ord.Time.TypeCheck.typer)

normalize :: OpenNormalizer Dh.Src '[DhTime, DhUTCTimeOrd]
normalize = Dalek.Exts.Time.Core.normalizer .<|> Dalek.Exts.Ord.Time.Core.normalizer

spec :: Spec
spec = do
  describe "end-to-end" $ do
    it "should work" $ do
      expr <- checked parser typer [i|
              UTCTime/LT $(1776-07-04T00:00:00Z) $(2018-01-01T00:00:00Z)
            |]
      Dh.normalizeWith normalize expr `shouldBe` Dh.BoolLit True
    it "should type" $ do
      expr <- parsed parser [i|
        UTCTime/LT : UTCTime -> UTCTime -> Bool
      |]
      shouldBeRight $ Dh.typeWithA typer Dhall.Context.empty expr
      Dh.normalizeWith normalize expr `shouldBe` sendEmbed DhUTCTimeLT
