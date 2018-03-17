{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}

module Dalek.Exts.Ord.Time.TheSpec (spec) where

import           Dalek.SpecUtils

import           Control.Applicative           ((<|>))

import qualified Dhall.Context
import qualified Dhall.Core                    as Dh
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


parser :: OpenParser '[DhTime, DhUTCTimeOrd]
parser = Dalek.Exts.Ord.Time.Parser.parser <|> Dalek.Exts.Time.Parser.parser

typer :: Dh.Typer (Open '[DhTime, DhUTCTimeOrd])
typer = toTyper $ typerUnion Dalek.Exts.Time.TypeCheck.typer (sendTyper Dalek.Exts.Ord.Time.TypeCheck.typer)

normalizer :: OpenNormalizer '[DhTime, DhUTCTimeOrd]
normalizer = Dalek.Exts.Time.Core.normalizer .<|> Dalek.Exts.Ord.Time.Core.normalizer

normalize :: Dh.Expr s (Open '[DhTime, DhUTCTimeOrd]) -> Dh.Expr () (Open '[DhTime, DhUTCTimeOrd])
normalize = Dh.normalizeWith normalizer

spec :: Spec
spec = do
  describe "end-to-end" $ do
    it "should work" $ do
      expr <- checkedAndNormalized parser typer normalizer [i|
              UTCTime/LT $(1776-07-04T00:00:00Z) $(2018-01-01T00:00:00Z)
            |]
      expr `shouldBe` Dh.BoolLit True
    it "should type" $ do
      expr <- parsed parser [i|
        UTCTime/LT : UTCTime -> UTCTime -> Bool
      |]
      shouldBeRight $ Dh.typeWithAN normalizer typer Dhall.Context.empty expr
      normalize expr `shouldBe` sendEmbed DhUTCTimeLT
