{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Dalek.Exts.Map.TheSpec where

import           Dalek.SpecUtils

import           Control.Applicative       ((<|>))

import qualified Dhall.Core                as Dh
import qualified Dhall.TypeCheck           as Dh

import           Dalek.Core
import           Dalek.Exts.Map.Core       (DhMap (..))
import qualified Dalek.Exts.Map.Core
import qualified Dalek.Exts.Map.Parser
import qualified Dalek.Exts.Map.TypeCheck
import           Dalek.Exts.Time.Core      (DhTime)
import qualified Dalek.Exts.Time.Core
import qualified Dalek.Exts.Time.Parser
import qualified Dalek.Exts.Time.TypeCheck
import           Dalek.Parser
import           Dalek.TypeCheck

parser :: OpenParser '[DhTime, DhMap]
parser = Dalek.Exts.Map.Parser.parser <|> Dalek.Exts.Time.Parser.parser

typer :: Dh.Typer (Open '[DhTime, DhMap])
typer = toTyper $ typerUnion Dalek.Exts.Time.TypeCheck.typer (sendTyper Dalek.Exts.Map.TypeCheck.typer)

normalizer :: OpenNormalizer '[DhTime, DhMap]
normalizer = Dalek.Exts.Time.Core.normalizer .<|> Dalek.Exts.Map.Core.normalizer

normalize :: Dh.Expr s (Open '[DhTime, DhMap]) -> Dh.Expr () (Open '[DhTime, DhMap])
normalize = Dh.normalizeWith normalizer

spec :: Spec
spec = describe "end-to-end" $ do
  it "proof of concept" $ do
    expr <- checkedAndNormalized parser typer normalizer [i|
            let k = $(1776-07-04T00:00:00Z)
         in let m = Map/insert UTCTime Natural k +3 (Map/empty UTCTime Natural)
         in Map/lookup UTCTime Natural k m
          |]
    expr `shouldBe` Dh.OptionalLit Dh.Natural (Just $ Dh.NaturalLit 3)
