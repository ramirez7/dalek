{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedLists       #-}

module Dalek.Exts.Map.TheSpec where

import           Dalek.SpecUtils

import           Control.Applicative       ((<|>))

import qualified Dhall.Context
import qualified Dhall.Core                as Dh
import qualified Dhall.Parser              as Dh
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

parser :: OpenParser Dh.Src '[DhTime, DhMap]
parser = Dalek.Exts.Map.Parser.parser <|> Dalek.Exts.Time.Parser.parser

typer :: Dh.Typer Dh.Src (Open Dh.Src '[DhTime, DhMap])
typer = toTyper $ typerUnion Dalek.Exts.Time.TypeCheck.typer (sendTyper Dalek.Exts.Map.TypeCheck.typer)

normalize :: OpenNormalizer Dh.Src '[DhTime, DhMap]
normalize = Dalek.Exts.Time.Core.normalizer .<|> Dalek.Exts.Map.Core.normalizer

spec :: Spec
spec = describe "end-to-end" $ do
  it "..." $ do
    expr <- checked parser typer [i|
            let k = $(1776-07-04T00:00:00Z)
         in let m = Map/insert UTCTime Natural k +3 (Map/empty UTCTime Natural)
         in Map/lookup UTCTime Natural k m
          |]
    Dh.normalizeWith normalize expr `shouldBe` Dh.OptionalLit Dh.Natural [Dh.NaturalLit 3]
