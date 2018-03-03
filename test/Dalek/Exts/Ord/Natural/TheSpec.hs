{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dalek.Exts.Ord.Natural.TheSpec (spec) where

import           Dalek.SpecUtils

import           Data.String.Interpolate          (i)

import           Dhall                            (Natural)
import qualified Dhall.Context                    as Dh
import qualified Dhall.Core                       as Dh
import qualified Dhall.Parser                     as Dh
import qualified Dhall.TypeCheck                  as Dh

import           Dalek.Core
import           Dalek.Parser
import           Dalek.TypeCheck

import           Dalek.Exts.Ord.Natural.Core      (DhNaturalOrd (..))
import qualified Dalek.Exts.Ord.Natural.Core      as DhNaturalOrd
import qualified Dalek.Exts.Ord.Natural.Parser    as DhNaturalOrd
import qualified Dalek.Exts.Ord.Natural.TypeCheck as DhNaturalOrd

parser :: OpenParser Dh.Src '[DhNaturalOrd]
parser = DhNaturalOrd.parser

typer :: Dh.Typer Dh.Src (Open Dh.Src '[DhNaturalOrd])
typer = toTyper $ sendTyper DhNaturalOrd.typer

normalizer :: OpenNormalizer Dh.Src '[DhNaturalOrd]
normalizer = DhNaturalOrd.normalizer

normalize :: OpenExpr Dh.Src '[DhNaturalOrd] -> OpenExpr Dh.Src '[DhNaturalOrd]
normalize = Dh.normalizeWith normalizer

-- TODO: Property-based tests
-- TODO: Abstraction instead of copy-paste
spec :: Spec
spec = describe "end-to-end" $ do
  it "should work with literals" $ do
    let expr (x :: Natural) = checked parser typer [i|

    if Natural/LT +#{x} +3 then "#t" else "#f"

    |]
    fmap normalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should work with application" $ do
    let expr (x :: Natural) = checked parser typer [i|

    (\\(x : Natural) -> if Natural/LT x +3 then "#t" else "#f") +#{x}

    |]

    fmap normalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should handle poorly-typed expressions" $ do
    expr <- parsed parser [i|

    if Natural/LT "2" +3 then "#t" else "#f"

    |]
    shouldBeLeft $ Dh.typeWithA typer Dh.empty expr
