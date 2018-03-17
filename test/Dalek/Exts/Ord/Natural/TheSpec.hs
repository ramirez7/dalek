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
import qualified Dhall.TypeCheck                  as Dh

import           Dalek.Core
import           Dalek.Parser
import           Dalek.TypeCheck

import           Dalek.Exts.Ord.Natural.Core      (DhNaturalOrd (..))
import qualified Dalek.Exts.Ord.Natural.Core      as DhNaturalOrd
import qualified Dalek.Exts.Ord.Natural.Parser    as DhNaturalOrd
import qualified Dalek.Exts.Ord.Natural.TypeCheck as DhNaturalOrd

parser :: OpenParser '[DhNaturalOrd]
parser = DhNaturalOrd.parser

typer :: Dh.Typer (Open '[DhNaturalOrd])
typer = toTyper $ sendTyper DhNaturalOrd.typer

normalizer :: OpenNormalizer '[DhNaturalOrd]
normalizer = DhNaturalOrd.normalizer

-- TODO: Property-based tests
-- TODO: Abstraction instead of copy-paste
spec :: Spec
spec = describe "end-to-end" $ do
  it "should work with literals" $ do
    let expr (x :: Natural) = checkedAndNormalized parser typer normalizer [i|

    if Natural/LT +#{x} +3 then "#t" else "#f"

    |]
    expr 3 `shouldBeIO` Dh.TextLit "#f"
    expr 2 `shouldBeIO` Dh.TextLit "#t"
  it "should work with application" $ do
    let expr (x :: Natural) = checkedAndNormalized parser typer normalizer [i|

    (\\(x : Natural) -> if Natural/LT x +3 then "#t" else "#f") +#{x}

    |]

    expr 3 `shouldBeIO` Dh.TextLit "#f"
    expr 2 `shouldBeIO` Dh.TextLit "#t"
  it "should handle poorly-typed expressions" $ do
    expr <- parsed parser [i|

    if Natural/LT "2" +3 then "#t" else "#f"

    |]
    shouldBeLeft $ Dh.typeWithAN normalizer typer Dh.empty expr
