{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dalek.Exts.Ord.Integer.TheSpec (spec) where

import           Dalek.SpecUtils

import           Data.String.Interpolate          (i)

import qualified Dhall.Context                    as Dh
import qualified Dhall.Core                       as Dh
import qualified Dhall.TypeCheck                  as Dh

import           Dalek.Core
import           Dalek.Parser
import           Dalek.TypeCheck

import           Dalek.Exts.Ord.Integer.Core      (DhIntegerOrd (..))
import qualified Dalek.Exts.Ord.Integer.Core      as DhIntegerOrd
import qualified Dalek.Exts.Ord.Integer.Parser    as DhIntegerOrd
import qualified Dalek.Exts.Ord.Integer.TypeCheck as DhIntegerOrd

parser :: OpenParser '[DhIntegerOrd]
parser = DhIntegerOrd.parser

typer :: Dh.Typer (Open '[DhIntegerOrd])
typer = toTyper $ sendTyper DhIntegerOrd.typer

normalizer :: OpenNormalizer '[DhIntegerOrd]
normalizer = DhIntegerOrd.normalizer

-- TODO: Property-based tests
-- TODO: Abstraction instead of copy-paste
spec :: Spec
spec = describe "end-to-end" $ do
  it "should work with literals" $ do
    let expr (x :: Integer) = checkedAndNormalized parser typer normalizer [i|

    if Integer/LT #{x} 3 then "#t" else "#f"

    |]
    expr 3 `shouldBeIO` Dh.TextLit "#f"
    expr 2 `shouldBeIO` Dh.TextLit "#t"
  it "should work with application" $ do
    let expr (x :: Integer) = checkedAndNormalized parser typer normalizer [i|

    (\\(x : Integer) -> if Integer/LT x 3 then "#t" else "#f") #{x}

    |]

    expr 3 `shouldBeIO` Dh.TextLit "#f"
    expr 2 `shouldBeIO` Dh.TextLit "#t"
  it "should handle poorly-typed expressions" $ do
    expr <- parsed parser [i|

    if Integer/LT "2" 3 then "#t" else "#f"

    |]
    shouldBeLeft $ Dh.typeWithAN normalizer typer Dh.empty expr
