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
import qualified Dhall.Parser                     as Dh
import qualified Dhall.TypeCheck                  as Dh

import           Dalek.Core
import           Dalek.Parser
import           Dalek.TypeCheck

import           Dalek.Exts.Ord.Integer.Core      (DhIntegerOrd (..))
import qualified Dalek.Exts.Ord.Integer.Core      as DhIntegerOrd
import qualified Dalek.Exts.Ord.Integer.Parser    as DhIntegerOrd
import qualified Dalek.Exts.Ord.Integer.TypeCheck as DhIntegerOrd

parser :: OpenParser Dh.Src '[DhIntegerOrd]
parser = DhIntegerOrd.parser

typer :: Dh.Typer Dh.Src (Open Dh.Src '[DhIntegerOrd])
typer = toTyper $ sendTyper DhIntegerOrd.typer

normalizer :: OpenNormalizer Dh.Src '[DhIntegerOrd]
normalizer = DhIntegerOrd.normalizer

normalize :: OpenExpr Dh.Src '[DhIntegerOrd] -> OpenExpr Dh.Src '[DhIntegerOrd]
normalize = Dh.normalizeWith normalizer

-- TODO: Property-based tests
-- TODO: Abstraction instead of copy-paste
spec :: Spec
spec = describe "end-to-end" $ do
  it "should work with literals" $ do
    let expr (x :: Integer) = checked parser typer [i|

    if Integer/LT #{x} 3 then "#t" else "#f"

    |]
    fmap normalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should work with application" $ do
    let expr (x :: Integer) = checked parser typer [i|

    (\\(x : Integer) -> if Integer/LT x 3 then "#t" else "#f") #{x}

    |]

    fmap normalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should handle poorly-typed expressions" $ do
    expr <- parsed parser [i|

    if Integer/LT "2" 3 then "#t" else "#f"

    |]
    shouldBeLeft $ Dh.typeWithA typer Dh.empty expr
