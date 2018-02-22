{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dalek.Exts.Ord.Double.TheSpec (spec) where

import           SpecUtils

import           Data.String.Interpolate          (i)

import qualified Dhall.Context                    as Dh
import qualified Dhall.Core                       as Dh
import qualified Dhall.Parser                     as Dh
import qualified Dhall.TypeCheck                  as Dh

import           Dalek.Core
import           Dalek.Parser
import           Dalek.TypeCheck

import           Dalek.Exts.Ord.Double.Core      (DhDoubleOrd (..))
import qualified Dalek.Exts.Ord.Double.Core      as DhDoubleOrd
import qualified Dalek.Exts.Ord.Double.Parser    as DhDoubleOrd
import qualified Dalek.Exts.Ord.Double.TypeCheck as DhDoubleOrd

parser :: OpenParser Dh.Src '[DhDoubleOrd]
parser = DhDoubleOrd.parser

typer :: Dh.Typer Dh.Src (Open Dh.Src '[DhDoubleOrd])
typer = toTyper $ sendTyper DhDoubleOrd.typer

normalizer :: OpenNormalizer Dh.Src '[DhDoubleOrd]
normalizer = DhDoubleOrd.normalizer

normalize :: OpenExpr Dh.Src '[DhDoubleOrd] -> OpenExpr Dh.Src '[DhDoubleOrd]
normalize = Dh.normalizeWith normalizer

-- TODO: Property-based tests
-- TODO: Abstraction instead of copy-paste
spec :: Spec
spec = describe "end-to-end" $ do
  it "should work with literals" $ do
    let expr (x :: Double) = checked parser typer [i|

    if Double/LT #{x} 3.0 then "#t" else "#f"

    |]
    fmap normalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should work with application" $ do
    let expr (x :: Double) = checked parser typer [i|

    (\\(x : Double) -> if Double/LT x 3.0 then "#t" else "#f") #{x}

    |]

    fmap normalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should handle poorly-typed expressions" $ do
    expr <- parsed parser [i|

    if Double/LT "2.0" 3.0 then "#t" else "#f"

    |]
    shouldBeLeft $ Dh.typeWithA typer Dh.empty expr
