{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhall.Ord.Double.TheSpec (spec) where

import           SpecUtils

import           Data.String.Interpolate    (i)

import qualified Dhall.Context              as Dh
import qualified Dhall.Core                 as Dh
import qualified Dhall.Parser               as Dh
import qualified Dhall.TypeCheck            as Dh

import           Dhall.Ord.Double.Core      (DhDoubleOrd (..))
import qualified Dhall.Ord.Double.Core      as DhDoubleOrd
import qualified Dhall.Ord.Double.Parser    as DhDoubleOrd
import qualified Dhall.Ord.Double.TypeCheck as DhDoubleOrd

-- TODO: Property-based tests
-- TODO: Abstraction instead of copy-paste
spec :: Spec
spec = describe "end-to-end" $ do
  it "should work with literals" $ do
    let expr (x :: Double) = checked DhDoubleOrd.parser DhDoubleOrd.typer [i|

    if Double/LT #{x} 3.0 then "#t" else "#f"

    |]
    fmap normalize (expr 3.0) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2.0) `shouldBeIO` Dh.TextLit "#t"
  it "should work with application" $ do
    let expr (x :: Double) = checked DhDoubleOrd.parser DhDoubleOrd.typer [i|

    (\\(x : Double) -> if Double/LT x 3.0 then "#t" else "#f") #{x}

    |]

    fmap normalize (expr 3.0) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2.0) `shouldBeIO` Dh.TextLit "#t"
  it "should handle poorly-typed expressions" $ do
    expr <- parsed DhDoubleOrd.parser [i|

    if Double/LT "2.0" 3.0 then "#t" else "#f"

    |]
    shouldBeLeft $ typeOf expr

normalize :: Dh.Expr Dh.Src DhDoubleOrd -> Dh.Expr Dh.Src DhDoubleOrd
normalize = Dh.normalizeWith DhDoubleOrd.normalizer

typeOf :: Dh.Expr Dh.Src DhDoubleOrd -> Either (Dh.TypeError Dh.Src DhDoubleOrd) (Dh.Expr Dh.Src DhDoubleOrd)
typeOf = Dh.typeWithA DhDoubleOrd.typer Dh.empty
