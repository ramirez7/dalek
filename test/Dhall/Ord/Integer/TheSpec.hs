{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhall.Ord.Integer.TheSpec (spec) where

import           SpecUtils

import           Data.String.Interpolate     (i)

import qualified Dhall.Context               as Dh
import qualified Dhall.Core                  as Dh
import qualified Dhall.Parser                as Dh
import qualified Dhall.TypeCheck             as Dh

import           Dhall.Ord.Integer.Core      (DhIntegerOrd (..))
import qualified Dhall.Ord.Integer.Core      as DhIntegerOrd
import qualified Dhall.Ord.Integer.Parser    as DhIntegerOrd
import qualified Dhall.Ord.Integer.TypeCheck as DhIntegerOrd

-- TODO: Property-based tests
-- TODO: Abstraction instead of copy-paste
spec :: Spec
spec = describe "end-to-end" $ do
  it "should work with literals" $ do
    let expr (x :: Integer) = checked DhIntegerOrd.parser DhIntegerOrd.typer [i|

    if Integer/LT #{x} 3 then "#t" else "#f"

    |]
    fmap normalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should work with application" $ do
    let expr (x :: Integer) = checked DhIntegerOrd.parser DhIntegerOrd.typer [i|

    (\\(x : Integer) -> if Integer/LT x 3 then "#t" else "#f") #{x}

    |]

    fmap normalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should handle poorly-typed expressions" $ do
    expr <- parsed DhIntegerOrd.parser [i|

    if Integer/LT "2" 3 then "#t" else "#f"

    |]
    shouldBeLeft $ typeOf expr

normalize :: Dh.Expr Dh.Src DhIntegerOrd -> Dh.Expr Dh.Src DhIntegerOrd
normalize = Dh.normalizeWith DhIntegerOrd.normalizer

typeOf :: Dh.Expr Dh.Src DhIntegerOrd -> Either (Dh.TypeError Dh.Src DhIntegerOrd) (Dh.Expr Dh.Src DhIntegerOrd)
typeOf = Dh.typeWithA DhIntegerOrd.typer Dh.empty
