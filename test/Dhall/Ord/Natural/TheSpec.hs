{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhall.Ord.Natural.TheSpec (spec) where

import           SpecUtils

import           Data.String.Interpolate     (i)

import           Dhall                       (Natural)
import qualified Dhall.Context               as Dh
import qualified Dhall.Core                  as Dh
import qualified Dhall.Parser                as Dh
import qualified Dhall.TypeCheck             as Dh

import           Dhall.Ord.Natural.Core      (DhNaturalOrd (..))
import qualified Dhall.Ord.Natural.Core      as DhNaturalOrd
import qualified Dhall.Ord.Natural.Parser    as DhNaturalOrd
import qualified Dhall.Ord.Natural.TypeCheck as DhNaturalOrd

-- TODO: Property-based tests
-- TODO: Abstraction instead of copy-paste
spec :: Spec
spec = describe "end-to-end" $ do
  it "should work with literals" $ do
    let expr (x :: Natural) = checked DhNaturalOrd.parser DhNaturalOrd.typer [i|

    if Natural/LT +#{x} +3 then "#t" else "#f"

    |]
    fmap normalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should work with application" $ do
    let expr (x :: Natural) = checked DhNaturalOrd.parser DhNaturalOrd.typer [i|

    (\\(x : Natural) -> if Natural/LT x +3 then "#t" else "#f") +#{x}

    |]

    fmap normalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap normalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should handle poorly-typed expressions" $ do
    expr <- parsed DhNaturalOrd.parser [i|

    if Natural/LT "2" +3 then "#t" else "#f"

    |]
    shouldBeLeft $ typeOf expr

normalize :: Dh.Expr Dh.Src DhNaturalOrd -> Dh.Expr Dh.Src DhNaturalOrd
normalize = Dh.normalizeWith DhNaturalOrd.normalizer

typeOf :: Dh.Expr Dh.Src DhNaturalOrd -> Either (Dh.TypeError Dh.Src DhNaturalOrd) (Dh.Expr Dh.Src DhNaturalOrd)
typeOf = Dh.typeWithA DhNaturalOrd.typer Dh.empty
