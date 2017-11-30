{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhall.Ord.Natural.TheSpec (spec) where

import           SpecUtils

import           Data.String.Interpolate     (i)
import qualified Text.Trifecta.Parser        as Tri
import qualified Text.Trifecta.Result        as Tri

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
    let expr (x :: Natural) = checked [i|

    if Natural/LT +#{x} +3 then "#t" else "#f"

    |]
    fmap ordNormalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap ordNormalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should work with application" $ do
    let expr (x :: Natural) = checked [i|

    (\\(x : Natural) -> if Natural/LT x +3 then "#t" else "#f") +#{x}

    |]

    fmap ordNormalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap ordNormalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should handle poorly-typed expressions" $ do
    expr <- parsed [i|

    if Natural/LT "2" +3 then "#t" else "#f"

    |]
    shouldBeLeft $ ordTypeOf expr

ordNormalize :: Dh.Expr Dh.Src DhNaturalOrd -> Dh.Expr Dh.Src DhNaturalOrd
ordNormalize = Dh.normalizeWith DhNaturalOrd.normalizer

ordTypeOf :: Dh.Expr Dh.Src DhNaturalOrd -> Either (Dh.TypeError Dh.Src DhNaturalOrd) (Dh.Expr Dh.Src DhNaturalOrd)
ordTypeOf = Dh.typeWithA DhNaturalOrd.typer Dh.empty

parsed :: String -> IO (Dh.Expr Dh.Src DhNaturalOrd)
parsed s = case Tri.parseString (Dh.unParser $ Dh.exprA DhNaturalOrd.parser) mempty s of
  Tri.Success a   -> pure a
  Tri.Failure err -> expectationFailure $ "Bad Parse: " ++ show err

checked :: String -> IO (Dh.Expr Dh.Src DhNaturalOrd)
checked s = do
  expr <- parsed s
  ordTypeOf expr `asRight` (pure . const expr)
