{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhall.Ord.Integer.TheSpec (spec) where

import           SpecUtils

import           Data.String.Interpolate     (i)
import qualified Text.Trifecta.Parser        as Tri
import qualified Text.Trifecta.Result        as Tri

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
    let expr (x :: Integer) = checked [i|

    if Integer/LT #{x} 3 then "#t" else "#f"

    |]
    fmap ordNormalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap ordNormalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should work with application" $ do
    let expr (x :: Integer) = checked [i|

    (\\(x : Integer) -> if Integer/LT x 3 then "#t" else "#f") #{x}

    |]

    fmap ordNormalize (expr 3) `shouldBeIO` Dh.TextLit "#f"
    fmap ordNormalize (expr 2) `shouldBeIO` Dh.TextLit "#t"
  it "should handle poorly-typed expressions" $ do
    expr <- parsed [i|

    if Integer/LT "2" 3 then "#t" else "#f"

    |]
    shouldBeLeft $ ordTypeOf expr

ordNormalize :: Dh.Expr Dh.Src DhIntegerOrd -> Dh.Expr Dh.Src DhIntegerOrd
ordNormalize = Dh.normalizeWith DhIntegerOrd.normalizer

ordTypeOf :: Dh.Expr Dh.Src DhIntegerOrd -> Either (Dh.TypeError Dh.Src DhIntegerOrd) (Dh.Expr Dh.Src DhIntegerOrd)
ordTypeOf = Dh.typeWithA DhIntegerOrd.typer Dh.empty

parsed :: String -> IO (Dh.Expr Dh.Src DhIntegerOrd)
parsed s = case Tri.parseString (Dh.unParser $ Dh.exprA DhIntegerOrd.parser) mempty s of
  Tri.Success a   -> pure a
  Tri.Failure err -> expectationFailure $ "Bad Parse: " ++ show err

checked :: String -> IO (Dh.Expr Dh.Src DhIntegerOrd)
checked s = do
  expr <- parsed s
  ordTypeOf expr `asRight` (pure . const expr)
