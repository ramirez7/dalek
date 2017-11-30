{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhall.Ord.Double.TheSpec (spec) where

import           SpecUtils

import           Data.String.Interpolate    (i)
import qualified Text.Trifecta.Parser       as Tri
import qualified Text.Trifecta.Result       as Tri

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
    let expr (x :: Double) = checked [i|

    if Double/LT #{x} 3.0 then "#t" else "#f"

    |]
    fmap ordNormalize (expr 3.0) `shouldBeIO` Dh.TextLit "#f"
    fmap ordNormalize (expr 2.0) `shouldBeIO` Dh.TextLit "#t"
  it "should work with application" $ do
    let expr (x :: Double) = checked [i|

    (\\(x : Double) -> if Double/LT x 3.0 then "#t" else "#f") #{x}

    |]

    fmap ordNormalize (expr 3.0) `shouldBeIO` Dh.TextLit "#f"
    fmap ordNormalize (expr 2.0) `shouldBeIO` Dh.TextLit "#t"
  it "should handle poorly-typed expressions" $ do
    expr <- parsed [i|

    if Double/LT "2.0" 3.0 then "#t" else "#f"

    |]
    shouldBeLeft $ ordTypeOf expr

ordNormalize :: Dh.Expr Dh.Src DhDoubleOrd -> Dh.Expr Dh.Src DhDoubleOrd
ordNormalize = Dh.normalizeWith DhDoubleOrd.normalizer

ordTypeOf :: Dh.Expr Dh.Src DhDoubleOrd -> Either (Dh.TypeError Dh.Src DhDoubleOrd) (Dh.Expr Dh.Src DhDoubleOrd)
ordTypeOf = Dh.typeWithA DhDoubleOrd.typer Dh.empty

parsed :: String -> IO (Dh.Expr Dh.Src DhDoubleOrd)
parsed s = case Tri.parseString (Dh.unParser $ Dh.exprA DhDoubleOrd.parser) mempty s of
  Tri.Success a   -> pure a
  Tri.Failure err -> expectationFailure $ "Bad Parse: " ++ show err

checked :: String -> IO (Dh.Expr Dh.Src DhDoubleOrd)
checked s = do
  expr <- parsed s
  ordTypeOf expr `asRight` (pure . const expr)
