{-# LANGUAGE OverloadedStrings #-}

module Dhall.Ord.Core.OrdCoreSpec (spec) where

import           Test.Hspec

import qualified Dhall.Core     as Dh
import           Dhall.Ord.Core (DhOrd (..))
import qualified Dhall.Ord.Core as DhOrd

spec :: Spec
spec = do
  describe "normalizer" $ do
    it "should work with literals" $ do
      let expr x =
            (Dh.BoolIf (mkCompare DhEQ (Dh.IntegerLit x) (Dh.IntegerLit 3))
                       (Dh.TextLit "#t")
                       (Dh.TextLit "#f"))
      ordNormalize (expr 3) `shouldBe` Dh.TextLit "#t"
      ordNormalize (expr 2) `shouldBe` Dh.TextLit "#f"
    it "should handle application" $ do
      let expr x =
            (Dh.App
              (Dh.Lam "x" Dh.Integer
                (Dh.BoolIf (mkCompare DhEQ (Dh.Var $ Dh.V "x" 0) (Dh.IntegerLit 3))
                           (Dh.TextLit "#t")
                           (Dh.TextLit "#f")))
                (Dh.IntegerLit x))
      ordNormalize (expr 3) `shouldBe` Dh.TextLit "#t"
      ordNormalize (expr 2) `shouldBe` Dh.TextLit "#f"

mkCompare :: DhOrd -> Dh.Expr () DhOrd -> Dh.Expr () DhOrd -> Dh.Expr () DhOrd
mkCompare dhOrd x y = (Dh.App (Dh.App (Dh.Embed dhOrd) x) y)

ordNormalize :: Dh.Expr () DhOrd -> Dh.Expr () DhOrd
ordNormalize = Dh.normalizeWith DhOrd.normalizer
