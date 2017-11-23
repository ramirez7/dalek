{-# LANGUAGE OverloadedStrings #-}

module Dhall.Ord.Core.OrdCoreSpec (spec) where

import           Test.Hspec

import qualified Dhall.Core     as Dh
import           Dhall.Ord.Core (DhOrd (..))
import qualified Dhall.Ord.Core as DhOrd

spec :: Spec
spec = do
  describe "normalize" $ do
    it "should work with Doubles" $ do
      DhOrd.normalize (DhEQ (Dh.DoubleLit 1.0) (Dh.DoubleLit 1.0)) `shouldBe` dh (Dh.BoolLit True)
    it "should work with Integers" $ do
      DhOrd.normalize (DhEQ (Dh.IntegerLit 1) (Dh.IntegerLit 1)) `shouldBe` dh (Dh.BoolLit True)
    it "should work with Naturals" $ do
      DhOrd.normalize (DhEQ (Dh.NaturalLit 1) (Dh.NaturalLit 1)) `shouldBe` dh (Dh.BoolLit True)
    it "should do nothing when poorly typed" $ do
      let expr = DhEQ (Dh.IntegerLit 1) (Dh.NaturalLit 1)
      DhOrd.normalize expr `shouldBe` dh (Dh.Embed expr)

  describe "normalizeEmbedded" $ do
    it "should work with literals" $ do
      let expr x =
            (Dh.BoolIf (Dh.Embed (DhEQ (Dh.IntegerLit x) (Dh.IntegerLit 3)))
                       (Dh.TextLit "#t")
                       (Dh.TextLit "#f"))
      DhOrd.normalizeEmbedded (expr 3) `shouldBe` dh (Dh.TextLit "#t")
      DhOrd.normalizeEmbedded (expr 2) `shouldBe` dh (Dh.TextLit "#f")
    it "should handle application" $ do
      let expr x =
            (Dh.App
              (Dh.Lam "x" Dh.Integer
                (Dh.BoolIf (Dh.Embed (DhEQ (Dh.Var $ Dh.V "x" 0) (Dh.IntegerLit 3)))
                           (Dh.TextLit "#t")
                           (Dh.TextLit "#f")))
                (Dh.IntegerLit x))
      DhOrd.normalizeEmbedded (expr 3) `shouldBe` dh (Dh.TextLit "#t")
      DhOrd.normalizeEmbedded (expr 2) `shouldBe` dh (Dh.TextLit "#f")

-- helper to guide inference
dh :: DhOrd.DhOrdEmbedded () -> DhOrd.DhOrdEmbedded ()
dh = id
