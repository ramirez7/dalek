{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Dalek.Exts.Ord.Double.Core where

import qualified Dhall.Core          as Dh

import           Data.Text.Buildable (Buildable (..))
import           Dalek.Patterns

data DhDoubleOrd =
    DhDoubleEQ
  | DhDoubleNEQ
  | DhDoubleLT
  | DhDoubleLTE
  | DhDoubleGT
  | DhDoubleGTE
  deriving (Eq, Show, Enum, Bounded)

toCmp :: DhDoubleOrd -> (Double -> Double -> Bool)
toCmp = \case
  DhDoubleEQ -> (==)
  DhDoubleNEQ -> (/=)
  DhDoubleLT -> (<)
  DhDoubleLTE -> (<=)
  DhDoubleGT -> (>)
  DhDoubleGTE -> (>=)

normalizer :: Dh.Normalizer s DhDoubleOrd
normalizer = \case
  Apps [E dhOrd, x, y] -> fmap Dh.BoolLit (cmpWith (toCmp dhOrd) x y)
  _ -> Nothing

-- | This assumes normalization of expressions
-- Returns Nothing if it is ill-typed and can't be normalized
cmpWith :: (Double -> Double -> Bool) -> Dh.Expr s a -> Dh.Expr s a -> Maybe Bool
cmpWith cmp lhs rhs = case (lhs, rhs) of
  (Dh.DoubleLit x, Dh.DoubleLit y) -> Just $ cmp x y
  _                                -> Nothing

instance Buildable DhDoubleOrd where
  build = \case
    DhDoubleEQ -> "Double/EQ"
    DhDoubleNEQ -> "Double/NEQ"
    DhDoubleLT -> "Double/LT"
    DhDoubleLTE -> "Double/LTE"
    DhDoubleGT -> "Double/GT"
    DhDoubleGTE -> "Double/GTE"
