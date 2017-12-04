{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Dhall.Ord.Natural.Core where

import           Dhall               (Natural)
import qualified Dhall.Core          as Dh

import           Dhall.Patterns
import           Data.Text.Buildable (Buildable (..))

data DhNaturalOrd =
    DhNaturalEQ
  | DhNaturalNEQ
  | DhNaturalLT
  | DhNaturalLTE
  | DhNaturalGT
  | DhNaturalGTE
  deriving (Eq, Show, Enum, Bounded)

toCmp :: DhNaturalOrd -> (Natural -> Natural -> Bool)
toCmp = \case
  DhNaturalEQ -> (==)
  DhNaturalNEQ -> (/=)
  DhNaturalLT -> (<)
  DhNaturalLTE -> (<=)
  DhNaturalGT -> (>)
  DhNaturalGTE -> (>=)

normalizer :: Dh.Normalizer DhNaturalOrd
normalizer = \case
  Apps [E dhOrd, x, y] -> fmap Dh.BoolLit (cmpWith (toCmp dhOrd) x y)
  _ -> Nothing

-- | This assumes normalization of expressions
-- Returns Nothing if it is ill-typed and can't be normalized
cmpWith :: (Natural -> Natural -> Bool) -> Dh.Expr s a -> Dh.Expr s a -> Maybe Bool
cmpWith cmp lhs rhs = case (lhs, rhs) of
  (Dh.NaturalLit x, Dh.NaturalLit y) -> Just $ cmp x y
  _                                  -> Nothing

instance Buildable DhNaturalOrd where
  build = \case
    DhNaturalEQ -> "Natural/EQ"
    DhNaturalNEQ -> "Natural/NEQ"
    DhNaturalLT -> "Natural/LT"
    DhNaturalLTE -> "Natural/LTE"
    DhNaturalGT -> "Natural/GT"
    DhNaturalGTE -> "Natural/GTE"
