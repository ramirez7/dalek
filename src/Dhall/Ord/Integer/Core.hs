{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Ord.Integer.Core where

import qualified Dhall.Core             as Dh

import           Data.Text.Buildable    (Buildable (..))

data DhIntegerOrd =
    DhIntegerEQ
  | DhIntegerNEQ
  | DhIntegerLT
  | DhIntegerLTE
  | DhIntegerGT
  | DhIntegerGTE
  deriving (Eq, Show, Enum, Bounded)

toCmp :: DhIntegerOrd -> (Integer -> Integer -> Bool)
toCmp = \case
  DhIntegerEQ -> (==)
  DhIntegerNEQ -> (/=)
  DhIntegerLT -> (<)
  DhIntegerLTE -> (<=)
  DhIntegerGT -> (>)
  DhIntegerGTE -> (>=)

normalizer :: Dh.Normalizer DhIntegerOrd
normalizer = \case
  Dh.App (Dh.App (Dh.Embed dhOrd) x) y -> fmap Dh.BoolLit (cmpWith (toCmp dhOrd) x y)
  _ -> Nothing

-- | This assumes normalization of expressions
-- Returns Nothing if it is ill-typed and can't be normalized
cmpWith :: (Integer -> Integer -> Bool) -> Dh.Expr s a -> Dh.Expr s a -> Maybe Bool
cmpWith cmp lhs rhs = case (lhs, rhs) of
  (Dh.IntegerLit x, Dh.IntegerLit y) -> Just $ cmp x y
  _                                -> Nothing

instance Buildable DhIntegerOrd where
  build = \case
    DhIntegerEQ -> "Integer/EQ"
    DhIntegerNEQ -> "Integer/NEQ"
    DhIntegerLT -> "Integer/LT"
    DhIntegerLTE -> "Integer/LTE"
    DhIntegerGT -> "Integer/GT"
    DhIntegerGTE -> "Integer/GTE"
