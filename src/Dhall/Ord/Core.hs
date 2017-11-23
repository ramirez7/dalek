{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dhall.Ord.Core where

import qualified Dhall.Core as Dh

data DhOrd =
    DhEQ
  | DhNEQ
  | DhLT
  | DhLTE
  | DhGT
  | DhGTE
  deriving (Eq, Show)

toCmp :: DhOrd -> (forall x. Ord x => x -> x -> Bool)
toCmp = \case
  DhEQ -> (==)
  DhNEQ -> (/=)
  DhLT -> (<)
  DhLTE -> (<=)
  DhGT -> (>)
  DhGTE -> (>=)

normalizer :: Dh.Normalizer DhOrd
normalizer = \case
  Dh.App (Dh.App (Dh.Embed dhOrd) x) y -> fmap Dh.BoolLit (cmpWith (toCmp dhOrd) x y)
  _ -> Nothing

-- | This assumes normalization of expressions
-- Returns Nothing if it is ill-typed and can't be normalized
cmpWith :: (forall x. Ord x => x -> x -> Bool) -> Dh.Expr s a -> Dh.Expr s a -> Maybe Bool
cmpWith cmp lhs rhs = case (lhs, rhs) of
  (Dh.DoubleLit x, Dh.DoubleLit y)   -> Just $ cmp x y
  (Dh.IntegerLit x, Dh.IntegerLit y) -> Just $ cmp x y
  (Dh.NaturalLit x, Dh.NaturalLit y) -> Just $ cmp x y
  -- TODO (?): List and Option defs
  _                                  -> Nothing
