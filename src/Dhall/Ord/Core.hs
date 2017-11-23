{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dhall.Ord.Core where

import qualified Dhall.Core as Dh

data DhOrd s =
    DhEQ (DhOrdEmbedded s) (DhOrdEmbedded s)
  | DnNEQ (DhOrdEmbedded s) (DhOrdEmbedded s)
  | DhLT (DhOrdEmbedded s) (DhOrdEmbedded s)
  | DhLTE (DhOrdEmbedded s) (DhOrdEmbedded s)
  | DhGT (DhOrdEmbedded s) (DhOrdEmbedded s)
  | DhGTE (DhOrdEmbedded s) (DhOrdEmbedded s)

deriving instance Eq s => Eq (DhOrd s)
deriving instance Show s => Show (DhOrd s)

type DhOrdEmbedded s = Dh.Expr s (DhOrd s)

normalize :: DhOrd s -> DhOrdEmbedded s
normalize e = case e of
  DhEQ x y  -> maybe (Dh.Embed e) Dh.BoolLit (dhallCmpWithNorm (==) x y)
  DnNEQ x y -> maybe (Dh.Embed e) Dh.BoolLit (dhallCmpWithNorm (/=) x y)
  DhLT x y  -> maybe (Dh.Embed e) Dh.BoolLit (dhallCmpWithNorm (<) x y)
  DhLTE x y -> maybe (Dh.Embed e) Dh.BoolLit (dhallCmpWithNorm (<=) x y)
  DhGT x y  -> maybe (Dh.Embed e) Dh.BoolLit (dhallCmpWithNorm (>) x y)
  DhGTE x y -> maybe (Dh.Embed e) Dh.BoolLit (dhallCmpWithNorm (>=) x y)

subst :: Dh.Substituter s (DhOrd s)
subst e v s = case e of
  DhEQ x y  -> DhEQ (Dh.substWith subst v s x) (Dh.substWith subst v s y)
  DnNEQ x y -> DnNEQ (Dh.substWith subst v s x) (Dh.substWith subst v s y)
  DhLT x y  -> DhLT (Dh.substWith subst v s x) (Dh.substWith subst v s y)
  DhLTE x y -> DhLTE (Dh.substWith subst v s x) (Dh.substWith subst v s y)
  DhGT x y  -> DhGT (Dh.substWith subst v s x) (Dh.substWith subst v s y)
  DhGTE x y -> DhGTE (Dh.substWith subst v s x) (Dh.substWith subst v s y)

normalizeEmbedded :: DhOrdEmbedded s -> DhOrdEmbedded s
normalizeEmbedded x = let dhNorm = Dh.normalizeAndSubstWith subst (const Nothing)
                       in dhNorm (dhNorm x >>= normalize)

dhallCmpWithNorm :: (forall x. Ord x => x -> x -> Bool) -> DhOrdEmbedded s -> DhOrdEmbedded s -> Maybe Bool
dhallCmpWithNorm cmp lhs rhs = dhallCmpWith cmp (normalizeEmbedded lhs) (normalizeEmbedded rhs)

-- | This assumes normalization of expressions
-- Returns Nothing if it is ill-typed and can't be normalized
dhallCmpWith :: (forall x. Ord x => x -> x -> Bool) -> Dh.Expr s a -> Dh.Expr s a -> Maybe Bool
dhallCmpWith cmp lhs rhs = case (lhs, rhs) of
  (Dh.DoubleLit x, Dh.DoubleLit y)   -> Just $ cmp x y
  (Dh.IntegerLit x, Dh.IntegerLit y) -> Just $ cmp x y
  (Dh.NaturalLit x, Dh.NaturalLit y) -> Just $ cmp x y
  -- TODO: List and Option(??) defs
  _                                  -> Nothing
