{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dalek.Orphans where

import           Data.List                  (sortOn)

import qualified Data.HashMap.Strict.InsOrd as HMI
import qualified Dhall.Core                 as Dh

instance (Ord k, Ord v) => Ord (HMI.InsOrdHashMap k v) where
  compare lhs rhs = compare (conv lhs) (conv rhs)
    where
      conv = sortOn fst . HMI.toList

deriving instance (Ord s, Ord a) => Ord (Dh.Expr s a)

deriving instance (Ord s, Ord a) => Ord (Dh.Chunks s a)
