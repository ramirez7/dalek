{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

-- TODO: Map + Rolling(?)
-- TODO: Extensible with Rec and Const and stuff
-- TODO: dhall-haskell Eq and Ord instances
module Dalek.Exts.Map.Core where

import qualified Dhall.Core as Dh

import           Data.Map   (Map)
import qualified Data.Map   as M

data DhMapF a =
  -- When type-checking this, we'll just have to say "trust me" by not inspecting
  -- the Map itself and instead using the annotations. We will still be embedding this
  -- using App, so the only way to make a DhMapLit will be via DhMapFromList or
  -- other combinators
    DhMapLit { _mapKeyTy :: !a, _mapValTy :: !a, _mapLit :: !(Map a a) }
  | DhMapTy
  | DhMapEmpty
  | DhMapSingleton
  | DhMapLookup
  | DhMapInsert
  | DhMapFromList
  deriving (Eq, Ord, Show)

-- | Inspired by the "Term trick":
--
-- http://blog.sumtypeofway.com/recursion-schemes-part-41-2-better-living-through-base-functors/
newtype Rec s f = Rec { unRec :: f (Dh.Expr s (Rec s f)) }

deriving instance (Show (f (Dh.Expr s (Rec s f))), Show s) => Show (Rec s f)
-- ^ Need to have a higher-kinded union like Eff to work with this
-- Then use Const functors to embed * terms
-- Dhall.Extensible should wrap it all up in a nice API
-- Tbh just depend on `freer`! Ping Allele and ask about cutting it into its own lib

normalize :: Dh.Expr s (Rec s DhMapF) -> Maybe (Dh.Expr s (Rec s DhMapF))
normalize = undefined
