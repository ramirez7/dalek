{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}

module Dalek.Exts.Ord.Natural.Core where

import           Data.Text.Buildable (Buildable (..))
import           Dhall               (Natural)
import qualified Dhall.Core          as Dh

import           Dalek.Core
import           Dalek.Patterns

data DhNaturalOrd expr =
    DhNaturalEQ
  | DhNaturalNEQ
  | DhNaturalLT
  | DhNaturalLTE
  | DhNaturalGT
  | DhNaturalGTE
  deriving (Eq, Show, Enum, Bounded)

toCmp :: DhNaturalOrd expr -> (Natural -> Natural -> Bool)
toCmp = \case
  DhNaturalEQ -> (==)
  DhNaturalNEQ -> (/=)
  DhNaturalLT -> (<)
  DhNaturalLTE -> (<=)
  DhNaturalGT -> (>)
  DhNaturalGTE -> (>=)

normalizer :: Member DhNaturalOrd fs => OpenNormalizer fs
normalizer = \case
  Apps [E dhOrd, Dh.NaturalLit x, Dh.NaturalLit y] -> Just $ Dh.BoolLit $ (toCmp dhOrd) x y
  _ -> Nothing

instance Buildable (DhNaturalOrd expr) where
  build = \case
    DhNaturalEQ -> "Natural/EQ"
    DhNaturalNEQ -> "Natural/NEQ"
    DhNaturalLT -> "Natural/LT"
    DhNaturalLTE -> "Natural/LTE"
    DhNaturalGT -> "Natural/GT"
    DhNaturalGTE -> "Natural/GTE"
