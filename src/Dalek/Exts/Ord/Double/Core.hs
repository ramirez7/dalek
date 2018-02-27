{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}

module Dalek.Exts.Ord.Double.Core where

import           Data.Scientific     (Scientific)
import           Data.Text.Buildable (Buildable (..))
import qualified Dhall.Core          as Dh

import           Dalek.Core
import           Dalek.Patterns

data DhDoubleOrd expr =
    DhDoubleEQ
  | DhDoubleNEQ
  | DhDoubleLT
  | DhDoubleLTE
  | DhDoubleGT
  | DhDoubleGTE
  deriving (Eq, Show, Enum, Bounded)

toCmp :: DhDoubleOrd expr -> (Scientific -> Scientific -> Bool)
toCmp = \case
  DhDoubleEQ -> (==)
  DhDoubleNEQ -> (/=)
  DhDoubleLT -> (<)
  DhDoubleLTE -> (<=)
  DhDoubleGT -> (>)
  DhDoubleGTE -> (>=)

normalizer :: Member DhDoubleOrd fs => OpenNormalizer s fs
normalizer = \case
  Apps [E dhOrd, Dh.DoubleLit x, Dh.DoubleLit y] -> Just $ Dh.BoolLit $ (toCmp dhOrd) x y
  _ -> Nothing

instance Buildable (DhDoubleOrd expr) where
  build = \case
    DhDoubleEQ -> "Double/EQ"
    DhDoubleNEQ -> "Double/NEQ"
    DhDoubleLT -> "Double/LT"
    DhDoubleLTE -> "Double/LTE"
    DhDoubleGT -> "Double/GT"
    DhDoubleGTE -> "Double/GTE"
