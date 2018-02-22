{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}

module Dalek.Exts.Ord.Integer.Core where

import           Data.Text.Buildable (Buildable (..))
import qualified Dhall.Core          as Dh

import           Dalek.Core
import           Dalek.Patterns

data DhIntegerOrd expr =
    DhIntegerEQ
  | DhIntegerNEQ
  | DhIntegerLT
  | DhIntegerLTE
  | DhIntegerGT
  | DhIntegerGTE
  deriving (Eq, Show, Enum, Bounded)

toCmp :: DhIntegerOrd expr -> (Integer -> Integer -> Bool)
toCmp = \case
  DhIntegerEQ -> (==)
  DhIntegerNEQ -> (/=)
  DhIntegerLT -> (<)
  DhIntegerLTE -> (<=)
  DhIntegerGT -> (>)
  DhIntegerGTE -> (>=)

normalizer :: Member DhIntegerOrd fs => OpenNormalizer s fs
normalizer = \case
  Apps [ER dhOrd, Dh.IntegerLit x, Dh.IntegerLit y] -> Just $ Dh.BoolLit $ (toCmp dhOrd) x y
  _ -> Nothing

instance Buildable (DhIntegerOrd expr) where
  build = \case
    DhIntegerEQ -> "Integer/EQ"
    DhIntegerNEQ -> "Integer/NEQ"
    DhIntegerLT -> "Integer/LT"
    DhIntegerLTE -> "Integer/LTE"
    DhIntegerGT -> "Integer/GT"
    DhIntegerGTE -> "Integer/GTE"
