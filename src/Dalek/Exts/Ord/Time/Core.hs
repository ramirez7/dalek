{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Dalek.Exts.Ord.Time.Core where

import           Data.Text.Buildable  (Buildable (..))
import qualified Dhall.Core           as Dh

import           Dalek.Core
import           Dalek.Exts.Time.Core
import           Dalek.Patterns

data DhUTCTimeOrd =
    DhUTCTimeEQ
  | DhUTCTimeNEQ
  | DhUTCTimeLT
  | DhUTCTimeLTE
  | DhUTCTimeGT
  | DhUTCTimeGTE
  deriving (Eq, Show, Enum, Bounded)

toCmp :: DhUTCTimeOrd -> (Ord a => a -> a -> Bool)
toCmp = \case
  DhUTCTimeEQ -> (==)
  DhUTCTimeNEQ -> (/=)
  DhUTCTimeLT -> (<)
  DhUTCTimeLTE -> (<=)
  DhUTCTimeGT -> (>)
  DhUTCTimeGTE -> (>=)

normalizer :: Members '[Const DhUTCTimeOrd, Const DhTime] as => OpenNormalizer s as
normalizer = \case
  Apps [EC dhOrd, EC (DhUTCTimeLit x), EC (DhUTCTimeLit y)] ->
    Just $ Dh.BoolLit $ (toCmp dhOrd) x y
  _ -> Nothing

instance Buildable DhUTCTimeOrd where
  build = \case
    DhUTCTimeEQ -> "UTCTime/EQ"
    DhUTCTimeNEQ -> "UTCTime/NEQ"
    DhUTCTimeLT -> "UTCTime/LT"
    DhUTCTimeLTE -> "UTCTime/LTE"
    DhUTCTimeGT -> "UTCTime/GT"
    DhUTCTimeGTE -> "UTCTime/GTE"
