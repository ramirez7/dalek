{-# LANGUAGE LambdaCase #-}

module Dhall.Time.Core where

import qualified Dhall.Core as Dh

import           Data.Time  (LocalTime, UTCTime)

data DhTime =
    DhUTCTime
  | DhUTCTimeLit !UTCTime
  | DhLocalTime
  | DhLocalTimeLit !LocalTime
  | DhLocalTimeDayOfWeek
  | DhUTCTimeToLocalTime
  | DhLocalTimeTimeOfDay
  deriving (Eq, Ord, Show)

normalizer :: Dh.Normalizer DhTime
normalizer = \case
  Dh.App (Dh.Embed DhLocalTimeDayOfWeek) (Dh.Embed (DhLocalTimeLit lt)) -> Just $ Dh.IntegerLit undefined
  -- TODO: exhaustive
