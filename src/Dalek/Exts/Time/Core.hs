{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}

module Dalek.Exts.Time.Core where

import qualified Data.Map                    as M
import           Data.Text.Buildable         (Buildable (..))
import qualified Data.Text.Lazy.Builder      as TLB
import           Data.Time.Format            (FormatTime, defaultTimeLocale,
                                              formatTime)
import qualified Dhall.Core                  as Dh

import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Dhall.Patterns

data DhTime =
    DhUTCTime
  | DhUTCTimeLit !UTCTime
  | DhLocalTime
  | DhLocalTimeLit !LocalTime
  | DhTimeZone
  | DhTimeZoneLit !TimeZone
  | DhLocalTimeDayOfWeek
  | DhUTCTimeToLocalTime
  | DhLocalTimeTimeOfDay
  deriving (Eq, Ord, Show)

normalizer :: Dh.Normalizer s DhTime
normalizer = \case
  Apps [E DhLocalTimeDayOfWeek, E (DhLocalTimeLit lt)] ->
    let (_, _, dayOfWeek) = toWeekDate (localDay lt)
     in Just $ Dh.IntegerLit (fromIntegral dayOfWeek)
  Apps [E DhUTCTimeToLocalTime, E (DhTimeZoneLit tz), E (DhUTCTimeLit t)] ->
    Just $ Dh.Embed $ DhLocalTimeLit (utcToLocalTime tz t)
  Apps [E DhLocalTimeTimeOfDay, E (DhLocalTimeLit LocalTime{..})] ->
    let TimeOfDay{..} = localTimeOfDay
     in Just $ Dh.RecordLit $ M.fromList
          [ ("hour", Dh.IntegerLit $ fromIntegral todHour)
          , ("minute", Dh.IntegerLit $ fromIntegral todMin)
          ]
  _ -> Nothing


instance Buildable DhTime where
  build = \case
    DhUTCTime -> "UTCTime"
    DhUTCTimeLit t -> quoteTime "%Y-%m-%dT%H:%M:%S%z" t
    DhLocalTime -> "LocalTime"
    DhLocalTimeLit t -> quoteTime "%Y-%m-%dT%H:%M:%S" t
    DhTimeZone -> "TimeZone"
    DhTimeZoneLit t -> quoteTime "%z" t
    DhLocalTimeDayOfWeek -> "LocalTime/dayOfWeek"
    DhUTCTimeToLocalTime -> "UTCTime/toLocalTime"
    DhLocalTimeTimeOfDay -> "LocalTime/timeOfDay"

quoteTime :: FormatTime t => String -> t -> TLB.Builder
quoteTime s t = "$(" `mappend` TLB.fromString (formatTime defaultTimeLocale s t) `mappend` ")"
