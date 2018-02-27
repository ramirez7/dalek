{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}

module Dalek.Exts.Time.Core where

import qualified Data.HashMap.Strict.InsOrd  as HMI
import           Data.Text.Buildable         (Buildable (..))
import qualified Data.Text.Lazy.Builder      as TLB
import           Data.Time.Format            (FormatTime, defaultTimeLocale,
                                              formatTime)
import qualified Dhall.Core                  as Dh

import           Dalek.Core                  (Member, OpenNormalizer, sendEmbed)
import           Dalek.Patterns
import           Data.Time
import           Data.Time.Calendar.WeekDate

data DhTime expr =
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

normalizer :: Member DhTime fs => OpenNormalizer s fs
normalizer = \case
  Apps [E DhLocalTimeDayOfWeek, E (DhLocalTimeLit lt)] ->
    let (_, _, dayOfWeek) = toWeekDate (localDay lt)
     in Just $ Dh.IntegerLit (fromIntegral dayOfWeek)
  Apps [E DhUTCTimeToLocalTime, E (DhTimeZoneLit tz), E (DhUTCTimeLit t)] ->
    Just $ sendEmbed $ DhLocalTimeLit (utcToLocalTime tz t)
  Apps [E DhLocalTimeTimeOfDay, E (DhLocalTimeLit LocalTime{..})] ->
    let TimeOfDay{..} = localTimeOfDay
     in Just $ Dh.RecordLit $ HMI.fromList
          [ ("hour", Dh.IntegerLit $ fromIntegral todHour)
          , ("minute", Dh.IntegerLit $ fromIntegral todMin)
          ]
  _ -> Nothing


instance Buildable (DhTime expr) where
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
