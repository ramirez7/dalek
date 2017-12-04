{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Time.Core where

import qualified Data.Map                    as M
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

normalizer :: Dh.Normalizer DhTime
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
