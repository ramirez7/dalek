{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dalek.Exts.Time.Parser where

import qualified Dhall.Parser            as Dh

import           Data.Time               (utc)
import qualified Data.Time.Parsers       as TP
import qualified Text.Parser.Combinators as TP

import           Dalek.Core
import           Dalek.Exts.Time.Core    (DhTime (..))
import           Dalek.Parser

-- TODO: time-parsers works..but we need a LookAheadParsing instance first!
-- So we newtype wrap ourselves..I wonder why Dhall's parser didn't newtype derive
-- LookAheadParsing. Maybe because it didn't need it?
-- Member DhUTCTimeOrd fs => OpenParser s fs
parser :: Member DhTime fs => OpenParser s fs
parser = sendParser $ TP.choice $
    [ TP.try $ quasiQuotes $ Dh.Parser $ fmap DhUTCTimeLit TP.utcTime
    , TP.try $ quasiQuotes $ Dh.Parser $ fmap DhLocalTimeLit TP.localTime
    , TP.try $ quasiQuotes $ Dh.Parser $ fmap (maybe (DhTimeZoneLit utc) DhTimeZoneLit) TP.timeZone
    , reservedOneOf
      [ DhLocalTimeDayOfWeek
      , DhUTCTimeToLocalTime
      , DhLocalTimeTimeOfDay
      , DhUTCTime
      , DhLocalTime
      , DhTimeZone

      ]
    ]
