module Dhall.Time.Parser where

import qualified Dhall.Parser            as Dh

import qualified Data.Time.Parsers       as TP
import qualified Text.Parser.Combinators as TP

import qualified Dhall.ParserUtils       as Dh
import           Dhall.Time.Core         (DhTime (..))

-- TODO: time-parsers works..but we need a LookAheadParsing instance first!
-- So we newtype wrap ourselves..I wonder why Dhall's parser didn't newtype derive
-- LookAheadParsing. Maybe because it didn't need it?
parser :: Dh.Parser DhTime
parser =  TP.choice $
    [ TP.try $ Dh.quasiQuotes $ Dh.Parser $ fmap DhUTCTimeLit TP.utcTime
    , TP.try $ Dh.quasiQuotes $ Dh.Parser $ fmap DhLocalTimeLit TP.localTime
    , Dh.reservedOneOf
      [ DhUTCTime
      , DhLocalTime
      , DhTimeZone
      , DhLocalTimeDayOfWeek
      , DhUTCTimeToLocalTime
      , DhLocalTimeTimeOfDay
      ]
    ]
