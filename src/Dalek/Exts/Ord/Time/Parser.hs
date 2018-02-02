module Dalek.Exts.Ord.Time.Parser where

import qualified Dhall.Parser           as Dh

import           Dalek.Exts.Ord.Time.Core (DhUTCTimeOrd)
import qualified Dhall.ParserUtils      as Dh

parser :: Dh.Parser DhUTCTimeOrd
parser = Dh.reservedEnum
