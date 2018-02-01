module Dalek.Exts.Ord.Natural.Parser where

import qualified Dhall.Parser           as Dh

import           Dalek.Exts.Ord.Natural.Core (DhNaturalOrd)
import qualified Dhall.ParserUtils      as Dh

parser :: Dh.Parser DhNaturalOrd
parser = Dh.reservedEnum
