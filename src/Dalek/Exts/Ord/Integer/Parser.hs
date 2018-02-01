module Dalek.Exts.Ord.Integer.Parser where

import qualified Dhall.Parser          as Dh

import           Dalek.Exts.Ord.Integer.Core (DhIntegerOrd)
import qualified Dhall.ParserUtils     as Dh

parser :: Dh.Parser DhIntegerOrd
parser = Dh.reservedEnum
