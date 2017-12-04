module Dhall.Ord.Integer.Parser where

import qualified Dhall.Parser          as Dh

import           Dhall.Ord.Integer.Core (DhIntegerOrd)
import qualified Dhall.ParserUtils     as Dh

parser :: Dh.Parser DhIntegerOrd
parser = Dh.reservedEnum
