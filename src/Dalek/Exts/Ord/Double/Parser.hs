module Dalek.Exts.Ord.Double.Parser where

import qualified Dhall.Parser          as Dh

import           Dalek.Exts.Ord.Double.Core (DhDoubleOrd)
import qualified Dhall.ParserUtils     as Dh

parser :: Dh.Parser DhDoubleOrd
parser = Dh.reservedEnum
