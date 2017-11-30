module Dhall.Ord.Double.Parser where

import qualified Dhall.Parser          as Dh

import           Dhall.Ord.Double.Core (DhDoubleOrd)
import qualified Dhall.ParserUtils     as Dh

parser :: Dh.Parser DhDoubleOrd
parser = Dh.reservedOneOf
