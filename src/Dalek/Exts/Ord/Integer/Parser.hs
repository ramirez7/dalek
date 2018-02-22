{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Dalek.Exts.Ord.Integer.Parser where

import           Dalek.Core
import           Dalek.Exts.Ord.Integer.Core (DhIntegerOrd)
import qualified Dhall.ParserUtils     as Dh
import           Dalek.Parser


parser :: Member DhIntegerOrd fs => OpenParser s fs
parser = sendParser (Dh.reservedEnumF @DhIntegerOrd)
