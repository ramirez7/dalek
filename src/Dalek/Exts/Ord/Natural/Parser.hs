{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Dalek.Exts.Ord.Natural.Parser where

import           Dalek.Core
import           Dalek.Exts.Ord.Natural.Core (DhNaturalOrd)
import qualified Dhall.ParserUtils     as Dh
import           Dalek.Parser


parser :: Member DhNaturalOrd fs => OpenParser s fs
parser = sendParser (Dh.reservedEnumF @DhNaturalOrd)
