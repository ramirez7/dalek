{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Dalek.Exts.Ord.Time.Parser where

import           Dalek.Core
import           Dalek.Exts.Ord.Time.Core (DhUTCTimeOrd)
import           Dalek.Parser
import qualified Dhall.ParserUtils        as Dh

parser :: Member DhUTCTimeOrd fs => OpenParser s fs
parser = sendParser (Dh.reservedEnumF @DhUTCTimeOrd)
