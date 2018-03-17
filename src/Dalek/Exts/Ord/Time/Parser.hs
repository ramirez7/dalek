{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Dalek.Exts.Ord.Time.Parser where

import           Dalek.Core
import           Dalek.Exts.Ord.Time.Core (DhUTCTimeOrd)
import           Dalek.Parser

parser :: Member DhUTCTimeOrd fs => OpenParser fs
parser = sendParser (reservedEnumF @DhUTCTimeOrd)
