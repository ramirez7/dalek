{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Dalek.Exts.Ord.Double.Parser where

import           Dalek.Core
import           Dalek.Exts.Ord.Double.Core (DhDoubleOrd)
import           Dalek.Parser


parser :: Member DhDoubleOrd fs => OpenParser s fs
parser = sendParser (reservedEnumF @DhDoubleOrd)
