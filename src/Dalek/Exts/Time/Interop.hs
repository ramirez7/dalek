{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dalek.Exts.Time.Interop where

import           Data.Time

import           Dalek.Core
import           Dalek.Exts.Time.Core
import           Dalek.Interop
import           Dalek.Patterns

utcTime :: Member DhTime fs => OutputType fs UTCTime
utcTime = ConcreteOutputType {
    concreteExtract = \case
      E (DhUTCTimeLit t) -> Just t
      _ -> Nothing
  , concreteExpected = sendEmbed DhUTCTime
}
