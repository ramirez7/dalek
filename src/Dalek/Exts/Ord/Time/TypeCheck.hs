{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Dalek.Exts.Ord.Time.TypeCheck where

import qualified Dhall.Core               as Dh

import           Dalek.Core
import           Dalek.Exts.Ord.Time.Core
import           Dalek.Exts.Time.Core
import           Dalek.TypeCheck

typer :: (Member DhTime fs) => OpenTyper DhUTCTimeOrd fs
typer = \case
  _ -> Dh.Pi "_" (sendEmbed DhUTCTime) (Dh.Pi "_" (sendEmbed DhUTCTime) Dh.Bool)
