{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Dalek.Exts.Time.TypeCheck where

import qualified Data.HashMap.Strict.InsOrd as HMI
import qualified Dhall.Core                 as Dh

import           Dalek.Core
import           Dalek.Exts.Time.Core
import           Dalek.TypeCheck

typer :: Member DhTime fs => OpenTyper s DhTime fs
typer = \case
  DhUTCTime -> Dh.Const Dh.Type
  DhUTCTimeLit _ -> sendEmbed DhUTCTime
  DhLocalTime -> Dh.Const Dh.Type
  DhLocalTimeLit _ -> sendEmbed DhLocalTime
  DhTimeZone -> Dh.Const Dh.Type
  DhTimeZoneLit _ -> sendEmbed DhTimeZone
  DhLocalTimeDayOfWeek -> Dh.Pi "_" (sendEmbed DhLocalTime) Dh.Integer
  DhUTCTimeToLocalTime -> Dh.Pi "_" (sendEmbed DhTimeZone) (Dh.Pi "_" (sendEmbed DhUTCTime) (sendEmbed DhLocalTime))
  DhLocalTimeTimeOfDay -> Dh.Pi "_" (sendEmbed DhLocalTime) (Dh.Record $ HMI.fromList [("hour", Dh.Integer), ("minute", Dh.Integer)])
