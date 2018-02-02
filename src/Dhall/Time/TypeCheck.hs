{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Time.TypeCheck where

import qualified Data.Map        as M
import qualified Dhall.Core      as Dh
import qualified Dhall.TypeCheck as Dh

import           Dhall.Time.Core


typer :: Dh.Typer s DhTime
typer = \case
  DhUTCTime -> Dh.Const Dh.Type
  DhUTCTimeLit _ -> Dh.Embed DhUTCTime
  DhLocalTime -> Dh.Const Dh.Type
  DhLocalTimeLit _ -> Dh.Embed DhLocalTime
  DhTimeZone -> Dh.Const Dh.Type
  DhTimeZoneLit _ -> Dh.Embed DhTimeZone
  DhLocalTimeDayOfWeek -> Dh.Pi "_" (Dh.Embed DhLocalTime) Dh.Integer
  DhUTCTimeToLocalTime -> Dh.Pi "_" (Dh.Embed DhTimeZone) (Dh.Pi "_" (Dh.Embed DhUTCTime) (Dh.Embed DhLocalTime))
  DhLocalTimeTimeOfDay -> Dh.Pi "_" (Dh.Embed DhLocalTime) (Dh.Record $ M.fromList [("hour", Dh.Integer), ("minute", Dh.Integer)])
