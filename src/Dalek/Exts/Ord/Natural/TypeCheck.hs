{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Dalek.Exts.Ord.Natural.TypeCheck where

import qualified Dhall.Core            as Dh
import qualified Dhall.TypeCheck       as Dh

import           Dalek.Exts.Ord.Natural.Core

typer :: Dh.Typer DhNaturalOrd
typer = \case
  _ -> (Dh.Pi "_" Dh.Natural (Dh.Pi "_" Dh.Natural Dh.Bool))
