{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Ord.Natural.TypeCheck where

import qualified Dhall.Core            as Dh
import qualified Dhall.TypeCheck       as Dh

import           Dhall.Ord.Natural.Core

typer :: Dh.Typer DhNaturalOrd
typer = \case
  _ -> (Dh.Pi "_" Dh.Natural (Dh.Pi "_" Dh.Natural Dh.Bool))
