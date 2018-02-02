{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Dalek.Exts.Ord.Double.TypeCheck where

import qualified Dhall.Core            as Dh
import qualified Dhall.TypeCheck       as Dh

import           Dalek.Exts.Ord.Double.Core

typer :: Dh.Typer s DhDoubleOrd
typer = \case
  _ -> (Dh.Pi "_" Dh.Double (Dh.Pi "_" Dh.Double Dh.Bool))
