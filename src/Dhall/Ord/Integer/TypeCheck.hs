{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Ord.Integer.TypeCheck where

import qualified Dhall.Core            as Dh
import qualified Dhall.TypeCheck       as Dh

import           Dhall.Ord.Integer.Core

typer :: Dh.Typer s DhIntegerOrd
typer = \case
  _ -> (Dh.Pi "_" Dh.Integer (Dh.Pi "_" Dh.Integer Dh.Bool))
