{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}

module Dalek.Exts.Ord.Integer.TypeCheck where

import qualified Dhall.Core                  as Dh

import           Dalek.Core
import           Dalek.Exts.Ord.Integer.Core
import           Dalek.TypeCheck

typer :: (Member DhIntegerOrd fs) => OpenTyper s DhIntegerOrd fs
typer = \case
  _ -> (Dh.Pi "_" Dh.Integer (Dh.Pi "_" Dh.Integer Dh.Bool))
