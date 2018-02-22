{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}

module Dalek.Exts.Ord.Double.TypeCheck where

import qualified Dhall.Core                  as Dh

import           Dalek.Core
import           Dalek.Exts.Ord.Double.Core
import           Dalek.TypeCheck

typer :: (Member DhDoubleOrd fs) => OpenTyper s DhDoubleOrd fs
typer = \case
  _ -> (Dh.Pi "_" Dh.Double (Dh.Pi "_" Dh.Double Dh.Bool))
