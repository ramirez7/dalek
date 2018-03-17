{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}

module Dalek.Exts.Ord.Natural.TypeCheck where

import qualified Dhall.Core                  as Dh

import           Dalek.Core
import           Dalek.Exts.Ord.Natural.Core
import           Dalek.TypeCheck

typer :: (Member DhNaturalOrd fs) => OpenTyper DhNaturalOrd fs
typer = \case
  _ -> (Dh.Pi "_" Dh.Natural (Dh.Pi "_" Dh.Natural Dh.Bool))
