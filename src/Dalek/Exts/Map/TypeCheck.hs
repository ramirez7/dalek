{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Dalek.Exts.Map.TypeCheck where

import qualified Dhall.Core          as Dh

import           Dalek.Core
import           Dalek.Exts.Map.Core
import           Dalek.TypeCheck

typer :: (Member DhMap fs) => OpenTyper s DhMap fs
typer = \case
  DhMapLit{..} -> mkMapTy _mapKeyTy _mapValTy
  DhMapTy -> Dh.Const Dh.Type
  DhMapEmpty ->
    (Dh.Pi "k" (Dh.Const Dh.Type)
      (Dh.Pi "v" (Dh.Const Dh.Type)
        (mkMapTy "k" "v")))
  DhMapSingleton ->
    (Dh.Pi "kty" (Dh.Const Dh.Type)
      (Dh.Pi "vty" (Dh.Const Dh.Type)
        (Dh.Pi "_" "kty"
          (Dh.Pi "_" "vty"
            (mkMapTy "kty" "vty")))))
  DhMapLookup ->
    (Dh.Pi "kty" (Dh.Const Dh.Type)
      (Dh.Pi "vty" (Dh.Const Dh.Type)
        (Dh.Pi "_" "kty"
          (Dh.Pi "_" (mkMapTy "kty" "vty")
            (Dh.App Dh.Optional "vty")))))
  DhMapInsert ->
    (Dh.Pi "kty" (Dh.Const Dh.Type)
      (Dh.Pi "vty" (Dh.Const Dh.Type)
        (Dh.Pi "_" "kty"
          (Dh.Pi "_" "vty"
            (Dh.Pi "_" (mkMapTy "kty" "vty")
              (mkMapTy "kty" "vty"))))))
  DhMapFromList -> undefined

mkMapTy :: (Member DhMap fs) => OpenExpr s fs -> OpenExpr s fs -> OpenExpr s fs
mkMapTy k v = (Dh.App (Dh.App (sendEmbed DhMapTy) k) v)
