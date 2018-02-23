{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

-- TODO: Map + Rolling(?)
-- TODO: Extensible with Rec and Const and stuff
-- TODO: dhall-haskell Eq and Ord instances
module Dalek.Exts.Map.Core where

import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text.Buildable (Buildable (..))

import           Dalek.Core
import           Dalek.Patterns
import qualified Dhall.Core          as Dh

import           Dalek.Orphans       ()

data DhMap expr =
  -- When type-checking this, we'll just have to say "trust me" by not inspecting
  -- the Map itself and instead using the annotations. We will still be embedding this
  -- using App, so the only way to make a DhMapLit will be via DhMapFromList or
  -- other combinators
    DhMapLit { _mapKeyTy :: !expr, _mapValTy :: !expr, _mapLit :: !(Map expr expr) }
  | DhMapTy
  | DhMapEmpty
  | DhMapSingleton
  | DhMapLookup
  | DhMapInsert
  | DhMapFromList
  deriving (Eq, Ord, Show)

normalizer :: (Ord s, Ord (Open s fs), Member DhMap fs) => OpenNormalizer s fs
normalizer = \case
  ER DhMapTy -> Nothing
  Apps [ER DhMapEmpty, kty, vty] -> Just $ sendEmbed $ DhMapLit kty vty mempty
  Apps [ER DhMapEmpty, kty, vty, k, v] -> Just $ sendEmbed $ DhMapLit kty vty (M.singleton k v)
  Apps [ER DhMapLookup, _, _, k, ER DhMapLit{..}] -> Just $ maybe (Dh.OptionalLit _mapValTy mempty) (Dh.OptionalLit _mapValTy . pure) $ M.lookup k _mapLit
  Apps [ER DhMapInsert, _, _, k, v, ER m@DhMapLit{_mapLit=curr}] -> Just $ sendEmbed $ m { _mapLit = M.insert k v curr }
  Apps [ER DhMapFromList, _, _, Dh.ListLit _ _] -> undefined -- TODO
  _ -> Nothing

instance Buildable expr => Buildable (DhMap expr) where
  build = \case
    DhMapLit{..} -> undefined --TODO
    DhMapTy -> "Map"
    DhMapEmpty -> "Map/empty"
    DhMapSingleton -> "Map/singleton"
    DhMapLookup -> "Map/lookup"
    DhMapInsert -> "Map/insert"
    DhMapFromList -> "Map/fromList"
