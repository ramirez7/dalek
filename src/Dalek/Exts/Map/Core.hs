{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns       #-}

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
import Dhall.Core (denote)

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

normalizer :: (Ord (Open fs), Member DhMap fs) => OpenNormalizer fs
normalizer = \case
  E DhMapTy -> Nothing
  Apps [E DhMapEmpty, denote -> kty, denote -> vty] ->
    Just $ sendEmbed $ DhMapLit kty vty mempty
  Apps [E DhMapEmpty, denote -> kty, denote -> vty, denote -> k, denote -> v] ->
    Just $ sendEmbed $ DhMapLit kty vty (M.singleton k v)
  Apps [E DhMapLookup, _, _, denote -> k, E DhMapLit{..}] ->
    Just $ maybe (Dh.OptionalLit (denote _mapValTy) Nothing) (Dh.OptionalLit (denote _mapValTy) . pure . denote) $ M.lookup k _mapLit
  Apps [E DhMapInsert, _, _, denote -> k, denote -> v, E m@DhMapLit{_mapLit=curr}] -> Just $ sendEmbed $ m { _mapLit = M.insert k v curr }
  Apps [E DhMapFromList, _, _, Dh.ListLit _ _] -> undefined -- TODO
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
