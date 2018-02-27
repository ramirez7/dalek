{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Dalek.Parser
  ( OpenParser
  , sendParser
  -- * Utilities
  , openParseStr
  , xParser
  -- * Re-exports
  , (<|>)
  , Tri.Result (..)
  , Tri.ErrInfo (..)
  ) where

import           Control.Applicative  (empty, (<|>))
import qualified Text.Trifecta.Parser as Tri
import qualified Text.Trifecta.Result as Tri

import qualified Dhall.Parser         as Dh

import           Dalek.Core

type OpenParser s fs = Dh.Parser (Open s fs)

-- | Lift a 'Dh.Parser' for a single member of an 'Open' to an 'OpenParser'
sendParser :: forall fs f s. Member f fs => Dh.Parser (f (OpenExpr s fs)) -> OpenParser s fs
sendParser = fmap (Rec . inj)

-- | Run an 'OpenParser' on a 'String'
openParseStr :: forall fs. OpenParser Dh.Src fs -> String -> Tri.Result (OpenExpr Dh.Src fs)
openParseStr p s = Tri.parseString (Dh.unParser $ Dh.exprA p) mempty s

-- | Parse lifted 'X'
xParser :: Member (C X) fs => OpenParser s fs
xParser = empty
