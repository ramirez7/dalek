{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Dalek.Parser where

import Control.Applicative (empty)
import qualified Text.Trifecta.Parser as Tri
import qualified Text.Trifecta.Result as Tri

import qualified Dhall.Parser         as Dh

import           Dalek.Core

type OpenParser s fs = Dh.Parser (Open s fs)

sendParser :: forall fs f s. Member f fs => Dh.Parser (f (OpenExpr s fs)) -> OpenParser s fs
sendParser = fmap (Rec . inj)

openParseStr :: forall fs. OpenParser Dh.Src fs -> String -> Tri.Result (OpenExpr Dh.Src fs)
openParseStr p s = Tri.parseString (Dh.unParser $ Dh.exprA p) mempty s

xParser :: Member (C X) fs => OpenParser s fs
xParser = empty
