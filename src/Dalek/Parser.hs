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

import           Control.Applicative     (empty, (<|>))
import qualified Text.Parser.Combinators as TP
import qualified Text.Trifecta.Parser    as Tri
import qualified Text.Trifecta.Result    as Tri

import qualified Dhall.Parser            as Dh

import           Dalek.Core

import           Data.Open.Union         (Union, weaken)

type OpenParser s fs = Dh.Parser (Open s fs)

sendParser :: forall fs f s. Member f fs => Dh.Parser (f (OpenExpr s fs)) -> OpenParser s fs
sendParser = fmap (Rec . inj)

openParseStr :: forall fs. OpenParser Dh.Src fs -> String -> Tri.Result (OpenExpr Dh.Src fs)
openParseStr p s = Tri.parseString (Dh.unParser $ Dh.exprA p) mempty s

{-
sendParser :: forall (fs :: [* -> *]) a s. Member (Const a) fs => Dh.Parser a -> OpenParser s fs
sendParser = fmap (Rec . inj . Const)

infixr `parserUnion`
-- | Typically used infix (right-associative) in conjunction with 'openParser':
--
-- @
-- empty `parserUnion` empty `parserUnion` openParser empty
-- @
--
-- NOTE: This actually won't work!! DhMapF isn't a Functor..I think just (TP.try . sendParser)
-- is what we should automate. Mb don't even need try due to <|>?
parserUnion :: Functor (Union (f ': fs)) => Dh.Parser a -> OpenParser s (f ': fs) -> OpenParser s (Const a ': f ': fs)
parserUnion p op = TP.try (sendParser p) <|> (fmap (mapRec weaken) op)

-- | A more specific 'sendParser'. Meant to help inference when building up
-- the parser. Used at the "bottom" of all the calls to parserUnion
openParser :: Dh.Parser a -> OpenParser s '[Const a]
openParser = sendParser

tstPrefix :: forall s. OpenParser s '[Const Int, Const Bool, Const ()]
tstPrefix =
  (parserUnion empty
    (parserUnion empty
      (openParser empty)))

tstInfix :: forall s. OpenParser s '[Const Int, Const Bool, Const ()]
tstInfix = empty `parserUnion` empty `parserUnion` openParser empty

voidOpenParser :: OpenParser s '[]
voidOpenParser = empty
-}
