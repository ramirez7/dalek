{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Dalek.TypeCheck where

import qualified Dhall.TypeCheck       as Dh
import Data.Functor.Contravariant

import Dalek.Core
import Data.Open.Union

-- OpenUnion as -> Expr s (OpenUnion as)
-- f (OpenExpr s fs) -> OpenExpr s fs
-- OpenTyper s (Union fs) fs ~ Typer
type OpenTyper s f fs = f (OpenExpr s fs) -> OpenExpr s fs
-- TODO: Open type should be a Typer alias. PartialTyper or smth should be this one
typerUnion :: OpenTyper s f ftarget -> OpenTyper s (Union fs) ftarget -> OpenTyper s (Union (f ': fs)) ftarget
typerUnion otf otfs = \uffs -> case decomp uffs of
  Right f -> otf f
  Left fs -> otfs fs

toTyper :: OpenTyper s (Union fs) fs -> Dh.Typer s (Open s fs)
toTyper = getOp . contramap unRec . Op

sendTyper :: OpenTyper s f ftarget -> OpenTyper s (Union '[f]) ftarget
sendTyper = (. extract)
{-
typerUnion :: forall a as is. USubset as (a ': as) is => Dh.Typer s a -> OpenTyper as -> OpenTyper (a ': as)
typerUnion ta tas = \aas ->
  union
    (\uas -> fmap urelax $ tas uas)
    (\(Identity a) -> fmap (ulift . Identity) $ ta a)
    aas

-- TODO: Put this on HOLD until the Rec stuff is folded into Extensible.
-- THEN get typer & normalizer combinators that allow projecting to a larger embedding

type OpenTyper2 a as = forall s. a -> Dh.Expr s (OpenUnion as)

sendTyper :: forall a as i. UElem a as i => Dh.Typer s a -> OpenTyper2 a as
sendTyper = undefined

typerContrUnion :: OpenTyper2 a target -> OpenTyper2 b target -> OpenTyper2 (OpenUnion '[a, b]) target
typerContrUnion = undefined

voidOpenTyper :: OpenTyper '[]
voidOpenTyper = absurdUnion
-}
