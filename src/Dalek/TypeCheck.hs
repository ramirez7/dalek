{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Dalek.TypeCheck where

import           Data.Functor.Contravariant

import           Dhall                      (Text)
import qualified Dhall.Core                 as Dh
import qualified Dhall.TypeCheck            as Dh

import           Dalek.Core
import           Data.Open.Union

-- OpenUnion as -> Expr s (OpenUnion as)
-- f (OpenExpr s fs) -> OpenExpr s fs
-- OpenTyper s (Union fs) fs ~ Typer
type OpenTyper s f fs = f (OpenExpr s fs) -> OpenExpr s fs

type OpenTypeError s fs = Dh.TypeError s (Open s fs)
-- TODO: Open type should be a Typer alias. PartialTyper or smth should be this one

typerUnion :: OpenTyper s f ftarget -> OpenTyper s (Union fs) ftarget -> OpenTyper s (Union (f ': fs)) ftarget
typerUnion otf otfs = \uffs -> case decomp uffs of
  Right f -> otf f
  Left fs -> otfs fs

toTyper :: OpenTyper s (Union fs) fs -> Dh.Typer s (Open s fs)
toTyper = getOp . contramap unRec . Op

sendTyper :: OpenTyper s f ftarget -> OpenTyper s (Union '[f]) ftarget
sendTyper = (. extract)

xTyper :: OpenTyper s (C X) fs
xTyper = absurd . unC

-- TODO: Shake out fixity/order of operations of PiBinding + the arrows
data PiBinding s a = !Text :. !(Dh.Expr s a)

(~>) :: PiBinding s a -> Dh.Expr s a -> Dh.Expr s a
(name :. input) ~> output = Dh.Pi name input output

(.~>) :: Dh.Expr s a -> Dh.Expr s a -> Dh.Expr s a
input .~> output = Dh.Pi "_" input output

infixr .~>
infixr ~>
