{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Dhall.Extensible.TypeCheck where

import           Data.Functor.Identity (Identity (..))

import           Data.Union

import qualified Dhall.Core            as Dh
import qualified Dhall.TypeCheck       as Dh

-- OpenUnion as -> Expr s (OpenUnion as)
type OpenTyper as = Dh.Typer (OpenUnion as)

typerUnion :: forall a as is. USubset as (a ': as) is => Dh.Typer a -> OpenTyper as -> OpenTyper (a ': as)
typerUnion ta tas = \aas ->
  union
    (\uas -> fmap urelax $ tas uas)
    (\(Identity a) -> fmap (ulift . Identity) $ ta a)
    aas
{-
-- TODO: Put this on HOLD until the Rec stuff is folded into Extensible.
-- THEN get typer & normalizer combinators that allow projecting to a larger embedding

type OpenTyper2 a as = forall s. a -> Dh.Expr s (OpenUnion as)

sendTyper :: forall a as i. UElem a as i => Dh.Typer a -> OpenTyper2 a as
sendTyper = undefined

typerContrUnion :: OpenTyper2 a target -> OpenTyper2 b target -> OpenTyper2 (OpenUnion '[a, b]) target
typerContrUnion = undefined
-}
voidOpenTyper :: OpenTyper '[]
voidOpenTyper = absurdUnion
