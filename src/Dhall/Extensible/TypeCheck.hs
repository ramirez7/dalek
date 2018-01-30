{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Dhall.Extensible.TypeCheck where

import           Data.Functor.Identity      (Identity (..))

import           Data.Union

import qualified Dhall.TypeCheck            as Dh

-- OpenUnion as -> Expr s (OpenUnion as)
type OpenTyper as = Dh.Typer (OpenUnion as)

typerUnion :: forall a as is. USubset as (a ': as) is => Dh.Typer a -> OpenTyper as -> OpenTyper (a ': as)
typerUnion ta tas = \aas ->
  union
    (\uas -> fmap urelax $ tas uas)
    (\(Identity a) -> fmap (ulift . Identity) $ ta a)
    aas

voidOpenTyper :: OpenTyper '[]
voidOpenTyper = absurdUnion
