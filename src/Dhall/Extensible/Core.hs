{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE KindSignatures         #-}

module Dhall.Extensible.Core where

import           Control.Applicative
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Functor.Identity     (Identity (..))
import           Data.Union

import qualified Dhall.Core                as Dh

type OpenNormalizer as = Dh.Normalizer (OpenUnion as)

normalizer :: Dh.Normalizer a -> Dh.Normalizer (OpenUnion as) -> Dh.Normalizer (OpenUnion (a ': as))
normalizer na nas = undefined

voidNormalizer :: OpenNormalizer '[]
voidNormalizer = const Nothing

{-
openNormalizers for extensions should be written like this
   c as
=> OpenNormalizer as
-> OpenNormalizer [a ': as]

so then we can build them up
-}

-- | TODO: Doc comment
sendNorm :: forall a as i. UElem a as i => Dh.Normalizer a -> OpenNormalizer as
sendNorm f = \exprU -> do
  (matched :: Dh.Expr s a) <- traverse (fmap runIdentity . umatch) exprU
  normalized <- f matched
  pure $ fmap (ulift . Identity) normalized

infixl 3 .<|>
-- | TODO: Doc comment
(.<|>) :: Dh.Normalizer a -> Dh.Normalizer a -> Dh.Normalizer a
nl .<|> nr = runMaybeT (MaybeT nl <|> MaybeT nr)
