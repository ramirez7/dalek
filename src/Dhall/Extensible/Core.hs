{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Dhall.Extensible.Core where

import           Control.Applicative
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Functor.Identity     (Identity (..))
import           Data.Union

import qualified Dhall.Core                as Dh

-- TODO: These types can get a little better to help ensure we provide normalizers
-- for every embedded type..but for now this'll work

type OpenNormalizer as = Dh.Normalizer (OpenUnion as)

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

sendEmbed :: forall s a as i. UElem a as i => a -> Dh.Expr s (OpenUnion as)
sendEmbed a = Dh.Embed $ ulift $ Identity a
