{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Dhall.Extensible.Core where

import           Control.Applicative
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Functor.Const        (Const (..))
import           Data.Open.Union

import qualified Dhall.Core                as Dh

-- TODO: These types can get a little better to help ensure we provide normalizers
-- for every embedded type..but for now this'll work

-- | Inspired by the "Term trick":
--
-- http://blog.sumtypeofway.com/recursion-schemes-part-41-2-better-living-through-base-functors/
--
-- @rec@ is typically kept polymorphic
newtype Rec s f rec = Rec { unRec :: f (Dh.Expr s (Rec s rec rec)) }

type Rec' s f = Rec s f f

mapRec :: Functor f => (forall a. f a -> g a) -> Rec s f rec -> Rec s g rec
mapRec f (Rec x) = Rec (f x)
{-
traverseRec :: (Monad m, Traversable f) => (forall a. f a -> m (g a)) -> Rec s f -> m (Rec s g)
traverseRec f (Rec x) = (fmap Rec . f) =<< (traverse (traverse (traverseRec f)) x)

deriving instance (Show (f (Dh.Expr s (Rec s f))), Show s) => Show (Rec s f)
-}
type OpenNormalizer s (fs :: [* -> *]) = Dh.Normalizer s (Rec' s (Union fs))

sendNormC :: forall s a fs. Member (Const a) fs => Dh.Normalizer s a -> OpenNormalizer s fs
sendNormC f = \exprU -> do
  (matched :: Dh.Expr s a) <- traverse (fmap getConst . prj . unRec) exprU
  normalized <- f matched
  pure $ fmap (Rec . inj . Const) normalized

{-
Union fs (Rec s (Union fs))
f (Rec s f)
-}
sendNormR :: forall s f fs. (Functor f, Member f fs) => (forall rec . Dh.Normalizer s (Rec s f rec)) -> OpenNormalizer s fs
sendNormR f = \exprU -> do
  (matched :: Dh.Expr s (Rec s f (Union fs))) <- traverse (fmap Rec . prj . unRec) exprU
  normalized <- f matched
  pure $ fmap (mapRec inj) normalized

{-
openNormalizers for extensions should be written like this
   c as
=> OpenNormalizer as
-> OpenNormalizer [a ': as]

so then we can build them up
-}
{-
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
-}
