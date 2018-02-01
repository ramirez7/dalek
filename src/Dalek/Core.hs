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

module Dalek.Core where

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
newtype Rec s f = Rec { unRec :: f (Dh.Expr s (Rec s f)) }

--mapRec :: Functor f => (forall a. f a -> g a) -> Rec s f -> Rec s g
--mapRec f (Rec x) = Rec (f x)
{-
traverseRec :: (Monad m, Traversable f) => (forall a. f a -> m (g a)) -> Rec s f -> m (Rec s g)
traverseRec f (Rec x) = (fmap Rec . f) =<< (traverse (traverse (traverseRec f)) x)

deriving instance (Show (f (Dh.Expr s (Rec s f))), Show s) => Show (Rec s f)
-}
type OpenNormalizer s (fs :: [* -> *]) = Dh.Normalizer s (Rec s (Union fs))

-- This doesn't work. It'll turn to nothing if you mix-and-match embeds -_-
--
-- You can't get this type signature usefully. You have to write the Normalizer
-- generic up-front
sendNormC :: forall s a fs. Member (Const a) fs => Dh.Normalizer s a -> OpenNormalizer s fs
sendNormC _ = undefined

infixl 3 .<|>
-- | TODO: Doc comment
(.<|>) :: Dh.Normalizer s a -> Dh.Normalizer s a -> Dh.Normalizer s a
nl .<|> nr = runMaybeT (MaybeT nl <|> MaybeT nr)

sendEmbed :: forall s a fs. Member (Const a) fs => a -> Dh.Expr s (Rec s (Union fs))
sendEmbed a = Dh.Embed $ Rec $ inj $ Const a

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




-}
