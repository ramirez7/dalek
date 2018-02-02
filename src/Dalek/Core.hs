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

module Dalek.Core (module Dalek.Core, Member, Members, Const(..), inj, prj) where

import           Control.Applicative
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Functor.Const        (Const (..))
import           Data.Open.Union

import qualified Dhall.Core                as Dh

-- | Inspired by the "Term trick":
--
-- http://blog.sumtypeofway.com/recursion-schemes-part-41-2-better-living-through-base-functors/
newtype Rec s f = Rec { unRec :: f (Dh.Expr s (Rec s f)) }

mapRec :: forall s f g. Functor g => (forall a. f a -> g a) -> Rec s f -> Rec s g
mapRec f (Rec x) = (Rec (fmap (fmap (mapRec f)) (f x)))
{-
deriving instance (Show (f (Dh.Expr s (Rec s f))), Show s) => Show (Rec s f)
-}

type Open s fs = Rec s (Union fs)

type OpenNormalizer s (fs :: [* -> *]) = Dh.Normalizer s (Open s fs)

-- | This is useful for when we want to write normalizers ONLY in terms of
-- vanilla Dhall + our embedded a. Time is like that.
--
-- For normalizers with dependencies on other extensions, we'll have to be
-- more generic. Maybe some ViewPatterns / PatternSynonyms could help make those
-- still as nice to write as the current Apps versions (along with some extra
-- Member constraints)
--
-- Things like Map especially can't use this.
sendNorm :: forall s a fs. Member (Const a) fs => Dh.Normalizer s a -> OpenNormalizer s fs
sendNorm f = \exprU -> do
  (matched :: Dh.Expr s a) <- traverse (fmap getConst . prj . unRec) exprU
  normalized <- f matched
  pure $ fmap (Rec . inj . Const) normalized

infixl 3 .<|>
-- | TODO: Doc comment
(.<|>) :: Dh.Normalizer s a -> Dh.Normalizer s a -> Dh.Normalizer s a
nl .<|> nr = runMaybeT (MaybeT nl <|> MaybeT nr)

sendEmbed :: forall s a fs. Member (Const a) fs => a -> Dh.Expr s (Rec s (Union fs))
sendEmbed a = Dh.Embed $ Rec $ inj $ Const a
