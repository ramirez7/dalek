{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Dalek.Core (module Dalek.Core, Member, Members, C(..), X(..), inj, prj) where

import           Control.Applicative
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Open.Union
import           Data.Text.Buildable       (Buildable (..))

import qualified Dhall.Core                as Dh
import           Dhall.TypeCheck           (X (..))

-- TODO: IDEA: :git ghci command lol
{-
λ: :kind! Dh.Normalizer Int Bool
Dh.Normalizer Int Bool :: *
= Dh.Normalizer Int Bool
λ: type family Id a where Id a = a
λ: :kind! Id (Dh.Normalizer Int Bool)
Id (Dh.Normalizer Int Bool) :: *
= Dh.Expr Int Bool -> Maybe (Dh.Expr Int Bool)
-}

-- | Inspired by the "Term trick":
--
-- http://blog.sumtypeofway.com/recursion-schemes-part-41-2-better-living-through-base-functors/
newtype Rec s f = Rec { unRec :: f (Dh.Expr s (Rec s f)) }

mapRec :: forall s f g. Functor g => (forall a. f a -> g a) -> Rec s f -> Rec s g
mapRec f (Rec x) = (Rec (fmap (fmap (mapRec f)) (f x)))

type Open s fs = Rec s (Union fs)
type OpenExpr s fs = Dh.Expr s (Open s fs)

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
sendNorm :: forall s f fs. Member f fs => Dh.Normalizer s (f (OpenExpr s fs)) -> OpenNormalizer s fs
sendNorm f = \exprU -> do
  matched <- traverse (prj . unRec) exprU
  normalized <- f matched
  pure $ fmap (Rec . inj) normalized

infixl 3 .<|>
-- | TODO: Doc comment
(.<|>) :: Dh.Normalizer s a -> Dh.Normalizer s a -> Dh.Normalizer s a
nl .<|> nr = runMaybeT (MaybeT nl <|> MaybeT nr)

sendEmbed :: forall fs s f. Member f fs => f (OpenExpr s fs) -> OpenExpr s fs
sendEmbed a = Dh.Embed $ Rec $ inj a

newtype C c a = C { unC :: c } deriving (Functor, Eq, Ord, Buildable, Show)

xNormalizer :: Member (C X) fs => OpenNormalizer s fs
xNormalizer = const Nothing
--------------------------------------------------------------------------------
-- instances

instance Show (OrphanUnion fs (OpenExpr s fs)) => Show (Open s fs) where
  show (Rec x) = show (OrphanUnion x)

instance Eq (OrphanUnion fs (OpenExpr s fs)) => Eq (Open s fs) where
  (Rec x) == (Rec y) = OrphanUnion x == OrphanUnion y

instance Ord (OrphanUnion fs (OpenExpr s fs)) => Ord (Open s fs) where
  compare (Rec x) (Rec y) = compare (OrphanUnion x) (OrphanUnion y)

instance Buildable (OrphanUnion fs (OpenExpr s fs)) => Buildable (Open s fs) where
  build (Rec x) = build (OrphanUnion x)

newtype OrphanUnion fs a = OrphanUnion (Union fs a)

instance (Show (f a)) => Show (OrphanUnion '[f] a) where
  show (OrphanUnion x) = show $ extract x

instance {-# OVERLAPPABLE #-} (Show (f a), Show (OrphanUnion fs a)) => Show (OrphanUnion (f ': fs) a) where
  show (OrphanUnion x) = case decomp x of
    Right fv -> show fv
    Left uv  -> show (OrphanUnion uv)

instance (Buildable (f a)) => Buildable (OrphanUnion '[f] a) where
  build (OrphanUnion x) = build $ extract x

instance {-# OVERLAPPABLE #-} (Buildable (f a), Buildable (OrphanUnion fs a)) => Buildable (OrphanUnion (f ': fs) a) where
  build (OrphanUnion x) = case decomp x of
    Right fv -> build fv
    Left uv  -> build (OrphanUnion uv)

instance (Eq (f a)) => Eq (OrphanUnion '[f] a) where
  (OrphanUnion x) == (OrphanUnion y) = extract x == extract y

instance {-# OVERLAPPABLE #-} (Eq (f a), Eq (OrphanUnion fs a)) => Eq (OrphanUnion (f ': fs) a) where
  (OrphanUnion x) == (OrphanUnion y) = case decomp x of
    Right fx -> case decomp y of
      Right fy -> fx == fy
      Left _   -> False
    Left ux -> case decomp y of
      Left uy -> OrphanUnion ux == OrphanUnion uy
      Right _ -> False

instance (Ord (f a)) => Ord (OrphanUnion '[f] a) where
  compare (OrphanUnion x) (OrphanUnion y) = compare (extract x) (extract y)

instance {-# OVERLAPPABLE #-} (Ord (f a), Ord (OrphanUnion fs a)) => Ord (OrphanUnion (f ': fs) a) where
  compare (OrphanUnion x) (OrphanUnion y) = case decomp x of
    Right fx -> case decomp y of
      Right fy -> compare fx fy
      Left _   -> GT
    Left ux -> case decomp y of
      Left uy -> compare (OrphanUnion ux) (OrphanUnion uy)
      Right _ -> LT
