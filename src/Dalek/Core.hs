{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Dalek.Core
  ( -- * Extensible Normalization
    OpenNormalizer
  , Open
  , OpenExpr
  , sendEmbed
  , (.<|>)
  -- * Note
  , unNote
  , reNote
  -- * Utilities
  , C (..)
  , xNormalizer
  , IsOpen
  , OpenSatisfies
  -- * Re-exports
  , Member
  , Members
  , X(..)
  , inj
  , prj
  -- * Internals
  , DalekUnion (..)
  , Rec (..)
  ,
  ) where

import           Control.Applicative
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Bifunctor            (first)
import           Data.Kind                 (Constraint)
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
--
-- This allows us to write our extensions @* -> *@ where the type variable
-- is the Dhall AST itself (along with all extensions..including the one we're writing)
--
-- This encoding allows for extensions to have recursive structures of extended Dhall ASTs
-- (for instance, containers like Map/Seq or custom AST syntax)
newtype Rec s f = Rec { unRec :: f (Dh.Expr s (Rec s f)) }

type Open s fs = Rec s (Union fs)
type OpenExpr s fs = Dh.Expr s (Open s fs)

type OpenNormalizer s (fs :: [* -> *]) = Dh.Normalizer s (Open s fs)

infixl 3 .<|>
-- | Normalizer alternative. Prefers the left-hand side.
(.<|>) :: Dh.Normalizer s a -> Dh.Normalizer s a -> Dh.Normalizer s a
nl .<|> nr = runMaybeT (MaybeT nl <|> MaybeT nr)

-- | Embed a value into an 'OpenExpr'
sendEmbed :: forall fs s f. Member f fs => f (OpenExpr s fs) -> OpenExpr s fs
sendEmbed a = Dh.Embed $ Rec $ inj a

-- | The same as 'Data.Functor.Const' but with different instances
newtype C c a = C { unC :: c } deriving (Functor, Eq, Ord, Buildable, Show)

-- | Normalizer for lifted 'X'
xNormalizer :: Member (C X) fs => OpenNormalizer s fs
xNormalizer = const Nothing
--------------------------------------------------------------------------------
-- Note stuff

-- Will need some instances on those fs to do this
unNoteOpen :: OpenExpr s fs -> OpenExpr X fs
unNoteOpen = undefined

-- | Remove all 'Note's from the AST
unNote :: Dh.Expr s a -> Dh.Expr t a
unNote = \case
  Dh.Note _ e -> unNote e
  Dh.Const x -> Dh.Const x
  Dh.Var x -> Dh.Var x
  Dh.Lam t e1 e2 -> Dh.Lam t (unNote e1) (unNote e2)
  Dh.Pi t e1 e2 -> Dh.Pi t (unNote e1) (unNote e2)
  Dh.App e1 e2 -> Dh.App (unNote e1) (unNote e2)
  Dh.Let t me1 e2 e3 -> Dh.Let t (fmap unNote me1) (unNote e2) (unNote e3)
  Dh.Annot e1 e2 -> Dh.Annot (unNote e1) (unNote e2)
  Dh.Bool -> Dh.Bool
  Dh.BoolLit b -> Dh.BoolLit b
  Dh.BoolAnd e1 e2 -> Dh.BoolAnd (unNote e1) (unNote e2)
  Dh.BoolOr e1 e2 -> Dh.BoolOr (unNote e1) (unNote e2)
  Dh.BoolEQ e1 e2 -> Dh.BoolEQ (unNote e1) (unNote e2)
  Dh.BoolNE e1 e2 -> Dh.BoolNE (unNote e1) (unNote e2)
  Dh.BoolIf e1 e2 e3 -> Dh.BoolIf (unNote e1) (unNote e2) (unNote e3)
  Dh.Natural -> Dh.Natural
  Dh.NaturalLit n -> Dh.NaturalLit n
  Dh.NaturalFold -> Dh.NaturalFold
  Dh.NaturalBuild -> Dh.NaturalBuild
  Dh.NaturalIsZero -> Dh.NaturalIsZero
  Dh.NaturalEven -> Dh.NaturalEven
  Dh.NaturalOdd -> Dh.NaturalOdd
  Dh.NaturalToInteger -> Dh.NaturalToInteger
  Dh.NaturalShow -> Dh.NaturalShow
  Dh.NaturalPlus e1 e2 -> Dh.NaturalPlus (unNote e1) (unNote e2)
  Dh.NaturalTimes e1 e2 -> Dh.NaturalTimes (unNote e1) (unNote e2)
  Dh.Integer -> Dh.Integer
  Dh.IntegerLit i -> Dh.IntegerLit i
  Dh.IntegerShow -> Dh.IntegerShow
  Dh.Double -> Dh.Double
  Dh.DoubleLit d -> Dh.DoubleLit d
  Dh.DoubleShow -> Dh.DoubleShow
  Dh.Text -> Dh.Text
  Dh.TextLit (Dh.Chunks chunks final) -> Dh.TextLit $ Dh.Chunks (fmap (\(t, e) -> (t, unNote e)) chunks) final
  Dh.TextAppend e1 e2 -> Dh.TextAppend (unNote e1) (unNote e2)
  Dh.List -> Dh.List
  Dh.ListLit me1 ve2 -> Dh.ListLit (fmap unNote me1) (fmap unNote ve2)
  Dh.ListAppend e1 e2 -> Dh.ListAppend (unNote e1) (unNote e2)
  Dh.ListBuild -> Dh.ListBuild
  Dh.ListFold -> Dh.ListFold
  Dh.ListLength -> Dh.ListLength
  Dh.ListHead -> Dh.ListHead
  Dh.ListLast -> Dh.ListLast
  Dh.ListIndexed -> Dh.ListIndexed
  Dh.ListReverse -> Dh.ListReverse
  Dh.Optional -> Dh.Optional
  Dh.OptionalLit e1 ve2  -> Dh.OptionalLit (unNote e1) (fmap unNote ve2)
  Dh.OptionalFold -> Dh.OptionalFold
  Dh.OptionalBuild -> Dh.OptionalBuild
  Dh.Record mpe -> Dh.Record (fmap unNote mpe)
  Dh.RecordLit mpe -> Dh.RecordLit (fmap unNote mpe)
  Dh.Union mpe -> Dh.Union (fmap unNote mpe)
  Dh.UnionLit t e1 mpe2 -> Dh.UnionLit t (unNote e1) (fmap unNote mpe2)
  Dh.Combine e1 e2 -> Dh.Combine (unNote e1) (unNote e2)
  Dh.Prefer e1 e2 -> Dh.Prefer (unNote e1) (unNote e2)
  Dh.Merge e1 e2 me3 -> Dh.Merge (unNote e1) (unNote e2) (fmap unNote me3)
  Dh.Constructors e -> Dh.Constructors (unNote e)
  Dh.Field e t -> Dh.Field (unNote e) t
  Dh.Embed a -> Dh.Embed a

-- | Convert a 'Note'-less AST to any other type of Noted AST
--
-- @
-- 'reNote' = 'first' 'absurd'
-- @
reNote :: Dh.Expr X a -> Dh.Expr s a
reNote = first absurd
--------------------------------------------------------------------------------
-- instances

type IsOpen s fs expr = expr ~ OpenExpr s fs

type OpenSatisfies (c :: * -> Constraint) s fs = c (DalekUnion fs (OpenExpr s fs))

instance Show (DalekUnion fs (OpenExpr s fs)) => Show (Open s fs) where
  show (Rec x) = show (DalekUnion x)

instance Eq (DalekUnion fs (OpenExpr s fs)) => Eq (Open s fs) where
  (Rec x) == (Rec y) = DalekUnion x == DalekUnion y

instance Ord (DalekUnion fs (OpenExpr s fs)) => Ord (Open s fs) where
  compare (Rec x) (Rec y) = compare (DalekUnion x) (DalekUnion y)

instance Buildable (DalekUnion fs (OpenExpr s fs)) => Buildable (Open s fs) where
  build (Rec x) = build (DalekUnion x)

-- | Newtype wrapper 'OpenUnion' so we can get some non-orphan instances we need
newtype DalekUnion fs a = DalekUnion (Union fs a)

instance (Show (f a)) => Show (DalekUnion '[f] a) where
  show (DalekUnion x) = show $ extract x

instance {-# OVERLAPPABLE #-} (Show (f a), Show (DalekUnion fs a)) => Show (DalekUnion (f ': fs) a) where
  show (DalekUnion x) = case decomp x of
    Right fv -> show fv
    Left uv  -> show (DalekUnion uv)

instance (Buildable (f a)) => Buildable (DalekUnion '[f] a) where
  build (DalekUnion x) = build $ extract x

instance {-# OVERLAPPABLE #-} (Buildable (f a), Buildable (DalekUnion fs a)) => Buildable (DalekUnion (f ': fs) a) where
  build (DalekUnion x) = case decomp x of
    Right fv -> build fv
    Left uv  -> build (DalekUnion uv)

instance (Eq (f a)) => Eq (DalekUnion '[f] a) where
  (DalekUnion x) == (DalekUnion y) = extract x == extract y

instance {-# OVERLAPPABLE #-} (Eq (f a), Eq (DalekUnion fs a)) => Eq (DalekUnion (f ': fs) a) where
  (DalekUnion x) == (DalekUnion y) = case decomp x of
    Right fx -> case decomp y of
      Right fy -> fx == fy
      Left _   -> False
    Left ux -> case decomp y of
      Left uy -> DalekUnion ux == DalekUnion uy
      Right _ -> False

instance (Ord (f a)) => Ord (DalekUnion '[f] a) where
  compare (DalekUnion x) (DalekUnion y) = compare (extract x) (extract y)

instance {-# OVERLAPPABLE #-} (Ord (f a), Ord (DalekUnion fs a)) => Ord (DalekUnion (f ': fs) a) where
  compare (DalekUnion x) (DalekUnion y) = case decomp x of
    Right fx -> case decomp y of
      Right fy -> compare fx fy
      Left _   -> GT
    Left ux -> case decomp y of
      Left uy -> compare (DalekUnion ux) (DalekUnion uy)
      Right _ -> LT
