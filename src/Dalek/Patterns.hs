{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ViewPatterns          #-}

module Dalek.Patterns where

import qualified Dhall.Core as Dh

import           Dalek.Core

pattern Apps :: [Dh.Expr t a] -> Dh.Expr t a
pattern Apps xs <- (gatherApps -> Just xs)
-- TODO: Make bidirectional?

-- | TODO: I don't think this has a place in this new higher-kinded world
pattern E :: a -> Dh.Expr t a
pattern E a = Dh.Embed a

pattern EC :: forall a s fs. Member (Const a) fs => a -> Dh.Expr s (Open s fs)
pattern EC a <- Dh.Embed (prj . unRec -> Just (Const a))

pattern ER :: forall f s fs. Member f fs => f (Dh.Expr s (Open s fs)) -> Dh.Expr s (Open s fs)
pattern ER a <- Dh.Embed (prj . unRec -> Just a)

gatherApps :: Dh.Expr t a -> Maybe [Dh.Expr t a]
gatherApps = \case
  Dh.App (Dh.App x y) z -> case gatherApps x of
    Just xs -> Just $ xs ++ [y,z]
    Nothing -> Just $ [x,y,z]
  Dh.App x y -> Just [x, y]
  _ -> Nothing

{-
Apps [x, y] = App x y
Apps [x, y, z] = App (App x y) z
Apps [x, y, z, w] = App (App (App x y) z) w
Apps [x] = NO
Apps [] = NO

f x y z
App (App (App (f x)) y) z
Apps [f, x, y, z]
-}
