{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Pattern Synonyms that help with writing your own Dhall extensions
module Dalek.Patterns
  ( pattern Apps
  , pattern E
  , pattern EC
  , gatherApps
  , ungatherApps
  ) where

import Data.List (foldl')

import qualified Dhall.Core as Dh

import           Dalek.Core

{-|

Application is implemented in Dhall with currying, so multiple-arg applications
result in nested 'App' constructors. This can become a pain because custom
Dhall normalization always requires the programmer to pattern match on 'App'.

This pattern helps clean up pattern matching on arbitrarily long applications:

@
Apps [f, x] = App f x
Apps [f, x, y] = App (App f x) y
Apps [f, x, y, z] = App (App (App f x) y) z
Apps [f] = NEVER MATCHES
Apps [] = NEVER MATCHES
@
-}
pattern Apps :: [Dh.Expr t a] -> Dh.Expr t a
pattern Apps xs <- (gatherApps -> Just xs)
-- TODO: Make bidirectional?
-- TODO: Once it's bidirectional, don't use a list. Do `Apps x y [z, w, ...]` for totality

-- | Pattern meant to help with matching on 'Embed'ded 'OpenUnion' terms.
pattern E :: forall f s fs. Member f fs => f (Dh.Expr X (Open fs)) -> Dh.Expr s (Open fs)
pattern E a <- Dh.Embed (prj . unRec -> Just a)
-- TODO: Make bidirectional? Would replace usage of sendEmbed I believe?

-- | Helpful for matching on a kind @*@ term that has been lifted to @* -> *@ using 'C'
pattern EC :: forall a s fs. Member (C a) fs => a -> Dh.Expr s (Open fs)
pattern EC a <- Dh.Embed (prj . unRec -> Just (C a))

-- TODO: Pattern for TextLit + Chunks []

-- TODO: Handle Notes here somehow?
gatherApps :: Dh.Expr t a -> Maybe [Dh.Expr t a]
gatherApps = \case
  Dh.App (Dh.App x y) z -> case gatherApps x of
    Just xs -> Just $ xs ++ [y,z]
    Nothing -> Just $ [x,y,z]
  Dh.App x y -> Just [x, y]
  _ -> Nothing

ungatherApps :: [Dh.Expr t a] -> Maybe (Dh.Expr t a)
ungatherApps = \case
  x : y : xs -> Just $ foldl' (\acc expr -> Dh.App acc expr) (Dh.App x y) xs
  _ -> Nothing
