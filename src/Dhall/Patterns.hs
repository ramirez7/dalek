{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

-- TODO: Should this be its own module, or should there be a more general
-- 'helpers' module for working with the AST that these are in?
module Dhall.Patterns where

import           Dhall.Core as Dh

pattern Apps :: [Dh.Expr t a] -> Dh.Expr t a
pattern Apps xs <- (gatherApps -> Just xs)
-- TODO: Make bidirectional?

pattern E :: a -> Dh.Expr t a
pattern E a = Dh.Embed a

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
