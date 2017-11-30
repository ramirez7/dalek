module Dhall.Utils where

import qualified Dhall.Core as Dh

app2 :: a -> Dh.Expr s a -> Dh.Expr s a -> Dh.Expr s a
app2 a x y = (Dh.App (Dh.App a x) y)
