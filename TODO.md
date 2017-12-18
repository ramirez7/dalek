* Proof-of-concept use of `Dhall.Extensible` to use both `Dhall.Time` + `Dhall.Ord`
* Dhall -> Haskell conversion utils (current ones in `dhall-haskell` are `X` specific)
* Embed `Data.Map`..somehow. This will require some more PRs to `dhall-haskell` I think. Look at how `List` is implemented as inspiration.
  * The `Ord` constraint could be problematic..but `(Ord s, Ord a) => Expr s a` seems doable. But kind of arbitrary.
* Look into extending with a basic constraints system. Probably as a separate compiler pass that turns constraints into Dictionary arguments + fills the values in from known instances.

Some ideas:
```haskell
data DhMap s a = forall k v. Ord k => DhMapLit (Map k v) (k -> Expr s a) (Expr s a -> Maybe k) (v -> Expr s a) (Expr s a -> v)
```
