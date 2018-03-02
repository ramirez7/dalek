* [X] Proof-of-concept use of `Dhall.Extensible` to use both `Dhall.Time` + `Dhall.Ord`
* `inline-dhall` TH that parses + typechecks + normalizes Dhall expressions for given Parser+Typer+Normalizer. Keep it generic enough so that `dalek`-created ones work fine.
* Make `dhall-repl` a standalone library. Add support for imports but keep extensible for `dalek`

# Interop
* FromDhall/ToDhall are canonical \*Types
* Dalek class that has typer/normalizer/parser(?). `function` would depend on it in addition to ToDhall
* `auto` for canonical like in vanilla Dhall
* InputType record/union combinators (maybe (r ->) monad?)
* Generic instances for record/union FromDhall/ToDhall. Make it the default
* `a -> Maybe b` where `Nothing` when it fails to deserialize.

# Usability
* Helpers for dealing with constraints. Already exists, but think it through. Maybe a different class instead of composition of a few instances/classes would be better? Regardless, clearly document this

# Tutorials
* Separate Interop tutorial
* Separate tutorial for writing extensions
* Tutorials for extensions themselves

# Imports
* In-memory

# Dhall.Repl
* Multiline editing
* Pretty printing (settable? A command?)
