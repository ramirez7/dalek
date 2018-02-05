* [X] Proof-of-concept use of `Dhall.Extensible` to use both `Dhall.Time` + `Dhall.Ord`
* `inline-dhall` TH that parses + typechecks + normalizes Dhall expressions for given Parser+Typer+Normalizer. Keep it generic enough so that `dalek`-created ones work fine.
* Make `dhall-repl` a standalone library. Add support for imports but keep extensible for `dalek`
