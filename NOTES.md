* Custom `App` logic using `normalizeWith` will do everything I did but simpler
* `exprA` can't produce custom `App` parses. Need

```haskell
exprX :: Parser (Expr Src X)
```
which can then be combined with a `Parser (Expr Src a)` w/custom `App` parsing
(for instance, parsing `App (App GT (DoubleLit 1.0)) (DoubleLit 1.2)`). Maybe using
`Alternative`. Then we can have

```
myExpr :: Parser (Expr Src a) -> Parser (Expr Src a)
```
