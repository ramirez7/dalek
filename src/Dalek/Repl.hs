{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Dhall repl that can work with any dhall extension
module Dalek.Repl where
-- Strict StateT has the MonadException instance we need
import           Control.Applicative        (empty, optional)
import           Control.Monad.State.Strict (StateT (..), evalStateT, get,
                                             modify)
import           Control.Monad.Trans        (lift)
import           Data.List                  (foldl')
import           Data.List.NonEmpty         (NonEmpty, (<|))
import qualified Data.List.NonEmpty         as NEL
import           Data.Map                   (Map)
import qualified Data.Map                   as M

import           Data.Text.Buildable        (Buildable (..))
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB

import qualified Dhall.Context
import qualified Dhall.Core                 as Dh
import qualified Dhall.Parser               as Dh
import qualified Dhall.TypeCheck            as Dh

import           Dalek.Parser               (Result (..), label, parseDhallStr,
                                             reserved, whitespace)

import qualified System.Console.Haskeline   as HL

type SrcExpr a = Dh.Expr Dh.Src a

data LetBind a = LetBind !(Maybe (SrcExpr a)) !(SrcExpr a) deriving (Eq, Show)

normalizeLetBindWith :: Dh.Normalizer Dh.Src a -> LetBind a -> LetBind a
normalizeLetBindWith n (LetBind annot e) =
  LetBind (fmap (Dh.normalizeWith n) annot) (Dh.normalizeWith n e)

-- :let x = $$$
letBindParser :: Dh.Parser a -> Dh.Parser (Text, LetBind a)
letBindParser p = do
  whitespace
  reserved ":let"
  -- identifier
  name <- label
  annot <- optional (reserved ":" >> Dh.exprA p)
  reserved "="
  val <- Dh.exprA p
  pure $ (name, LetBind annot val)

parseLetBind :: Dh.Parser a -> String -> Result (Text, LetBind a)
parseLetBind p s = parseDhallStr (letBindParser p) s

unletAllParser :: Dh.Parser Text
unletAllParser = do
  whitespace
  reserved ":unlet"
  label

parseUnletAll :: String -> Result Text
parseUnletAll = parseDhallStr unletAllParser

unletParser :: Dh.Parser Text
unletParser = do
  whitespace
  reserved ":unlet1"
  label

parseUnlet :: String -> Result Text
parseUnlet = parseDhallStr unletParser

-- | The values closer to the head of the list shadow the ones further along
newtype LetScope a = LetScope (Map Text (NonEmpty (LetBind a))) deriving (Eq, Show, Monoid)

bindLet :: Text -> LetBind a -> LetScope a -> LetScope a
bindLet name binding (LetScope m) =
  let newVal = maybe (pure binding) (binding <|) (M.lookup name m)
   in LetScope $ M.insert name newVal m

unbindLet :: Text -> LetScope a -> LetScope a
unbindLet name (LetScope m) = LetScope $ M.update (NEL.nonEmpty . NEL.tail) name m

unbindAllLet :: Text -> LetScope a -> LetScope a
unbindAllLet name (LetScope m) = LetScope $ M.delete name m

mkLet :: LetScope a -> SrcExpr a -> SrcExpr a
mkLet (LetScope m) e =
    foldl' (\acc (name, (LetBind annot val)) -> Dh.Let name annot val acc) e
  $ M.toList
  $ fmap NEL.head m

bindLetM :: Monad m => Text -> LetBind a -> ReplM m a ()
bindLetM name val = lift $ modify (bindLet name val)

unbindLetM :: Monad m => Text -> ReplM m a ()
unbindLetM name = lift $ modify (unbindLet name)

unbindAllLetM :: Monad m => Text -> ReplM m a ()
unbindAllLetM name = lift $ modify (unbindAllLet name)

mkLetM :: Monad m => SrcExpr a -> ReplM m a (SrcExpr a)
mkLetM e = fmap (\sc -> mkLet sc e) (lift get)

-- TODO: Multi-line editing will require more state than LetScope.
type ReplM m a = HL.InputT (StateT (LetScope a) m)

runReplM :: HL.MonadException m => ReplM m a x -> m x
runReplM = flip evalStateT mempty . HL.runInputT HL.defaultSettings

repl :: forall a. (Eq a, Buildable a) => Dh.Parser a -> Dh.Normalizer Dh.Src a -> Dh.Typer Dh.Src a -> IO ()
repl p n t = runReplM loop
  where
    loop :: ReplM IO a ()
    loop = HL.getInputLine ">>> " >>= \case
      Nothing -> loop
      Just (parseLetBind p -> Success (name, letBind)) -> do
        let LetBind annot expr = letBind
        case fmap (\a -> typeOf (Dh.Annot expr a)) annot of
          Just (Left err) -> do
            displayTypeError err
            loop
          _ -> do
            bindLetM name letBind
            loop
      Just (parseUnlet -> Success name) -> do
        unbindLetM name
        loop
      Just (parseUnletAll -> Success name) -> do
        unbindAllLetM name
        loop
      Just (parseDhallStr (Dh.exprA p) -> Success expr) -> do
        fullExpr <- mkLetM expr
        case typeOf fullExpr of
          Left err -> do
            displayTypeError err
            loop
          Right type_ -> do
            let normType = Dh.normalizeWith n type_
            let res = Dh.normalizeWith n fullExpr
            let buildStr = TL.unpack . TLB.toLazyText . build
            HL.outputStrLn (buildStr normType)
            HL.outputStrLn ""
            HL.outputStrLn (buildStr res)
            loop
      Just _ -> do
        -- TODO: Better error message (maybe just put the dhall parser error msg)
        HL.outputStrLn "Unknown command"
        loop
    typeOf = Dh.typeWithAN n t Dhall.Context.empty

    displayTypeError err = HL.outputStrLn $ "Type error: \n" ++ show err

defaultRepl :: IO ()
defaultRepl = repl xParser xNormalizer xTyper

xParser :: Dh.Parser Dh.X
xParser = empty

xNormalizer :: Dh.Normalizer s Dh.X
xNormalizer = const Nothing

xTyper :: Dh.Typer s Dh.X
xTyper = Dh.absurd


{-
DESIGN

READ
- Locations might get weird?
- Allow for multi-line inputs
  - :paste mode like scala? (w/Ctrl-D to escape it)
  - :{ :} like ghci? How to get out of it?
  - both?
- haskeline might give us history for free?
- what about auto-complete? haskeline can help

EVAL
- Typecheck first
- Normalize with :let scope
- Auto bind to resX like scala?
  - Maybe make LetScope a sum type (auto vs manual) to allow for better gc
  - NOTE: +state
- Ctrl-C to cancel computation

PRINT
- Use Buildable, which should print valid dhall programs
- Also print the type above?

Commands:
- :let $NAME = $EXPR will allow you to bind variables globally
  - Multiple :let of the same name will shadow each other in bind order
- :unlet $NAME will unbind ALL variables of $NAME
- :unlet1 $NAME will unbind the most recent binding (allows un-shadowing)
- :type will print the type or type error
  - Currently, the print part of the loop prints the result type. Maybe allow
  - toggling of that
    - NOTE: +state
- :gc will remove all resX auto-bindings
  - NOTE: +state
- :paste
  - NOTE: +state
- :{ :}
  - NOTE: +state
- :? to show all commands
- :set $CONFIG for things like prompt
  - NOTE: +state
- :timing off/on
  - NOTE: +state

Other:
- Allow for other compiler passes (like imports)?
-}
