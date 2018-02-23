{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Dalek <-> Haskell interop
--
-- Interop that requires extensions is in @Dalek.Exts.*.Interop@ modules
module Dalek.Interop where

import           Control.Exception      (throwIO)
import           Control.Monad          (guard, unless)
import           Data.Functor.Alt       (Alt (..))
import           Data.Functor.Apply     (Apply (..))
import qualified Data.Map               as M
import           Data.Text.Buildable    (Buildable (..))
import qualified Data.Text
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import           System.IO.Error        (userError)
import qualified Data.Vector

import           Dhall                  (Natural)
import qualified Dhall.Context          as Dh
import qualified Dhall.Core             as Dh
import           Dhall.Parser           (Src, exprA)
import qualified Dhall.TypeCheck        as Dh

import           Dalek.Core
import           Dalek.Parser
import           Dhall.ParserUtils

input :: (Eq (Open Src fs), Buildable (Open Src fs))
      => OpenNormalizer Src fs
      -> OpenParser Src fs
      -> Dh.Typer Src (Open Src fs)
      -> OutputType fs a
      -> Text
      -> IO a
input n p t outTy prg = do
  expr <- case parseDhallStr (exprA p) (TL.unpack prg) of
            Success e   -> pure e
            Failure err -> throwIO $ userError $ "Parse Error: " ++ show err
  ty <- case Dh.typeWithA t Dh.empty expr of
          Right x  -> pure x
          Left err -> throwIO $ userError $ "Type Error: " ++ show err
  let normTy = Dh.normalizeWith n ty
  unless (normTy == expected outTy) $
    throwIO $ userError $ "Type mismatch: Expected " ++ buildStr (expected outTy) ++ "but got " ++ buildStr normTy
  let normExpr = Dh.normalizeWith n expr
  case extract outTy normExpr of
    Just a  -> pure a
    Nothing -> throwIO $ userError "Bad extract"

data OutputType fs a = OutputType
    { extract  :: OpenExpr Src fs -> Maybe a
    -- ^ Extracts Haskell value from the Dalek expression
    , expected :: OpenExpr Src fs
    -- ^ Dalek type of the Haskell value
    }
    deriving (Functor)

-- | This instance is only useful for record parsing
instance Apply (OutputType fs) where
  fa2b <.> fa = OutputType {
      extract = \expr -> do
        a2b <- extract fa2b expr
        a <- extract fa expr
        Just (a2b a)
    , expected = case (expected fa2b, expected fa) of
        (Dh.Record x, Dh.Record y) -> Dh.Record $ x `mappend` y
        (x, _)                     -> x
  }

-- | This instance is only useful for union parsing
instance Alt (OutputType fs) where
  lhs <!> rhs = OutputType {
      extract = \expr -> extract lhs expr <!> extract rhs expr
    , expected = case (expected lhs, expected rhs) of
        (Dh.Union x, Dh.Union y) -> Dh.Union $ x `mappend` y
        (x, _)                   -> x
  }

-- | Useful with the 'Apply' instance. For instance, @(Bool, Double)@ can be parsed with:
--
-- @
-- (,) <$> record "_1" bool <.> record "_2" double
-- @
record :: Text
       -> OutputType fs a
       -> OutputType fs a
record field fieldTy = OutputType {
    extract = \case
      Dh.RecordLit vals -> M.lookup field vals >>= extract fieldTy
      _ -> Nothing
  , expected = Dh.Record $ M.singleton field (expected fieldTy)
}

-- | Useful with the 'Alt' instance. For instance, @Either Bool Double@ can be parsed with:
--
-- @
-- union "Left" Left bool <!> union "Right" Right double
-- @
union :: Text
      -> (a -> r)
      -> OutputType fs a
      -> OutputType fs r
union tag ctor tagTy = OutputType {
    extract = \case
      Dh.UnionLit thisTag val _ -> do
        guard $ thisTag == tag
        a <- extract tagTy val
        pure $ ctor a
      _ -> Nothing
  , expected = Dh.Union $ M.singleton tag (expected tagTy)
}

raw :: OpenExpr Src fs -- Expected Dhall type
     -> OutputType fs (OpenExpr Src fs)
raw ty = OutputType Just ty

bool :: OutputType fs Bool
bool = OutputType {
    extract = \case
      Dh.BoolLit x -> Just x
      _ -> Nothing
  , expected = Dh.Bool
}

integer :: OutputType fs Integer
integer = OutputType {
    extract = \case
      Dh.IntegerLit x -> Just x
      _ -> Nothing
  , expected = Dh.Integer
}

double :: OutputType fs Double
double = OutputType {
    extract = \case
      Dh.DoubleLit x -> Just x
      _ -> Nothing
  , expected = Dh.Double
}

natural :: OutputType fs Natural
natural = OutputType {
    extract = \case
      Dh.NaturalLit x -> Just x
      _ -> Nothing
  , expected = Dh.Natural
}

lazyText :: OutputType fs Text
lazyText = OutputType {
    extract = \case
      Dh.TextLit (Dh.Chunks [] t) -> Just (TLB.toLazyText t)
      _ -> Nothing
  , expected = Dh.Text
}

strictText :: OutputType fs Data.Text.Text
strictText = TL.toStrict <$> lazyText

optional :: OutputType fs a -> OutputType fs (Maybe a)
optional aTy = OutputType {
    extract = \case
       Dh.OptionalLit _ v ->
         traverse (extract aTy) $
           if Data.Vector.null v
             then Nothing
             else Just (Data.Vector.head v)
       _ -> Nothing
  , expected = Dh.App Dh.Optional (expected aTy)
}

list :: OutputType fs a -> OutputType fs [a]
list = fmap Data.Vector.toList . vector

vector :: OutputType fs a -> OutputType fs (Data.Vector.Vector a)
vector aTy = OutputType {
    extract = \case
      Dh.ListLit _ v -> traverse (extract aTy) v
      _ -> Nothing
  , expected = Dh.App Dh.List (expected aTy)
}
string :: OutputType fs String
string = TL.unpack <$> lazyText

unit :: OutputType fs ()
unit = OutputType {
    extract = \case
      Dh.RecordLit fields | M.null fields -> Just ()
      _ -> Nothing
  , expected = Dh.Record M.empty
}

pair :: OutputType fs a -> OutputType fs b -> OutputType fs (a, b)
pair aTy bTy = (,) <$> record "_1" aTy <.> record "_2" bTy

function :: OpenNormalizer Src fs -> InputType fs a -> OutputType fs b -> OutputType fs (a -> b)
function nrm inTy outTy = undefined

data InputType fs a = InputType
    { embed    :: a -> OpenExpr Src fs
    -- ^ Embeds a Haskell value as a Dalek expression
    , declared :: OpenExpr Src fs
    -- ^ Dalek type of the Haskell value
    }

class FromDhall fs a where
  fromDhall :: InteropOptions -> OutputType fs a

class ToDhall fs a where
  toDhall :: InteropOptions -> InputType fs a

data InteropOptions = InteropOptions
    { fieldModifier       :: Text -> Text
    -- ^ Function used to transform Haskell field names into their corresponding
    --   Dhall field names
    , constructorModifier :: Text -> Text
    -- ^ Function used to transform Haskell constructor names into their
    --   corresponding Dhall alternative names
    }

buildStr :: Buildable a => a -> String
buildStr = TL.unpack . TLB.toLazyText . build
