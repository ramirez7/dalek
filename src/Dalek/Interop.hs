{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Dalek <-> Haskell interop
--
-- Interop that requires extensions is in @Dalek.Exts.*.Interop@ modules
module Dalek.Interop
  ( input
  , OutputType (..)
  , InputType (..)
  -- * OutputType combinators
  , raw
  , bool
  , integer
  , natural
  , scientific
  , double
  , lazyText
  , strictText
  , string
  , optional
  , list
  , vector
  , unit
  , pair
  , function
  , record
  , union
  -- * WIP
  , FromDhall (..)
  , ToDhall (..)
  , InteropOptions (..)) where

import           Control.Exception          (throwIO)
import           Control.Monad              (guard, unless)
import           Data.Functor.Alt           (Alt (..))
import           Data.Functor.Apply         (Apply (..))
import qualified Data.HashMap.Strict.InsOrd as HMI
import           Data.Scientific            (Scientific, toRealFloat)
import qualified Data.Text
import           Data.Text.Buildable        (Buildable (..))
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB
import qualified Data.Vector
import           System.IO.Error            (userError)

import           Dhall                      (Natural)
import qualified Dhall.Context              as Dh
import qualified Dhall.Core                 as Dh
import           Dhall.Parser               (Src, exprA)
import qualified Dhall.TypeCheck            as Dh

import           Dalek.Core
import           Dalek.Parser
import           Dhall.ParserUtils

input :: (Eq (Open Src fs), Buildable (Open Src fs))
      => OpenNormalizer Src fs -- ^ Custom normalizer
      -> OpenParser Src fs -- ^ Custom parser
      -> Dh.Typer Src (Open Src fs) -- ^ Custom typer
      -> OutputType fs a -- ^ 'OutputType' to unmarshal to
      -> Text -- ^ Dhall program
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

-- | Specification of how to unmarshal from a Dhall value to a Haskell value
data OutputType fs a = OutputType
    { extract  :: OpenExpr Src fs -> Maybe a
    -- ^ Extracts Haskell value from the Dalek expression
    , expected :: OpenExpr Src fs
    -- ^ Dalek type of the Haskell value
    }
    deriving (Functor)

-- | If the 'OutputType's are expecting 'Record's, this instance will combine
-- the record types and extract them both.
--
-- Meant to be used with the 'record' combinator
--
-- Otherwise, it will behave in undefined (but still lawful) ways
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


-- | If the 'OutputType's are expecting 'Union's, this instance will combine
-- the union types and try to parse each case.
--
-- Meant to be used with the 'union' combinator
--
-- Otherwise, it just prefers the left-hand side
instance Alt (OutputType fs) where
  lhs <!> rhs = case (expected lhs, expected rhs) of
    (Dh.Union lTy, Dh.Union rTy) -> OutputType {
          extract = \expr -> extract lhs expr <!> extract rhs expr
        , expected = Dh.Union $ lTy `mappend` rTy
      }
    (lTy, _j) -> OutputType {
          extract = extract lhs
        , expected = lTy
      }

-- | Useful with the 'Apply' instance. For instance, @(Bool, Double)@ can be parsed with:
--
-- @
-- (,) <$> 'record' \"_1\" 'bool' '<.>' record \"_2\" 'double'
-- @
record :: Text
       -> OutputType fs a
       -> OutputType fs a
record field fieldTy = OutputType {
    extract = \case
      Dh.RecordLit vals -> HMI.lookup field vals >>= extract fieldTy
      _ -> Nothing
  , expected = Dh.Record $ HMI.singleton field (expected fieldTy)
}

-- | Useful with the 'Alt' instance. For instance, @Either Bool Double@ can be parsed with:
--
-- @
-- 'union' \"Left\" 'Left' 'bool' '<!>' 'union' \"Right\" 'Right' 'double'
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
  , expected = Dh.Union $ HMI.singleton tag (expected tagTy)
}

-- | Extract a Dhall `Expr` as-is. All that will be done is checking against
-- the provided type.
--
-- Useful when parsing parametric Dhall programs
raw :: OpenExpr Src fs -- ^ Expected Dhall type
     -> OutputType fs (OpenExpr Src fs)
raw ty = OutputType Just ty

-- | Extract a 'Bool'
bool :: OutputType fs Bool
bool = OutputType {
    extract = \case
      Dh.BoolLit x -> Just x
      _ -> Nothing
  , expected = Dh.Bool
}

-- | Extract an 'Integer'
integer :: OutputType fs Integer
integer = OutputType {
    extract = \case
      Dh.IntegerLit x -> Just x
      _ -> Nothing
  , expected = Dh.Integer
}

-- | Extract a 'Scientific'
scientific :: OutputType fs Scientific
scientific = OutputType {
    extract = \case
      Dh.DoubleLit x -> Just x
      _ -> Nothing
  , expected = Dh.Double
}

-- | Extract a 'Double'
double :: OutputType fs Double
double = OutputType {
    extract = \case
      Dh.DoubleLit x -> Just (toRealFloat x)
      _ -> Nothing
  , expected = Dh.Double
}

-- | Extract a 'Natural'
natural :: OutputType fs Natural
natural = OutputType {
    extract = \case
      Dh.NaturalLit x -> Just x
      _ -> Nothing
  , expected = Dh.Natural
}

-- | Extract a lazy 'Text'
lazyText :: OutputType fs Text
lazyText = OutputType {
    extract = \case
      Dh.TextLit (Dh.Chunks [] t) -> Just (TLB.toLazyText t)
      _ -> Nothing
  , expected = Dh.Text
}

-- | Extract a strict 'Data.Text.Text'
strictText :: OutputType fs Data.Text.Text
strictText = TL.toStrict <$> lazyText

-- | Extract a 'Maybe'
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

-- | Extract a list
list :: OutputType fs a -> OutputType fs [a]
list = fmap Data.Vector.toList . vector

-- | Extract a 'Vector'
vector :: OutputType fs a -> OutputType fs (Data.Vector.Vector a)
vector aTy = OutputType {
    extract = \case
      Dh.ListLit _ v -> traverse (extract aTy) v
      _ -> Nothing
  , expected = Dh.App Dh.List (expected aTy)
}

-- | Extract a 'String'
string :: OutputType fs String
string = TL.unpack <$> lazyText

-- | Extract @()@, as the empty record @{=}@
unit :: OutputType fs ()
unit = OutputType {
    extract = \case
      Dh.RecordLit fields | HMI.null fields -> Just ()
      _ -> Nothing
  , expected = Dh.Record HMI.empty
}

-- | Extract a Tuple, as the Dhall @{_1 : a, _2 : b }@
pair :: OutputType fs a -> OutputType fs b -> OutputType fs (a, b)
pair aTy bTy = (,) <$> record "_1" aTy <.> record "_2" bTy

-- | Extract a function
function :: OpenNormalizer Src fs -> InputType fs a -> OutputType fs b -> OutputType fs (a -> b)
function nrm inTy outTy = undefined

-- | Specification of how to marshal from a Haskell value to a Dhall value
data InputType fs a = InputType
    { embed    :: a -> OpenExpr Src fs
    -- ^ Embeds a Haskell value as a Dalek expression
    , declared :: OpenExpr Src fs
    -- ^ Dalek type of the Haskell value
    }

-- | WIP: Equivalent of dhall's 'Interpret' type class
class FromDhall fs a where
  fromDhall :: InteropOptions -> OutputType fs a

-- | WIP: Equivalent of dhall's 'Inject' type class
class ToDhall fs a where
  toDhall :: InteropOptions -> InputType fs a

-- | WIP: Equivalent of dhall's 'InterpretOptions' type
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
