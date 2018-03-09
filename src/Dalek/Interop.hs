{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE BangPatterns          #-}


-- | Dalek <-> Haskell interop
--
-- Interop that requires extensions is in @Dalek.Exts.*.Interop@ modules
module Dalek.Interop
  ( input
  , OutputType (..)
  , InputType (..)
  , extract
  -- * OutputType combinators
  , raw
  , forAll
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
  -- * InputType combinators
  , rawIn
  , boolIn
  , integerIn
  , naturalIn
  , scientificIn
  , doubleIn
  , lazyTextIn
  , strictTextIn
  , stringIn
  , optionalIn
  , listIn
  , vectorIn
  , unitIn
  -- * WIP
  , FromDhall (..)
  , ToDhall (..)
  , InteropOptions (..)) where

import           Control.Exception          (Exception, throwIO)
import           Control.Monad              (guard)
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Foldable              as F
import           Data.Functor.Alt           (Alt (..))
import           Data.Functor.Apply         (Apply (..))
import           Data.Functor.Contravariant
import qualified Data.HashMap.Strict.InsOrd as HMI
import           Data.Scientific            (Scientific, fromFloatDigits,
                                             toRealFloat)
import qualified Data.Text
import           Data.Text.Buildable        (Buildable (..))
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.Vector
import           System.IO.Error            (IOError, userError)

import           Dhall                      (Natural)
import qualified Dhall.Context              as Dh
import qualified Dhall.Core                 as Dh
import           Dhall.Parser               (Src (..), exprA)
import qualified Dhall.TypeCheck            as Dh

import           Dalek.Core
import           Dalek.Parser
import           Dalek.Patterns

input :: forall fs a
       . (OpenSatisfies Eq Src fs, OpenSatisfies Buildable Src fs)
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
  annot <- case annotWith outTy expr of
    Just a  -> pure a
    Nothing -> throwIO $ userError "Bad annot"
  _ <- throws $ first mkTypeException $ Dh.typeWithAN n t Dh.empty annot
  let normExpr = Dh.normalizeWith n expr
  case extract outTy normExpr of
    Just a  -> pure a
    Nothing -> throwIO $ userError "bad extract"

mkTypeException :: Buildable a => Dh.TypeError Src a -> IOError
mkTypeException e = userError $ "Type Error: " ++ buildStr e

throws :: Exception e => Either e a -> IO a
throws (Left  e) = throwIO e
throws (Right r) = return r

-- TODO: This is all very tricky...

-- TODO: Maybe we can make this parametric over another type variable
-- ty :: ([* -> *] -> * -> *) to make this work for Output and Input types
-- Idk if the Input one makes sense though
data OutputType fs a =
    QuantifiedOutputType
      { quantifiedExpected :: OpenExpr Src fs
      -- ^ The kind of the type parameter
      , quantifiedExtract  :: OpenExpr Src fs -> OutputType fs a
      -- ^ OutputType logic that depends on the type parameter
      }
  | ConcreteOutputType
      { concreteExtract  :: OpenExpr Src fs -> Maybe a
      -- ^ Extracts Haskell value from the Dalek expression
      , concreteExpected :: OpenExpr Src fs
      -- ^ Dalek type of the Haskell value
     }
  deriving Functor

extract :: OutputType fs a -> OpenExpr Src fs -> Maybe a
extract QuantifiedOutputType{..} = \case
  Apps (f : ty : xs) -> ungatherApps (f : xs) >>= extract (quantifiedExtract ty)
  _ -> Nothing
extract ConcreteOutputType{..} = concreteExtract

-- TODO: It would be nice to not destroy all the Notes here, but I don't know how
annotWith :: forall fs a
           . OpenSatisfies Buildable Src fs
          => OutputType fs a
          -> OpenExpr Src fs
          -> Maybe (OpenExpr Src fs)
annotWith aTy expr = case (unNote expr :: OpenExpr Src fs) of
  Dh.Note src e -> Dh.Note src <$> annotWith aTy e
  Apps (f : xs) -> do
    (exprTy, annotArgs) <- go xs aTy
    argAnnotExpr <- ungatherApps (f : annotArgs)
    Just $ annotExpr argAnnotExpr exprTy
  e -> case aTy of
    ConcreteOutputType{..}   -> Just $ annotExpr e concreteExpected
    QuantifiedOutputType _ _ -> Nothing
  where
    go (ty : rest) QuantifiedOutputType{..} = do
      (outTy, annotRest) <- go rest (quantifiedExtract ty)
      Just $ (outTy, annotExpr ty quantifiedExpected : annotRest)
    go xs ConcreteOutputType{..} = Just $ (concreteExpected, xs)
    go [] (QuantifiedOutputType _ _) = Nothing

annotExpr :: Buildable a => Dh.Expr Src a -> Dh.Expr Src a -> Dh.Expr Src a
annotExpr expr ty = case expr of
  Dh.Note (Src begin end bytes) _ ->
      Dh.Note (Src begin end bytes') (Dh.Annot expr ty)
    where
      bytes' = bytes `mappend` " : " `mappend` suffix
  _ -> Dh.Annot expr ty
  where
    suffix = BL.toStrict $ TL.encodeUtf8 $ TLB.toLazyText $ build $ ty

-- TODO: Replace OutputType with QuantifiedOutputType entirely (but call it OutputType)

-- | For example:
--
-- @
-- forAll (Const Type) $ \\ty -> paramTy ty
-- @
forAll :: OpenExpr Src fs -> (OpenExpr Src fs -> OutputType fs a) -> OutputType fs a
forAll = QuantifiedOutputType

-- | If the 'OutputType's are expecting 'Record's, this instance will combine
-- the record types and concreteExtract them both.
--
-- Meant to be used with the 'record' combinator
--
-- Otherwise, it will behave in undefined (but still lawful) ways
instance Apply (OutputType fs) where
  fa2b <.> fa = ConcreteOutputType {
      concreteExtract = \expr -> do
        a2b <- concreteExtract fa2b expr
        a <- concreteExtract fa expr
        Just (a2b a)
    , concreteExpected = case (concreteExpected fa2b, concreteExpected fa) of
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
  lhs <!> rhs = case (concreteExpected lhs, concreteExpected rhs) of
    (Dh.Union lTy, Dh.Union rTy) -> ConcreteOutputType {
          concreteExtract = \expr -> concreteExtract lhs expr <!> concreteExtract rhs expr
        , concreteExpected = Dh.Union $ lTy `mappend` rTy
      }
    (lTy, _j) -> ConcreteOutputType {
          concreteExtract = concreteExtract lhs
        , concreteExpected = lTy
      }

-- | Useful with the 'Apply' instance. For instance, @(Bool, Double)@ can be parsed with:
--
-- @
-- (,) <$> 'record' \"_1\" 'bool' '<.>' record \"_2\" 'double'
-- @
record :: Text
       -> OutputType fs a
       -> OutputType fs a
record field fieldTy = ConcreteOutputType {
    concreteExtract = \case
      Dh.RecordLit vals -> HMI.lookup field vals >>= concreteExtract fieldTy
      _ -> Nothing
  , concreteExpected = Dh.Record $ HMI.singleton field (concreteExpected fieldTy)
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
union tag ctor tagTy = ConcreteOutputType {
    concreteExtract = \case
      Dh.UnionLit thisTag val _ -> do
        guard $ thisTag == tag
        a <- concreteExtract tagTy val
        pure $ ctor a
      _ -> Nothing
  , concreteExpected = Dh.Union $ HMI.singleton tag (concreteExpected tagTy)
}

-- | Extract a Dhall `Expr` as-is. All that will be done is checking against
-- the provided type.
--
-- Useful when parsing parametric Dhall programs
raw :: OpenExpr Src fs -- ^ Expected Dhall type
     -> OutputType fs (OpenExpr Src fs)
raw ty = ConcreteOutputType Just ty

-- | Extract a 'Bool'
bool :: OutputType fs Bool
bool = ConcreteOutputType {
    concreteExtract = \case
      Dh.BoolLit x -> Just x
      _ -> Nothing
  , concreteExpected = Dh.Bool
}

-- | Extract an 'Integer'
integer :: OutputType fs Integer
integer = ConcreteOutputType {
    concreteExtract = \case
      Dh.IntegerLit x -> Just x
      _ -> Nothing
  , concreteExpected = Dh.Integer
}

-- | Extract a 'Scientific'
scientific :: OutputType fs Scientific
scientific = ConcreteOutputType {
    concreteExtract = \case
      Dh.DoubleLit x -> Just x
      _ -> Nothing
  , concreteExpected = Dh.Double
}

-- | Extract a 'Double'
double :: OutputType fs Double
double = ConcreteOutputType {
    concreteExtract = \case
      Dh.DoubleLit x -> Just (toRealFloat x)
      _ -> Nothing
  , concreteExpected = Dh.Double
}

-- | Extract a 'Natural'
natural :: OutputType fs Natural
natural = ConcreteOutputType {
    concreteExtract = \case
      Dh.NaturalLit x -> Just x
      _ -> Nothing
  , concreteExpected = Dh.Natural
}

-- | Extract a lazy 'Text'
lazyText :: OutputType fs Text
lazyText = ConcreteOutputType {
    concreteExtract = \case
      Dh.TextLit (Dh.Chunks [] t) -> Just (TLB.toLazyText t)
      _ -> Nothing
  , concreteExpected = Dh.Text
}

-- | Extract a strict 'Data.Text.Text'
strictText :: OutputType fs Data.Text.Text
strictText = TL.toStrict <$> lazyText

-- | Extract a 'Maybe'
optional :: OutputType fs a -> OutputType fs (Maybe a)
optional aTy = ConcreteOutputType {
    concreteExtract = \case
       Dh.OptionalLit _ v ->
         traverse (concreteExtract aTy) $
           if Data.Vector.null v
             then Nothing
             else Just (Data.Vector.head v)
       _ -> Nothing
  , concreteExpected = Dh.App Dh.Optional (concreteExpected aTy)
}

-- | Extract a list
list :: OutputType fs a -> OutputType fs [a]
list = fmap Data.Vector.toList . vector

-- | Extract a 'Vector'
vector :: OutputType fs a -> OutputType fs (Data.Vector.Vector a)
vector aTy = ConcreteOutputType {
    concreteExtract = \case
      Dh.ListLit _ v -> traverse (concreteExtract aTy) v
      _ -> Nothing
  , concreteExpected = Dh.App Dh.List (concreteExpected aTy)
}

-- | Extract a 'String'
string :: OutputType fs String
string = TL.unpack <$> lazyText

-- | Extract @()@, as the empty record @{=}@
unit :: OutputType fs ()
unit = ConcreteOutputType {
    concreteExtract = \case
      Dh.RecordLit fields | HMI.null fields -> Just ()
      _ -> Nothing
  , concreteExpected = Dh.Record HMI.empty
}

-- | Extract a Tuple, as the Dhall @{_1 : a, _2 : b }@
pair :: OutputType fs a -> OutputType fs b -> OutputType fs (a, b)
pair aTy bTy = (,) <$> record "_1" aTy <.> record "_2" bTy

-- | Extract a function
function :: OpenNormalizer Src fs -> InputType fs a -> OutputType fs b -> OutputType fs (a -> b)
function nrm inTy outTy = ConcreteOutputType {
    concreteExtract = \case
      lam@(Dh.Lam _ _ _) -> Just $ \a ->
        case concreteExtract outTy $ Dh.normalizeWith nrm $ Dh.App lam (embed inTy a) of
          Just b -> b
          Nothing -> error "You cannot decode a function if it does not have the correct type"
      _ -> Nothing
  , concreteExpected = Dh.Pi "_" (declared inTy) (concreteExpected outTy)
}

-- | Specification of how to marshal from a Haskell value to a Dhall value
data InputType fs a = InputType
    { embed    :: a -> OpenExpr Src fs
    -- ^ Embeds a Haskell value as a Dalek expression
    , declared :: OpenExpr Src fs
    -- ^ Dalek type of the Haskell value
    }

instance Contravariant (InputType fs) where
  contramap f bTy = InputType {
      embed = \a -> embed bTy (f a)
    , declared = declared bTy
  }

rawIn :: OpenExpr Src fs -- ^ Dhall type of expr
      -> InputType fs (OpenExpr Src fs)
rawIn ty = InputType {
    embed = id
  , declared = ty
}

boolIn :: InputType fs Bool
boolIn = InputType {
    embed = Dh.BoolLit
  , declared = Dh.Bool
}

integerIn :: InputType fs Integer
integerIn = InputType {
    embed = Dh.IntegerLit
  , declared = Dh.Integer
}

naturalIn :: InputType fs Natural
naturalIn = InputType {
    embed = Dh.NaturalLit
  , declared = Dh.Natural
}

scientificIn :: InputType fs Scientific
scientificIn = InputType {
    embed = Dh.DoubleLit
  , declared = Dh.Double
}

doubleIn :: InputType fs Double
doubleIn = fromFloatDigits >$< scientificIn

lazyTextIn :: InputType fs Text
lazyTextIn = InputType {
    embed = Dh.TextLit . Dh.Chunks [] . TLB.fromLazyText
  , declared = Dh.Text
}

strictTextIn :: InputType fs Data.Text.Text
strictTextIn = TL.fromStrict >$< lazyTextIn

stringIn :: InputType fs String
stringIn = TL.pack >$< lazyTextIn

optionalIn :: InputType fs a -> InputType fs (Maybe a)
optionalIn aTy = InputType {
    embed = \may ->
      Dh.OptionalLit (declared aTy) $
        fmap (embed aTy) $ Data.Vector.fromList $ F.toList may
  , declared = Dh.App Dh.Optional (declared aTy)
}

vectorIn :: InputType fs a -> InputType fs (Data.Vector.Vector a)
vectorIn aTy = InputType {
    embed = \as ->
      Dh.ListLit (Just (declared aTy)) (fmap (embed aTy) as)
  , declared = Dh.App Dh.List (declared aTy)
}

unitIn :: InputType fs ()
unitIn = InputType {
    embed = const $ Dh.RecordLit HMI.empty
  , declared = Dh.Record HMI.empty
}

listIn :: InputType fs a -> InputType fs [a]
listIn aTy = Data.Vector.fromList >$< vectorIn aTy

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
