{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Dalek <-> Haskell interop
module Dalek.Interop where

import           Data.Text    (Text)

import qualified Dhall.Core   as Dh
import           Dhall.Parser (Src)

import           Dalek.Core

data OutputType fs a = OutputType
    { extract  :: OpenExpr Src fs -> Maybe a
    -- ^ Extracts Haskell value from the Dalek expression
    , expected :: OpenExpr Src fs
    -- ^ Dalek type of the Haskell value
    }
    deriving (Functor)

expr :: OpenExpr Src fs -- Expected Dhall type
     -> OutputType fs (OpenExpr Src fs)
expr ty = OutputType Just ty

bool :: OutputType fs Bool
bool = OutputType {
    extract = \case
      Dh.BoolLit x -> Just x
      _ -> Nothing
  , expected = Dh.Bool
}

function :: InputType fs a -> OutputType fs b -> OutputType fs (a -> b)
function input output = undefined

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
