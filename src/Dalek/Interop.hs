{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveFunctor  #-}

-- | Dalek <-> Haskell interop
module Dalek.Interop where

import qualified Dhall.Core as Dh

import           Dalek.Core

data OpenOutputType s fs a = Type
    { extract  :: Dh.Expr s (Open s fs) -> Maybe a
    -- ^ Extracts Haskell value from the Dalek expression
    , expected :: Dh.Expr s (Open s fs)
    -- ^ Dalek type of the Haskell value
    }
    deriving (Functor)

data OpenInputType s fs a = InputType
    { embed    :: a -> Dh.Expr s (Open s fs)
    -- ^ Embeds a Haskell value as a Dalek expression
    , declared :: Dh.Expr s (Open s fs)
    -- ^ Dalek type of the Haskell value
    }

-- TODO: Inject class w/Generic inject that turns into records/unions??
