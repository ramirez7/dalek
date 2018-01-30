{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Dhall.Extensible.Parser where

import           Control.Applicative   ((<|>), empty)
import           Data.Functor.Identity (Identity (..))
import           Data.Union
import qualified Text.Parser.Combinators as TP

import qualified Dhall.Parser          as Dh

type OpenParser as = Dh.Parser (OpenUnion as)

sendParser :: forall a as i. UElem a as i => Dh.Parser a -> OpenParser as
sendParser = fmap (ulift . Identity)

relaxParser :: forall as bs is. USubset as bs is => OpenParser as -> OpenParser bs
relaxParser = fmap urelax

parserUnion :: forall a as is. USubset as (a ': as) is => Dh.Parser a -> OpenParser as -> OpenParser (a ': as)
parserUnion pa pas = TP.try (sendParser pa) <|> relaxParser pas

voidOpenParser :: OpenParser '[]
voidOpenParser = empty
