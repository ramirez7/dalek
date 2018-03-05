{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Dalek.Parser
  ( OpenParser
  , sendParser
  -- * Utilities
  , openParseStr
  , xParser
  -- * Useful parsing utilities
  , reserved
  , reservedA
  , reservedWith
  , reservedF
  , reservedEnum
  , reservedEnumF
  , reservedOneOf
  , quasiQuotes
  , parseDhallStr
  , label
  , whitespace
  -- * Re-exports
  , (<|>)
  , Tri.Result (..)
  , Tri.ErrInfo (..)
  ) where

import           Control.Applicative     (empty, (<|>))

import qualified Dhall.Parser            as Dh

import           Dalek.Core

import           Dhall.Core              (reservedIdentifiers)
import           Dhall.Parser            (Parser (..))

import           Control.Monad           (guard)
import           Data.Functor            (void)
import qualified Data.HashSet
import           Data.List               (sortBy)
import           Data.Ord                (Down (..), comparing)
import qualified Data.Text
import           Data.Text.Buildable     (Buildable (..))
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Builder  as TLB
import qualified Text.Parser.Char        as TP
import           Text.Parser.Combinators ((<?>))
import qualified Text.Parser.Combinators as TP
import qualified Text.Parser.Token       as TP
import qualified Text.Trifecta.Parser    as Tri
import qualified Text.Trifecta.Result    as Tri

type OpenParser s fs = Dh.Parser (Open s fs)

-- | Lift a 'Dh.Parser' for a single member of an 'Open' to an 'OpenParser'
sendParser :: forall fs f s. Member f fs => Dh.Parser (f (OpenExpr s fs)) -> OpenParser s fs
sendParser = fmap (Rec . inj)

-- | Run an 'OpenParser' on a 'String'
openParseStr :: forall fs. OpenParser Dh.Src fs -> String -> Tri.Result (OpenExpr Dh.Src fs)
openParseStr p s = Tri.parseString (Dh.unParser $ Dh.exprA p) mempty s

-- | Parse lifted 'X'
xParser :: Member (C X) fs => OpenParser s fs
xParser = empty

-------------------------------------------------------------------------------
-- Parsing helpers


reserved :: Data.Text.Text -> Dh.Parser ()
reserved x = do _ <- TP.text x; whitespace

blockComment :: Dh.Parser ()
blockComment = do
    _ <- TP.text "{-"
    blockCommentContinue

blockCommentChunk :: Dh.Parser ()
blockCommentChunk =
    TP.choice
        [ blockComment  -- Nested block comment
        , character
        , endOfLine
        ]
  where
    character = void (TP.satisfy predicate)
      where
        predicate c = '\x20' <= c && c <= '\x10FFFF' || c == '\n' || c == '\t'

    endOfLine = void (TP.text "\r\n")

blockCommentContinue :: Dh.Parser ()
blockCommentContinue = endOfComment <|> continue
  where
    endOfComment = void (TP.text "-}")

    continue = do
        blockCommentChunk
        blockCommentContinue

lineComment :: Dh.Parser ()
lineComment = do
    _ <- TP.text "--"
    TP.skipMany notEndOfLine
    endOfLine
    return ()
  where
    endOfLine =
            void (TP.char '\n'  )
        <|> void (TP.text "\r\n")

    notEndOfLine = void (TP.satisfy predicate)
      where
        predicate c = ('\x20' <= c && c <= '\x10FFFF') || c == '\t'


whitespaceChunk :: Dh.Parser ()
whitespaceChunk =
    TP.choice
        [ void (TP.satisfy predicate)
        , void (TP.text "\r\n")
        , lineComment
        , blockComment
        ] <?> "whitespace"
  where
    predicate c = c == ' ' || c == '\t' || c == '\n'

whitespace :: Dh.Parser ()
whitespace = TP.skipSome whitespaceChunk

-- TODO: Why do these parsers seem to treat slashes as whitespace?? Or something

-- | 'reservedOneOf' for every possible value of @a@
reservedEnum :: (Buildable a, Enum a, Bounded a) => Dh.Parser a
reservedEnum = reservedOneOf [minBound..maxBound]

-- | 'reservedEnum' with a different type signature to enable guiding inference
-- with TypeApplications
reservedEnumF :: forall f a. (Buildable (f a), Enum (f a), Bounded (f a)) => Dh.Parser (f a)
reservedEnumF = reservedEnum

-- | Tried in reverse alphabetical 'build' order (helps with collisions)
reservedOneOf :: Buildable a => [a] -> Dh.Parser a
reservedOneOf = TP.choice . fmap (TP.try . reservedA) . sortBy (comparing (Down . build))

reservedWith :: Data.Text.Text -> a -> Dh.Parser a
reservedWith name a = do
  reserved name
  return a

reservedA :: Buildable a => a -> Dh.Parser a
reservedA a = do
  reserved . TL.toStrict . TLB.toLazyText . build $ a
  return a

reservedF :: forall f a. Buildable (f a) => f a -> Parser (f a)
reservedF = reservedA

quasiQuotes :: Parser a -> Parser a
quasiQuotes = TP.between (TP.symbol "$(") (TP.symbol ")")

---


simpleLabel :: Parser TL.Text
simpleLabel = TP.try (do
    text <- quotedLabel
    guard (not (Data.HashSet.member text reservedIdentifiers))
    return text )

quotedLabel :: Parser TL.Text
quotedLabel = TP.try (do
    c  <- TP.satisfy headCharacter
    cs <- TP.many (TP.satisfy tailCharacter)
    let string = c:cs
    return (TL.pack string) )
  where
    headCharacter c = alpha c || c == '_'

    tailCharacter c = alpha c || digit c || c == '_' || c == '-' || c == '/'

backtickLabel :: Parser TL.Text
backtickLabel = do
    _ <- TP.char '`'
    t <- quotedLabel
    _ <- TP.char '`'
    return t

label :: Parser TL.Text
label = (do
    t <- backtickLabel <|> simpleLabel
    whitespace
    return t ) <?> "label"

alpha :: Char -> Bool
alpha c = ('\x41' <= c && c <= '\x5A') || ('\x61' <= c && c <= '\x7A')

digit :: Char -> Bool
digit c = '\x30' <= c && c <= '\x39'

-- | Adds a newline to the end of the String
parseDhallStr :: Parser a -> String -> Tri.Result a
parseDhallStr p s = Tri.parseString (unParser $ p) mempty (s ++ "\n")
