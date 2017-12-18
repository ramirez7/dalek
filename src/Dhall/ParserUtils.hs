{-# LANGUAGE OverloadedStrings #-}

module Dhall.ParserUtils where

import           Dhall.Parser            (Parser)

import           Control.Applicative     ((<|>))
import           Data.Functor            (void)
import qualified Data.Text
import           Data.Text.Buildable     (Buildable (..))
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Builder  as TLB
import qualified Text.Parser.Char        as TP
import           Text.Parser.Combinators ((<?>))
import qualified Text.Parser.Combinators as TP
import qualified Text.Parser.Token       as TP

reserved :: Data.Text.Text -> Parser ()
reserved x = do _ <- TP.text x; whitespace

blockComment :: Parser ()
blockComment = do
    _ <- TP.text "{-"
    blockCommentContinue

blockCommentChunk :: Parser ()
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

blockCommentContinue :: Parser ()
blockCommentContinue = endOfComment <|> continue
  where
    endOfComment = void (TP.text "-}")

    continue = do
        blockCommentChunk
        blockCommentContinue

lineComment :: Parser ()
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


whitespaceChunk :: Parser ()
whitespaceChunk =
    TP.choice
        [ void (TP.satisfy predicate)
        , void (TP.text "\r\n")
        , lineComment
        , blockComment
        ] <?> "whitespace"
  where
    predicate c = c == ' ' || c == '\t' || c == '\n'

whitespace :: Parser ()
whitespace = TP.skipMany whitespaceChunk

reservedEnum :: (Buildable a, Enum a, Bounded a) => Parser a
reservedEnum = TP.choice $ fmap reservedA [minBound..maxBound]

reservedOneOf :: Buildable a => [a] -> Parser a
reservedOneOf = TP.choice . fmap (TP.try . reservedA)

reservedA :: Buildable a => a -> Parser a
reservedA a = do
  reserved . TL.toStrict . TLB.toLazyText . build $ a
  return a

quasiQuotes :: Parser a -> Parser a
quasiQuotes = TP.between (TP.symbol "$(") (TP.symbol ")")
