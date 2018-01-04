{-# LANGUAGE OverloadedStrings #-}

module Dhall.ParserUtils (module Dhall.ParserUtils, Tri.Result (..)) where

import           Dhall.Core              (reservedIdentifiers)
import           Dhall.Parser            (Parser (..))

import           Control.Applicative     ((<|>))
import           Control.Monad           (guard)
import           Data.Functor            (void)
import qualified Data.HashSet
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

parseDhallStr :: Parser a -> String -> Tri.Result a
parseDhallStr p s = Tri.parseString (unParser $ p) mempty s
