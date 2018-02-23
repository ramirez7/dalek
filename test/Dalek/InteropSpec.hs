{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{-
TODO:
- input tests
  - Confirm it normalizes
-}
module Dalek.InteropSpec where

import           Dalek.SpecUtils

import           Data.Functor.Alt   ((<!>))
import           Data.Functor.Apply ((<.>))
import           Data.Text.Lazy     (Text)
import qualified Data.Vector as V

import           Dalek.Core
import           Dalek.Interop
import           Dalek.Parser
import           Dalek.TypeCheck

xInput :: OutputType '[C X] a -> Text -> IO a
xInput = input xNormalizer xParser (toTyper $ sendTyper xTyper)

spec :: Spec
spec = describe "OutputType" $ do
  it "bool" $ do
    t <- xInput bool "True"
    t `shouldBe` True

    f <- xInput bool "False"
    f `shouldBe` False
  it "integer" $ do
    x <- xInput integer "-2"
    x `shouldBe` -2
  it "double" $ do
    x <- xInput double "2.2"
    x `shouldBe` 2.2
  it "natural" $ do
    x <- xInput natural "+5"
    x `shouldBe` 5
  it "lazyText" $ do
    t <- xInput lazyText "\"hello world\""
    t `shouldBe` "hello world"
  it "strictText" $ do
    t <- xInput strictText "\"hello world\""
    t `shouldBe` "hello world"
  it "optional" $ do
    just <- xInput (optional natural) "[+2] : Optional Natural"
    just `shouldBe` Just 2
    nothing <- xInput (optional natural) "[] : Optional Natural"
    nothing `shouldBe` Nothing
  it "list" $ do
    xs <- xInput (list natural) "[+2, +3, +4]"
    xs `shouldBe` [2,3,4]
    nil <- xInput (list natural) "[] : List Natural"
    nil `shouldBe` []
  it "vector" $ do
    xs <- xInput (vector natural) "[+2, +3, +4]"
    xs `shouldBe` V.fromList [2,3,4]
    nil <- xInput (vector natural) "[] : List Natural"
    nil `shouldBe` V.fromList []
  it "string" $ do
    s <- xInput string "\"hello world\""
    s `shouldBe` "hello world"
  it "unit" $ do
    u <- xInput unit "{=}"
    u `shouldBe` ()
  it "pair" $ do
    p <- xInput (pair bool double) "{ _1 = True, _2 = 2.2 }"
    p `shouldBe` (True, 2.2)
  it "record" $ do
    let tupleTy = (,,) <$> record "_1" bool <.> record "_2" double <.> record "_3" integer
    tuple <- xInput tupleTy "{ _1 = True, _2 = 2.2, _3 = 4 }"
    tuple `shouldBe` (True, 2.2, 4)
  it "union" $ do
    let eitherTy = union "Left" Left bool <!> union "Right" Right double
    left <- xInput eitherTy "< Left = False | Right : Double >"
    left `shouldBe` Left False
    right <- xInput eitherTy "< Right = 2.2 | Left : Bool >"
    right `shouldBe` Right 2.2
