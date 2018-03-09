{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-
TODO:
- input tests
  - Confirm it normalizes
-}
module Dalek.InteropSpec where

import           Dalek.SpecUtils

import           Data.Functor.Alt    ((<!>))
import           Data.Functor.Apply  ((<.>))
import           Data.Text.Buildable
import           Data.Text.Lazy      (Text)
import qualified Data.Vector         as V

import qualified Dhall.Core          as Dh
import           Dhall.Parser        (Src)
import qualified Dhall.TypeCheck     as Dh

import           Dalek.Core
import           Dalek.Interop
import           Dalek.Parser
import           Dalek.Patterns
import           Dalek.TypeCheck

type SpecExts = '[DhExistentialList]
specInput :: OutputType SpecExts a -> Text -> IO a
specInput = input specNormalizer specParser specTyper

specNormalizer :: OpenNormalizer s SpecExts
specNormalizer = exListNormalizer

specParser :: OpenParser s SpecExts
specParser = exListParser

specTyper :: Dh.Typer s (Open s SpecExts)
specTyper = toTyper $ sendTyper exListTyper

spec :: Spec
spec = describe "OutputType" $ do
  it "bool" $ do
    t <- specInput bool "True"
    t `shouldBe` True

    f <- specInput bool "False"
    f `shouldBe` False
  it "integer" $ do
    x <- specInput integer "-2"
    x `shouldBe` -2
  it "scientific" $ do
    x <- specInput scientific "2.2"
    x `shouldBe` 2.2
  it "double" $ do
    x <- specInput double "2.2"
    x `shouldBe` 2.2
  it "natural" $ do
    x <- specInput natural "+5"
    x `shouldBe` 5
  it "lazyText" $ do
    t <- specInput lazyText "\"hello world\""
    t `shouldBe` "hello world"
  it "strictText" $ do
    t <- specInput strictText "\"hello world\""
    t `shouldBe` "hello world"
  it "optional" $ do
    just <- specInput (optional natural) "[+2] : Optional Natural"
    just `shouldBe` Just 2
    nothing <- specInput (optional natural) "[] : Optional Natural"
    nothing `shouldBe` Nothing
  it "list" $ do
    xs <- specInput (list natural) "[+2, +3, +4]"
    xs `shouldBe` [2,3,4]
    nil <- specInput (list natural) "[] : List Natural"
    nil `shouldBe` []
  it "vector" $ do
    xs <- specInput (vector natural) "[+2, +3, +4]"
    xs `shouldBe` V.fromList [2,3,4]
    nil <- specInput (vector natural) "[] : List Natural"
    nil `shouldBe` V.fromList []
  it "string" $ do
    s <- specInput string "\"hello world\""
    s `shouldBe` "hello world"
  it "unit" $ do
    u <- specInput unit "{=}"
    u `shouldBe` ()
  it "pair" $ do
    p <- specInput (pair bool double) "{ _1 = True, _2 = 2.2 }"
    p `shouldBe` (True, 2.2)
  it "record" $ do
    let tupleTy = (,,) <$> record "_1" bool <.> record "_2" double <.> record "_3" integer
    tuple <- specInput tupleTy "{ _1 = True, _2 = 2.2, _3 = 4 }"
    tuple `shouldBe` (True, 2.2, 4)
  it "union" $ do
    let eitherTy = union "Left" Left bool <!> union "Right" Right double
    left <- specInput eitherTy "< Left = False | Right : Double >"
    left `shouldBe` Left False
    right <- specInput eitherTy "< Right = 2.2 | Left : Bool >"
    right `shouldBe` Right 2.2
  it "function" $ do
    let funTy = function specNormalizer naturalIn string
    f <- specInput funTy "\\(n : Natural) -> Natural/show n"
    f 2 `shouldBe` "+2"
  it "quantified" $ do
    let elTy = forAll (Dh.Const Dh.Type) $ \ty ->
          ConcreteOutputType {
              concreteExpected = Dh.App (sendEmbed DhExistentialListType) ty
            , concreteExtract = \case
                Apps [E DhExistentialList, Dh.ListLit _ xs] ->
                  Just $ ExistentialList ty (V.toList xs)
                _ -> Nothing
          }
    el <- specInput elTy "Existential/List Natural [+1]"
    el `shouldBe` ExistentialList Dh.Natural [Dh.NaturalLit 1]

data ExistentialList fs = ExistentialList {
    exListTy  :: OpenExpr Src fs
  , exListVal :: [OpenExpr Src fs]
}

deriving instance OpenSatisfies Show Src fs => Show (ExistentialList fs)
deriving instance OpenSatisfies Eq Src fs => Eq (ExistentialList fs)

elTy' :: Member DhExistentialList fs => OutputType fs (ExistentialList fs)
elTy' = forAll (Dh.Const Dh.Type) $ \ty ->
      ConcreteOutputType {
          concreteExpected = Dh.App (sendEmbed DhExistentialListType) ty
        , concreteExtract = \case
            Apps [E DhExistentialList, Dh.ListLit _ xs] ->
              Just $ ExistentialList ty (V.toList xs)
            _ -> Nothing
      }

data DhExistentialList expr =
    DhExistentialList
  | DhExistentialListType
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Buildable (DhExistentialList expr) where
  build = \case
    DhExistentialList -> "Existential/List"
    DhExistentialListType -> "Existential/List/Type"

exListNormalizer :: Member DhExistentialList fs => OpenNormalizer s fs
exListNormalizer = const Nothing

exListParser :: Member DhExistentialList fs => OpenParser s fs
exListParser = sendParser $ reservedEnumF @DhExistentialList

exListTyper :: Member DhExistentialList fs => OpenTyper s DhExistentialList fs
exListTyper = \case
  DhExistentialList ->
        ("ty" :. Dh.Const Dh.Type)
     ~> (Dh.App Dh.List "ty")
    .~> (Dh.App (sendEmbed DhExistentialListType) "ty")
  DhExistentialListType -> Dh.Const Dh.Type .~> Dh.Const Dh.Type

-- TODO: InputType tests (maybe round trip via function?)
