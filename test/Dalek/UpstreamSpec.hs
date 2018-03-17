{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeApplications      #-}

module Dalek.UpstreamSpec (spec, repl) where

import           Dalek.SpecUtils

import qualified Data.Foldable              as F
import qualified Data.HashMap.Strict.InsOrd as HMI
import           Data.Text.Buildable        (Buildable (..))
import qualified Data.Text.Lazy.Builder     as TLB
import           Data.Traversable           (for)

import qualified Dhall.Core                 as Dh
import qualified Dhall.TypeCheck            as Dh

import           Dalek.Core
import           Dalek.Parser
import           Dalek.Patterns
import qualified Dalek.Repl
import           Dalek.TypeCheck

-- | For debugging
repl :: IO ()
repl = Dalek.Repl.repl upstreamParser upstreamNormalizer upstreamTyper

spec :: Spec
spec = do
  describe "TypeLits" $ do
    it "should properly perform type equality on Natural lits" $ do
      shouldCheck upstreamParser upstreamTyper upstreamNormalizer [i|
          Natural/Phantom/mk +2 : Natural/Phantom +2
      |]
      shouldNotCheck upstreamParser upstreamTyper upstreamNormalizer [i|
          Natural/Phantom/mk +2 : Natural/Phantom +3
      |]
    it "should properly perform type equality on List lits" $ do
      shouldCheck upstreamParser upstreamTyper upstreamNormalizer [i|
          Natural/List/Phantom/mk [+2, +3] : Natural/List/Phantom [+2, +3]
      |]
      shouldNotCheck upstreamParser upstreamTyper upstreamNormalizer [i|
          Natural/Phantom/mk [+2, +3] : Natural/Phantom [+2, +3, +4]
      |]
  describe "Type Families" $ do
    it "should normalize embedded Type Families during Type Checking" $ do
      shouldCheck upstreamParser upstreamTyper upstreamNormalizer [i|
          <a = {=} | b : {}> : Enum/Rep ["a", "b"]
      |]
  describe "Closed Kinds" $ do
    it "should support extension-provided closed kinds" $ do
      shouldCheck upstreamParser upstreamTyper upstreamNormalizer [i|
          +55 : Closed/Rep Closed/Natural
      |]
      shouldCheck upstreamParser upstreamTyper upstreamNormalizer [i|
          False : Closed/Rep Closed/Bool
      |]
      shouldNotCheck upstreamParser upstreamTyper upstreamNormalizer [i|
          +55 : Closed/Rep Closed/Bool
      |]
      shouldNotCheck upstreamParser upstreamTyper upstreamNormalizer [i|
          False : Closed/Rep Closed/Natural
      |]
      summonedNat <- checkedAndNormalized upstreamParser upstreamTyper upstreamNormalizer [i|
          Closed/Summon Closed/Natural : Natural
      |]
      summonedNat `shouldBe` Dh.NaturalLit 7
      summonedBool <- checkedAndNormalized upstreamParser upstreamTyper upstreamNormalizer [i|
          Closed/Summon Closed/Bool : Bool
      |]
      summonedBool `shouldBe` Dh.BoolLit True
    it "should allow extension-supported closed kinds to be provided as Record field types" $ do
      shouldCheck upstreamParser upstreamTyper upstreamNormalizer [i|
          { b : Closed/Bool, n : Closed/Natural} : Type
      |]
      shouldCheck upstreamParser upstreamTyper upstreamNormalizer [i|
          { b = False, n = +44 } : Closed/Record/Rep { b : Closed/Bool, n : Closed/Natural }
      |]
      summonedRecord <- checkedAndNormalized upstreamParser upstreamTyper upstreamNormalizer [i|
          Closed/Record/Summon { b : Closed/Bool, n : Closed/Natural } : { b : Bool, n : Natural }
      |]
      summonedRecord `shouldBe` Dh.RecordLit (HMI.fromList [("b", Dh.BoolLit True), ("n", Dh.NaturalLit 7)])

data Upstream expr =
    NaturalPhantom
  | NaturalPhantomMk
  | NaturalListPhantom
  | NaturalListPhantomMk
  | EnumRep
  | ClosedKind
  | ClosedNatural
  | ClosedBool
  | ClosedRep
  | ClosedSummon
  | ClosedRecordRep
  | ClosedRecordSummon
  deriving (Eq, Show, Ord, Enum, Bounded)

upstreamNormalizer :: OpenNormalizer '[Upstream]
upstreamNormalizer = ignoringUnclosed $ \case
  Apps [E EnumRep, Dh.ListLit _ v] -> do
    ctors <- for (F.toList v) $ \case
        Dh.TextLit (Dh.Chunks [] builder) -> Just (TLB.toLazyText builder, Dh.Record HMI.empty)
        _ -> Nothing
    pure $ Dh.Union (HMI.fromList ctors)
  Apps [E ClosedRep, E ClosedBool] -> Just Dh.Bool
  Apps [E ClosedRep, E ClosedNatural] -> Just Dh.Natural
  Apps [E ClosedSummon, E ClosedBool] -> Just $ Dh.BoolLit True
  Apps [E ClosedSummon, E ClosedNatural] -> Just $ Dh.NaturalLit 7
  Apps [E ClosedRecordRep, Dh.Record mp] -> fmap Dh.Record $ for mp $ \case
    E ClosedBool -> Just Dh.Bool
    E ClosedNatural -> Just Dh.Natural
    _ -> Nothing
  Apps [E ClosedRecordSummon, Dh.Record mp] -> fmap Dh.RecordLit $ for mp $ \case
    E ClosedBool -> Just $ Dh.BoolLit True
    E ClosedNatural -> Just $ Dh.NaturalLit 7
    _ -> Nothing
  _ -> Nothing

upstreamTyper :: Dh.Typer (Open '[Upstream])
upstreamTyper = toTyper $ sendTyper $ \case
  -- Natural type lits
  NaturalPhantom ->
    ("_" :. Dh.Natural) ~> (Dh.Const Dh.Type)
  NaturalPhantomMk -> ("n" :. Dh.Natural) ~> (Dh.App (sendEmbed NaturalPhantom) "n")
  -- List type lits
  NaturalListPhantom ->
    ("_" :. (Dh.App Dh.List Dh.Natural)) ~> (Dh.Const Dh.Type)
  NaturalListPhantomMk ->
    ("ns" :. (Dh.App Dh.List Dh.Natural)) ~> (Dh.App (sendEmbed NaturalListPhantom) "ns")
  -- Enum generation
  EnumRep ->
    ("_" :. (Dh.App Dh.List Dh.Text)) ~> (Dh.Const Dh.Type)
  -- Poly-kinded record fields
  ClosedKind -> Dh.Const Dh.Kind
  ClosedNatural -> sendEmbed ClosedKind
  ClosedBool -> sendEmbed ClosedKind
  ClosedRep -> ("_" :. (sendEmbed ClosedKind)) ~> (Dh.Const Dh.Type)
  ClosedSummon -> ("ck" :. (sendEmbed ClosedKind)) ~> (Dh.App (sendEmbed ClosedRep) "ck")
  ClosedRecordRep -> Dh.Const Dh.Type .~> Dh.Const Dh.Type
  ClosedRecordSummon -> ("record" :. Dh.Const Dh.Type) ~> Dh.App (sendEmbed ClosedRecordRep) "record"

upstreamParser :: OpenParser '[Upstream]
upstreamParser = sendParser $ reservedEnumF @Upstream

instance Buildable (Upstream expr) where
  build = \case
    NaturalPhantom -> "Natural/Phantom"
    NaturalPhantomMk -> "Natural/Phantom/mk"
    NaturalListPhantom -> "Natural/List/Phantom"
    NaturalListPhantomMk -> "Natural/List/Phantom/mk"
    EnumRep -> "Enum/Rep"
    ClosedKind -> "Closed/Kind"
    ClosedNatural -> "Closed/Natural"
    ClosedBool -> "Closed/Bool"
    ClosedRep -> "Closed/Rep"
    ClosedSummon -> "Closed/Summon"
    ClosedRecordRep -> "Closed/Record/Rep"
    ClosedRecordSummon -> "Closed/Record/Summon"
