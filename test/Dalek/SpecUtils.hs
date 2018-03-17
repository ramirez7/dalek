{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}

module Dalek.SpecUtils (module X, module Dalek.SpecUtils) where

import           Test.Hspec              as X hiding (expectationFailure)
import qualified Test.Hspec

import Data.Functor (void)

import qualified Dhall.Context           as Dh
import qualified Dhall.Core              as Dh
import qualified Dhall.Parser            as Dh
import qualified Dhall.TypeCheck         as Dh

import           Dalek.Parser            (Result (..), parseDhallStr)

import           Data.Text.Buildable     (Buildable (..))
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy as TL

import           Data.String.Interpolate as X (i)

rightWith :: (Show a, Show e, Eq a) => Either e a -> a -> IO ()
rightWith eea expected = case eea of
  Right a     -> a `shouldBe` expected
  (Left left) -> expectationFailure $ "Not Right: Left " ++ show left

shouldBeLeft :: (Show a) => Either e a -> IO ()
shouldBeLeft eea = case eea of
  Left _      -> pure ()
  Right right -> expectationFailure $ "Not Left: Right " ++ show right

shouldBeRight :: (Show e) => Either e a -> IO ()
shouldBeRight eea = case eea of
  Right _   -> pure ()
  Left left -> expectationFailure $ "Not Right: Left " ++ show left

asRight :: Show e => Either e a -> (a -> IO b) -> IO b
asRight eea f = case eea of
  Right a   -> f a
  Left left -> expectationFailure $ "Not Right: Left " ++ show left

expectationFailure :: String -> IO a
expectationFailure e = Test.Hspec.expectationFailure e >> error "unreachable"

shouldBeIO :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation
shouldBeIO ioa expected = do
  a <- ioa
  a `shouldBe` expected

parsed :: Dh.Parser a -> String -> IO (Dh.Expr Dh.Src a)
parsed p s = case parseDhallStr (Dh.exprA p) s of
  Success a   -> pure a
  Failure err -> expectationFailure $ "Bad Parse: " ++ show err

parsed' :: Dh.Parser a -> String -> IO a
parsed' p s = case parseDhallStr p s of
  Success a   -> pure a
  Failure err -> expectationFailure $ "Bad Parse: " ++ show err

checked :: (Buildable a, Eq a) => Dh.Parser a -> Dh.Typer a -> Dh.Normalizer a -> String -> IO (Dh.Expr Dh.Src a)
checked p t n s = do
  expr <- parsed p s
  case Dh.typeWithAN n t Dh.empty expr of
    Right x -> pure x
    Left err -> expectationFailure $ "Erroneously did not check: " ++ show err ++ " ..for program: \n" ++ buildStr s

checkedAndNormalized :: (Buildable a, Eq a) => Dh.Parser a -> Dh.Typer a -> Dh.Normalizer a -> String -> IO (Dh.Expr Dh.Src a)
checkedAndNormalized p t n s = do
  expr <- parsed p s
  case Dh.typeWithAN n t Dh.empty expr of
    Right _ -> pure ()
    Left err -> expectationFailure $ "Erroneously did not check: " ++ show err ++ " ..for program: \n" ++ buildStr s
  pure $ Dh.normalizeWith n expr

shouldCheck :: (Buildable a, Eq a) => Dh.Parser a -> Dh.Typer a -> Dh.Normalizer a -> String -> IO ()
shouldCheck p t n s = void (checked p t n s)

shouldNotCheck :: (Buildable a, Show a, Eq a) => Dh.Parser a -> Dh.Typer a -> Dh.Normalizer a -> String -> IO ()
shouldNotCheck p t n s = do
  expr <- parsed p s
  case Dh.typeWithAN n t Dh.empty expr of
    Left _ -> pure ()
    Right x -> expectationFailure $ "Erroneously did check with type: " ++ buildStr x ++ "..for program: \n" ++ buildStr s

shouldBeSuccess :: Show a => Result a -> IO ()
shouldBeSuccess = \case
  Success _ -> pure ()
  x@(Failure _) -> expectationFailure $ "Not Result.Success: " ++ show x

shouldBeFailure :: Show a => Result a -> IO ()
shouldBeFailure = \case
  Failure _ -> pure ()
  x@(Success _) -> expectationFailure $ "Not Result.Failure: " ++ show x

buildStr :: Buildable a => a -> String
buildStr = TL.unpack . TLB.toLazyText . build
