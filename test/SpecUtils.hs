{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module SpecUtils (module X, module SpecUtils) where

import           Test.Hspec           as X hiding (expectationFailure)

import qualified Dhall.Context        as Dh
import qualified Dhall.Core           as Dh
import qualified Dhall.Parser         as Dh
import qualified Dhall.TypeCheck      as Dh

import           Dhall.ParserUtils    (Result(..), parseDhallStr)

import           Data.Text.Buildable  (Buildable)

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
expectationFailure = error

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

checked :: (Buildable a, Eq a) => Dh.Parser a -> Dh.Typer a -> String -> IO (Dh.Expr Dh.Src a)
checked p t s = do
  expr <- parsed p s
  Dh.typeWithA t Dh.empty expr `asRight` (pure . const expr)
