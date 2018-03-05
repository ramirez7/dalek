{-# LANGUAGE OverloadedStrings #-}

module Dalek.ParserSpec where

import           Dalek.SpecUtils

import           Dalek.Parser

spec :: Spec
spec = describe "reserved" $ do
  it "should handle whitespace correctly" $ do
    shouldBeSuccess $ parseDhallStr (reserved "x") "x x"
    shouldBeFailure $ parseDhallStr (reserved "x") "x/x"
