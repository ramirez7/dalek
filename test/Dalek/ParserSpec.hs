{-# LANGUAGE OverloadedStrings #-}

module Dalek.ParserSpec where

import           Dalek.SpecUtils

import           Dalek.Parser

spec :: Spec
spec = describe "reserved" $ do
  it "should handle whitespace correctly" $ do
    let p = reserved "x"
    -- The Dhall parsers expected a newline at the end of file
    shouldBeSuccess $ parseDhallStr p "x"
    shouldBeSuccess $ parseDhallStr p "x\ny"
    shouldBeSuccess $ parseDhallStr p "x\ty"
    shouldBeSuccess $ parseDhallStr p "x y"
    shouldBeSuccess $ parseDhallStr p "x -- comment"

    shouldBeFailure $ parseDhallStr p "x/y"
    shouldBeFailure $ parseDhallStr p "xy"
