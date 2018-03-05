{-# LANGUAGE OverloadedStrings #-}

module Dalek.ParserSpec where

import           Dalek.SpecUtils

import           Dalek.Parser

spec :: Spec
spec = describe "reserved" $ do
  it "should handle whitespace correctly" $ do
    let p = reserved "x"
    -- The Dhall parsers expected a newline at the end of file
    let withEofNewline s = s ++ "\n"
    shouldBeSuccess $ parseDhallStr p $ withEofNewline "x"
    shouldBeSuccess $ parseDhallStr p $ withEofNewline "x\ny"
    shouldBeSuccess $ parseDhallStr p $ withEofNewline "x\ty"
    shouldBeSuccess $ parseDhallStr p $ withEofNewline "x y"
    shouldBeSuccess $ parseDhallStr p $ withEofNewline "x -- comment"

    shouldBeFailure $ parseDhallStr p $ withEofNewline "x/y"
    shouldBeFailure $ parseDhallStr p $ withEofNewline "xy"
