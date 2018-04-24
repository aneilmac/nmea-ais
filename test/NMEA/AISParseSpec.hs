{-# LANGUAGE OverloadedStrings #-}
module NMEA.AISParseSpec(main, spec) where

import Test.Hspec
import Test.Hspec.Attoparsec
import Data.Attoparsec.Text
import Data.Text.Lazy

import NMEA.AIS.Parse
import NMEA.AIS

main :: IO ()
main = hspec spec

message :: Text
message = "!AIVDM,1,1,,A,13HOI:0P0000VOHLCnHQKwvL05Ip,0*23"

spec :: Spec
spec = do
  describe "Parse a single AIS message" $ do
    it "Parses an AIS message" $ do
      message ~> parseAIS `shouldParse` AIS AI A AISContent
