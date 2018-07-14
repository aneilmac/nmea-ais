{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS.ParseSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Attoparsec
import Data.Attoparsec.ByteString
import Data.ByteString.Lazy

import NMEA.AIS.Parse
import NMEA.AIS

main :: IO ()
main = hspec spec

message :: ByteString
message = "!AIVDM,1,1,,A,13HOI:0P0000VOHLCnHQKwvL05Ip,0*23"

spec :: Spec
spec = do
  describe "Parse a single AIS message" $ do
    it "Parses an AIS message" $ do
      message ~> parseAIS `shouldParse` AIS AI A 
        (Raw "\SOH\ETX\CAN\US\EM\n\NUL \
             \\NUL\NUL\NUL\NUL&\US\CAN\FS\DC36\CAN!\ESC?>\FS\NUL\ENQ\EM8")
