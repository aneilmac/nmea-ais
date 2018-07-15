{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS.ParseSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Attoparsec
import Data.Attoparsec.ByteString
import Data.ByteString.Lazy

import NMEA.AIS.Parse
import NMEA.AIS.PositionReportClassA
import NMEA.AIS

main :: IO ()
main = hspec spec

message1, message2 :: ByteString
message1 = "!AIVDM,1,1,,A,13HOI:0P0000VOHLCnHQKwvL05Ip,0*23"
message2 = "!AIVDM,1,1,,A,133sVfPP00PD>hRMDH@jNOvN20S8,0*7F"

spec :: Spec
spec = do
  describe "Parse a single AIS message" $ do
    it "Parses an AIS message" $ do
      message1 ~> parseAIS `shouldParse` AIS AI A (PosClassA PRCA
        { type' = 1
        , repeat' = 0
        , mmsi = 227006760
        , status = UNDER_WAY
        , turn = -128.0
        , speed = toSpeedOverGround 0
        , accuracy = False
        , lon = Just 0.1313800
        , lat = Just 49.4755767
        , course = Just 36.7
        , heading = Nothing
        , second = Second 14
        , maneuver = ManeuverIndicator 0
        , raim = False
        , radio = 2248
        })
    it "Parses another AIS message" $ do
      message2 ~> parseAIS `shouldParse` AIS AI A 
        (Raw "\SOH\ETX\CAN\US\EM\n\NUL \
             \\NUL\NUL\NUL\NUL&\US\CAN\FS\DC36\CAN!\ESC?>\FS\NUL\ENQ\EM8")
