{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS.PosistionReportClassASpec (main, spec) where

import Test.Hspec

import NMEA.AIS.PositionReportClassA

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Speed Over Group Checks" $ do
    it "Should be invalid" $ do
      toSpeedOverGround 1023 `shouldBe` Nothing
      toSpeedOverGround (-1) `shouldBe` Nothing
    it "Should show correct strings" $ do
      show (toSpeedOverGround 1022) `shouldBe` "Just >=102.2 knots"
      show (toSpeedOverGround 1021) `shouldBe` "Just ~102.1 knots"
      show (toSpeedOverGround 0)    `shouldBe` "Just ~0.0 knots"
  describe "Lon/Lat checks" $ do
    it "Should latitude translate correctly" $ do
      toLat 38049766 `shouldBe` Just 63.416276666666668
    it "Should longitude translate correctly" $ do
      toLon 6247336 `shouldBe` Just 10.412226666666667
