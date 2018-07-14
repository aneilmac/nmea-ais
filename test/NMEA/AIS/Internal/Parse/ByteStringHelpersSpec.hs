{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS.Internal.Parse.ByteStringHelpersSpec (main, spec) where

import Test.Hspec
import Data.ByteString.Lazy (ByteString, unpack)
import Data.Word (Word8)

import NMEA.AIS.Internal.Parse.ByteStringHelpers

main :: IO ()
main = hspec spec

-- | Map of False values. Where indices match digit characters in ASCII table
--   then these values are marked True.
digitTruths :: [Bool]
digitTruths = [x | y <- [0..127]
                 , let x = if y < 48 || y > 57 then False else True ]

spec :: Spec
spec = do
  describe "Bytestring Helpers" $ do
    it "Decimal characters are recognized" $ do
      mapM_ (\(a, b) -> isDecimal a `shouldBe` b) $ zip [0..127] digitTruths

