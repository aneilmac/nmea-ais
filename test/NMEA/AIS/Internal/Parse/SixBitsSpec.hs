{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS.Internal.Parse.SixBitsSpec (main, spec) where

import Test.Hspec
import Data.ByteString (ByteString, unpack)
import Data.Word (Word8)

import NMEA.AIS.Internal.Parse.SixBits
import NMEA.AIS.Internal.Parse.BitsTraverse (toBits, BitStream)

main :: IO ()
main = hspec spec

in' :: ByteString
in' = "0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVW`abcdefghijklmnopqrstuvw"

decodeAsciiAlt :: ByteString -> BitStream
decodeAsciiAlt = toBits . unpack . decodeAscii

spec :: Spec
spec = do
  describe "Parse Sixbit string" $ do
    it "Parses a sixbit character into the equivalent sixbit" $ do
      mapM_ (\(a, b) -> d6 a `shouldBe` b) $ zip (unpack in') [0..63]
    it "Parses empty sixbit string" $ do
      decodeAscii "" `shouldBe` "" 
    it "Parses 1-val sixbit string" $ do
      -- Transforms: 100110 => 00100110 => 10011000
      decodeAscii "V" `shouldBe` "\152"
    it "Parses arbitrary sixbit string" $ do
      -- Transforms: 001001 001010 => 00001001 00001010 => 00100100 10100000
      decodeAscii "9:" `shouldBe` "$\160"
  describe "Check Sixbit string equivalence" $ do
    it "Parses empty sixbit string" $ do
      decodeAscii' "" `shouldBe` decodeAsciiAlt "" 
    it "Parses 1-val sixbit string" $ do
      decodeAscii' "V" `shouldBe` take 6 (decodeAsciiAlt "V")
    it "Parses arbitrary sixbit string" $ do
       decodeAscii' "9:" `shouldBe` take 12 (decodeAsciiAlt "9:")
