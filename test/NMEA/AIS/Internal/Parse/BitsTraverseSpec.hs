{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS.Internal.Parse.BitsTraverseSpec (main, spec) where

import Test.Hspec
import Data.ByteString.Lazy (ByteString, unpack)
import Data.Word (Word8)

import NMEA.AIS.Internal.Parse.BitsTraverse

import Control.Monad

main :: IO ()
main = hspec spec

toUnsignedLEI :: BitStream -> Int
toUnsignedLEI = toUnsignedLE

toSignedLEI :: BitStream -> Int
toSignedLEI = toSignedLE

toUnsignedBEI :: BitStream -> Int
toUnsignedBEI = toUnsignedBE

toSignedBEI :: BitStream -> Int
toSignedBEI = toSignedBE

spec :: Spec
spec = do
  describe "BitsTraverse functions" $ do
    it "Tranlates to unsigned little endian" $ do
      -- Unsigned Little Endian
      toUnsignedLEI [] `shouldBe` 0
      toUnsignedLEI [False] `shouldBe` 0
      toUnsignedLEI [False, False] `shouldBe` 0
      toUnsignedLEI [True] `shouldBe` 1
      toUnsignedLEI [True, False] `shouldBe` 1
      toUnsignedLEI [False, True] `shouldBe` 2
      toUnsignedLEI [True, True] `shouldBe` 3
      toUnsignedLEI [True, True, True, True] `shouldBe` 15
      toUnsignedLEI [True, False, True, True] `shouldBe` 13
    it "Translates to unsigned big endian" $ do
      toUnsignedBEI [] `shouldBe` 0
      toUnsignedBEI [False] `shouldBe` 0
      toUnsignedBEI [False, False] `shouldBe` 0
      toUnsignedBEI [True] `shouldBe` 1
      toUnsignedBEI [False, True] `shouldBe` 1
      toUnsignedBEI [True, False] `shouldBe` 2
      toUnsignedBEI [True, True] `shouldBe` 3
      toUnsignedBEI [True, True, True, True] `shouldBe` 15
      toUnsignedBEI [True, False, True, True] `shouldBe` 11
    it "Translates to signed little endian" $ do
      toSignedLEI [] `shouldBe` 0
      toSignedLEI [False] `shouldBe` 0
      toSignedLEI [False, False] `shouldBe` 0
      toSignedLEI [True] `shouldBe` negate 1
      toSignedLEI [True, False] `shouldBe` 1
      toSignedLEI [False, True] `shouldBe` negate 2
      toSignedLEI [True, True] `shouldBe` negate 1
      toSignedLEI [True, False, False, True] `shouldBe` negate 7
      toSignedLEI [False, True, False, True] `shouldBe` negate 6
    it "Translates to signed big endian" $ do
      toSignedBEI [] `shouldBe` 0
      toSignedBEI [False] `shouldBe` 0
      toSignedBEI [False, False] `shouldBe` 0
      toSignedBEI [True] `shouldBe` negate 1
      toSignedBEI [True, False] `shouldBe` negate 2
      toSignedBEI [False, True] `shouldBe` 1
      toSignedBEI [True, True] `shouldBe` negate 1
      toSignedBEI [True, False, False, True] `shouldBe` negate 7
      toSignedBEI [False, True, False, True] `shouldBe` 5
    it "Words to BitStream" $ do
      toBits [] `shouldBe` []
      toBits [0] `shouldBe` [False, False, False, False, False, False, False,
                             False]
      toBits [0, 1] `shouldBe` [False, False, False, False, False, False,
                                False, False, False, False, False, False,
                                False, False, False, True]
      toBits [5, 255] `shouldBe` [False, False, False, False, False, True,
                                  False, True,  True,  True,  True,  True,
                                  True,  True,  True,  True]
    it "BitStream to Words" $ do
      toWords (replicate 8 False) `shouldBe` [0]
      toWords [False, False, False, False, False, False, False, False, False,
               False, False, False, False, False, False, True]
        `shouldBe` [0, 1]  
      toWords [False, False, False, False, False, True, False, True, True,
               True, True,  True, True,  True,  True,  True]
        `shouldBe` [5, 255] 
    it "Partial BitStream to Words" $ do
      toWords [] `shouldBe` []
      toWords (replicate 1 True)  `shouldBe` [128]
      toWords (replicate 2 True)  `shouldBe` [192]
      toWords (replicate 3 True)  `shouldBe` [224]
      toWords (replicate 4 True)  `shouldBe` [240]
      toWords (replicate 5 True)  `shouldBe` [248]
      toWords (replicate 6 True)  `shouldBe` [252]
      toWords (replicate 7 True)  `shouldBe` [254]
      toWords (replicate 8 True)  `shouldBe` [255]
  describe "BitTraverse Monad" $ do
    it "Should always return given value" $ do
      32 `shouldBe` parseBits (return 32) ""
      Just "Hello" `shouldBe` parseBits (return $ Just "Hello") "00"
    it "Should extract 1 number" $ do
      84 `shouldBe` parseBits (grabUnsignedBE 8) "T"
    it "Should extract 2 numbers" $ do
      (84, 69) `shouldBe`
        parseBits (
          do a <- grabUnsignedBE 8
             b <- grabUnsignedBE 8
             return (a, b)
          )
          "TEAM"
    it "Should extract 4 numbers" $ do
      -- Note reverse order here.
      [77, 65, 69, 84] `shouldBe`
        parseBits (replicateM 4 $ grabUnsignedBE 8) "TEAM"
