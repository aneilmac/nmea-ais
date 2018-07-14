{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS.Internal.Parse.ByteStringHelpers
 ( ch
 , char
 , isDecimal
 , digit
 , hexadecimal
 )
 where

import Data.Word
import Data.Bits
import Data.ByteString
import Data.Attoparsec.ByteString

ch :: Char -> Word8
ch = toEnum . fromEnum
{-# INLINE ch #-}

char :: Char -> Parser Word8
char = word8 . ch

isDecimal :: Word8 -> Bool
isDecimal a = a >= ch '0' && a <= ch '9'
{-# INLINE isDecimal #-}

digit :: Parser Word8 
digit = satisfy isDecimal <?> "digit"

hexadecimal :: (Integral a, Bits a) => Parser a
hexadecimal = foldl' step 0 `fmap` takeWhile1 isHexDigit
  where
    isHexDigit :: Word8 -> Bool 
    isHexDigit c = (c >= ch '0' && c <= ch '9') ||
                   (c >= ch 'a' && c <= ch 'f') ||
                   (c >= ch 'A' && c <= ch 'F')
    step a w | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
             | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
             | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)
