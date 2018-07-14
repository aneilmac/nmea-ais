{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS.Internal.Parse.SixBits
  ( d6
  , decodeAscii
  , decodeAscii'
  ) where

import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.Bits

import NMEA.AIS.Internal.Parse.BitsTraverse (BitStream)

-- | Takes an ASCII values and converts it into its 6 bits representation.
--   equivalent. Subtracts 48 from the ASCII value, if the result is greater
--   than 40 then subtracts 8.
d6 :: Word8 -> Word8
d6 c = let c' = c - 48
        in if c' > 40
              then c' - 8
              else c'
{-# INLINE d6 #-}

-- | Decodes an ASCII stream into a stream of contiguous sixbit values of the
--   BitStream format for consumption by a BitDecoder.
--   This function is optimized for BitStream conversion.
--   Equivalent to  @toBits . Data.ByteString.unpack . decodeAscii@ only when
--   the stream length is divisible by both 8 and 6. Otherwise 'decodeAscii'
--   will add additional padding bits to bring up the size to an 8
--   divisor while 'decodeAscii'' does not padding bits.
decodeAscii' :: B.ByteString -- ^ Input stream of ASCII
             -> BitStream -- ^ Output stream of decoded sixbits.
decodeAscii' = toSixBits . B.unpack . B.map d6
  where toSixBits [] =  []
        toSixBits (b:bs) = testBit b 5 : testBit b 4 : testBit b 3 
                         : testBit b 2 : testBit b 1 : testBit b 0
                         : toSixBits bs

-- | Decodes an ASCII stream into a stream of contiguous sixbit values.
--   Note: this function will add additional padding bits to the datastream
--   such that the stream size is divisible by 8. For a non-padded stream look
--   into alternative 'decodeAscii''
decodeAscii :: B.ByteString -- ^ Input stream of ASCII
            -> B.ByteString -- ^ Output stream of decoded sixbits.
decodeAscii ss
  | Just (s', ss') <- B.uncons ds = B.pack $ decodePacked s' ss'
  | otherwise                     = B.empty
  where ds = B.map d6 ss

-- | Decodes packed sixbits into one long six-bit stream. Shifts the first word
--   by 2 bits, add ORs the first 2 bits of the next word. The remainder of the
--   second word is then parsed with the rest of the stream.
--   So Bytestream: 00ABCDEF 00abcdef would transform into ABCDEFab cdef00
--   (Note padding bytes that must be added to bring us up to a Word8 size.)
decodePacked :: Word8         -- ^ The sixbit to pack up
             -> B.ByteString  -- ^ The remaining stream
             -> [Word8]       -- ^ The collection of packed words
decodePacked s ss
  | Just (s', ss') <- B.uncons ss = packTwo s s' : decodePacked (strip s') ss'
  | otherwise                     = [shift s 2]
  where packTwo a b = shift a 2 .|. shift b (-4) -- Combine the two words
        strip a = shift (15 .&. a) 2             -- Strip second word

