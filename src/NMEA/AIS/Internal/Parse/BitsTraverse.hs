{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Description : TODO
Copyright   : (c) Archibald Neil MacDonald 2018
Maintainer  : FortOyer@hotmail.co.uk
Stability   : experimental
Portability : POSIX
-}
module NMEA.AIS.Internal.Parse.BitsTraverse 
  ( BitStream
  , toWords
  , toBits
  -- * Monadic
  , BitsTraverse
  -- ** Parse
  , parseBits
  , parseBits'
  , parseBitsPartial
  , parseBitsPartial'
  -- ** Grabs
  , grabSignedBE
  , grabUnsignedBE
  , grabSignedLE
  , grabUnsignedLE
  , grabFloat
  , grabBool
  , grabString
  , skip
  -- * BitStream Conversion
  , toUnsignedBE
  , toUnsignedLE
  , toSignedBE
  , toSignedLE
  , toString
  ) where

import qualified Data.ByteString as B
import Data.Bits
import Data.Word (Word8)

type BitStream = [Bool]

-- | Internal grab function for common types. Converts a BitStream conversion
--   function into a grab monad.
grabGeneric :: (BitStream -> a) -- ^ Conversion function
            -> Int -- ^ Size of field to consume.
            -> BitsTraverse a
grabGeneric f i = BT $ \bs -> let (a, b) = splitAt i bs in (f a, b)

-- Grabs a signed big-endian integer from the BitStream.
grabSignedBE :: Int -- ^ Size of field to consume.
             -> BitsTraverse Int
grabSignedBE = grabGeneric toSignedLE

-- Grabs a signed little-endian integer from the BitStream.
grabSignedLE :: Int -- ^ Size of field to consume.
             -> BitsTraverse Int
grabSignedLE = grabGeneric toSignedLE

-- Grabs an unsigned big-endian integer from the BitStream.
grabUnsignedBE :: Int -- ^ Size of field to consume.
               -> BitsTraverse Int
grabUnsignedBE = grabGeneric toUnsignedBE

-- Grabs an unsigned little-endian integer from the BitStream.
grabUnsignedLE :: Int -- ^ Size of field to consume.
               -> BitsTraverse Int
grabUnsignedLE = grabGeneric toUnsignedLE

-- Grabs a float from the BitStream.
grabFloat :: Int -- ^ Size of field to consume.
          -> BitsTraverse Float
grabFloat = undefined --grabGeneric toFloat

-- Grabs a sixbit encoded string from the BitStream.
grabString :: Int -- ^ Size of field to consume.
           -> BitsTraverse B.ByteString
grabString = grabGeneric toString

-- | Takes a single bit from the BitStream and converts it into a bool.
grabBool :: BitsTraverse Bool
grabBool = grabGeneric head 1

-- | Skips a section of BitStream.
skip :: Int -- ^ Size of field to consume.
     -> BitsTraverse ()
skip i = BT $ \(bs) -> ((), drop i bs)

-- | Converts a BitStream into an unsigned big-endian number.
toUnsignedBE :: Bits a => BitStream -> a
toUnsignedBE = toUBE zeroBits
  where toUBE i []        = i
        toUBE i (True:xs) = toUBE ((i `shiftL` 1) `setBit` 0) xs
        toUBE i (_:xs)    = toUBE (i `shiftL` 1) xs

-- | Converts a BitStream into a signed big-endian number. The most significant
--   bit is the signed bit.
toSignedBE :: Bits a => BitStream -> a
toSignedBE []     = zeroBits
toSignedBE (b:bs) = toSBE (if b then complement zeroBits else zeroBits) bs
  where toSBE i []        = i
        toSBE i (True:xs) = toSBE ((i `shiftL` 1) `setBit` 0) xs
        toSBE i (_:xs)    = toSBE ((i `shiftL` 1) `clearBit` 0) xs

-- | Converts a BitStream into an signed little-endian number.
toUnsignedLE :: Bits a => BitStream -> a
toUnsignedLE []         = zeroBits
toUnsignedLE (True:bs)  = (toUnsignedLE bs `shiftL` 1) `setBit` 0
toUnsignedLE (_:bs)     = toUnsignedLE bs `shiftL` 1

-- | Converts a BitStream into a signed little-endian number. The most
--   significant bit is the signed bit.
toSignedLE :: Bits a => BitStream -> a
toSignedLE []         = zeroBits
toSignedLE [True]     = complement zeroBits
toSignedLE (True:bs)  = (toSignedLE bs `shiftL` 1) `setBit` 0
toSignedLE (_:bs)     = (toSignedLE bs `shiftL` 1) `clearBit` 0

-- | Converts a BitStream into string format. Nibbles of 6-bits are converted 
--   by a string where words 0-31 match ASCII 64-95, and words 32-63 match 
--   ASCII 32-63.
--
--   Anything not in this range, including lowercase ASCII letters, can not be
--   encoded.
toString :: BitStream -> B.ByteString
toString ss = let ws = map (`shiftR` 2) (toWords ss)
               in B.pack $ map (\w -> if w < 32 then w + 64 else w) ws 

toFloat :: BitStream -> Float
toFloat = undefined
  
-- | Converts list of words into a stream of contiguous bits.
toBits :: [Word8] -> BitStream
toBits []     = []
toBits (b:bs) = testBit b 7 : testBit b 6 : testBit b 5 : testBit b 4
              : testBit b 3 : testBit b 2 : testBit b 1 : testBit b 0
              : toBits bs

-- | Converts a BitStream to a list of Words. Bitstream will be padded to make
--   the stream divisible by 8.
toWords ::  BitStream -> [Word8]
toWords [] = []
toWords [a,b,c,d,e,f,g]      = [apB a 7 .|. apB b 6 .|. apB c 5 .|. apB d 4 .|.
                                apB e 3 .|. apB f 2 .|. apB g 1]
toWords [a,b,c,d,e,f]        = [apB a 7 .|. apB b 6 .|. apB c 5 .|. apB d 4 .|.
                                apB e 3 .|. apB f 2]
toWords [a,b,c,d,e]          = [apB a 7 .|. apB b 6 .|. apB c 5 .|. apB d 4 .|.
                                apB e 3]
toWords [a,b,c,d]            = [apB a 7 .|. apB b 6 .|. apB c 5 .|. apB d 4]
toWords [a,b,c]              = [apB a 7 .|. apB b 6 .|. apB c 5]
toWords [a,b]                = [apB a 7 .|. apB b 6]
toWords [a]                  = [apB a 7]
toWords (a:b:c:d:e:f:g:h:ss) = (apB a 7 .|. apB b 6 .|. apB c 5 .|. apB d 4 .|.
                                apB e 3 .|. apB f 2 .|. apB g 1 .|. apB h 0)
                             : toWords ss

-- | Utility function
--   @apB b i@ is equivalent to @if b then bit i else zeroBits@.
--   Used in 'toWords'.
apB :: Bool -> Int -> Word8
apB b i = if b then bit i else zeroBits

parseBits' :: BitsTraverse a -> BitStream -> a
parseBits' b a = fst $ parseB b a

parseBitsPartial' :: BitsTraverse a -> BitStream -> (a, BitStream)
parseBitsPartial' = parseB

parseBits :: BitsTraverse a -> B.ByteString -> a
parseBits b a = fst $ parseB b (toBits $ B.unpack a)

parseBitsPartial :: BitsTraverse a -> B.ByteString -> (a, B.ByteString)
parseBitsPartial b a = let (o, ss) = parseB b (toBits $ B.unpack a)
                        in (o, B.pack $ toWords ss)

data Result a = Done ByteString a
              | Partial (ByteString -> Result a)

-- | BitsTraversal Monad. This Monad can be used to consume bits from a
--   BitStream using the grab methods.
newtype BitsTraverse a = BT { parseB :: BitStream -> (a, BitStream)  }

instance Functor BitsTraverse where
  fmap f a = BT $ \bs -> let (a', bs') = parseB a bs in (f a', bs')

instance Applicative BitsTraverse where
  f <*> a = BT $ \bs ->
    let (a', bs') = parseB a bs
        (f', bs'') = parseB f bs'
     in (f' a', bs'')

  pure  a = BT $ \bs -> (a, bs)

instance Monad BitsTraverse where
  a >>= f = BT $ \bs -> let (a', bs') = parseB a bs in parseB (f a') bs'

