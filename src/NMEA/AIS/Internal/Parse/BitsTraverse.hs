{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Copyright   : (c) Archibald Neil MacDonald 2018
Maintainer  : FortOyer@hotmail.co.uk
Stability   : experimental
Portability : POSIX

This is a very simple parec-like parser for extracting arbitrary-length fields
from a long stream of bits. Fields can be extracted and converted into
signed/unsigned integers, booleans, strings and floats that conform to the AIS
sixbit format.

-}
module NMEA.AIS.Internal.Parse.BitsTraverse 
  ( BitStream
  -- * BitTraverse
  , BitsTraverse
  , parseBits
  -- $bitsTraverse
  , Result (..)
  -- * Grabs
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
  , toWords
  , toBits
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
grabGeneric f i = BT $ \bs -> 
  if length bs < i 
     then Partial $ \bs' ->  let (a, b) = splitAt i (bs <> bs') in Done b (f a)
     else let (a, b) = splitAt i bs in Done b $ f a

-- | Grabs a signed big-endian integer from the BitStream.
grabSignedBE :: Int -- ^ Size of field to consume.
             -> BitsTraverse Int
grabSignedBE = grabGeneric toSignedLE

-- Grabs a signed little-endian integer from the BitStream.
grabSignedLE :: Int -- ^ Size of field to consume.
             -> BitsTraverse Int
grabSignedLE = grabGeneric toSignedLE

-- | Grabs an unsigned big-endian integer from the BitStream.
grabUnsignedBE :: Int -- ^ Size of field to consume.
               -> BitsTraverse Int
grabUnsignedBE = grabGeneric toUnsignedBE

-- | Grabs an unsigned little-endian integer from the BitStream.
grabUnsignedLE :: Int -- ^ Size of field to consume.
               -> BitsTraverse Int
grabUnsignedLE = grabGeneric toUnsignedLE

-- | Grabs a float from the BitStream.
grabFloat :: Int -- ^ Size of field to consume.
          -> BitsTraverse Float
grabFloat = undefined --grabGeneric toFloat

-- | Grabs a sixbit encoded string from the BitStream.
grabString :: Int -- ^ Size of field to consume.
           -> BitsTraverse B.ByteString
grabString = grabGeneric toString

-- | Takes a single bit from the BitStream and converts it into a bool.
grabBool :: BitsTraverse Bool
grabBool = grabGeneric head 1

-- | Skips a section of BitStream.
skip :: Int -- ^ Size of field to consume.
     -> BitsTraverse ()
skip = grabGeneric $ const ()

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

-- $bitsTraverse
--
-- An example parser:
--
-- @
--    m :: BitsTraverse (Bool, Int)
--    m = do
--      b <- grabBool
--      i <- grabUnsignedBE 4 
--      return (b, i)
--
--    r = parseBits' m [True, False, True, True, True] 
-- @
--
-- @r@ is @Done [] (True, 14)@. The first argument of Done is the
-- unconsumed input (none), and the second is the result of the computation.
--
-- In cases where not enough input is passed to the parser, the parser will
-- return a partial result object which can be fed with additional input to get
-- the final output. An example parser:
--
-- @
--    m :: BitsTraverse (Bool, Int)
--    m = do
--      b <- grabBool
--      i <- grabUnsignedBE 4 
--      return (b, i)
--
--    p = parseBits' m [True, False, True, True] 
--    r = p [True, False, True]
-- @
--
-- @p@ Would be a 'Partial' Result. @r@ would be 
-- @Done [False, True] (True, 14)@. Note the unconsumed input which was passed.

-- | Given a BitsTraverseMonad and a stream to consume, returns the result of
--   the consumed stream.
--
--   This is a variant that consumes Bytestrings. Unconsumed output and input
--   must be divisible by 8.
parseBits :: BitsTraverse a -- ^ BitsTraverse to consume a stream.
          -> B.ByteString   -- ^ Stream to consume.
          -> Result B.ByteString a
parseBits a b = convertBits $ parseB a $ toBits $ B.unpack b
  where convertBits (Done bs o) = Done (B.pack $ toWords bs) o
        convertBits (Partial g) = Partial $ \bs ->
                                    convertBits $ g $ toBits $ B.unpack bs

-- | Given a BitsTraverseMonad and a stream to consume, returns the result of
--   the consumed stream. This consumes Bistreams directly.
--
--   This is a variant that consumes BitStreams. It is considered faster than
--   'parseBits''. Uncomsumed output and input can be a multiple of any number.
parseBits' :: BitsTraverse a -- ^ BitsTraverse to consume a stream.
           -> BitStream -- ^ Stream to consume.
           -> Result BitStream a
parseBits' = parseB

-- | The Result generated from a BitsTraverse run.
data Result i a = Done i a
                -- ^ Complete output. Unconsumed BitStream and the final state.
                | Partial (i -> Result i a)
                  -- ^ Partial output. Feed with additional data to complete.

instance Functor (Result i) where
  fmap f (Done bs a) = Done bs $ f a
  fmap f (Partial g) = Partial $ \bs -> f <$> g bs

-- | BitsTraversal Monad. This Monad can be used to consume bits from a
--   BitStream using the grab methods.
newtype BitsTraverse a = BT { parseB :: BitStream -> Result BitStream a }

instance Functor BitsTraverse where
  fmap f a = BT $ \bs -> f <$> parseB a bs

instance Applicative BitsTraverse where
  f <*> a = BT $ \bs -> ppA (parseB f bs) a
    where ppA (Done bs f) btA = f <$> parseB btA bs 
          ppA (Partial g) btA = Partial $ \bs -> ppA (g bs) btA

  pure a = BT $ \bs -> Done bs a

instance Monad BitsTraverse where
  a >>= f = BT $ \bs -> ppA (parseB a bs) f
    where ppA (Done bs a) f = parseB (f a) bs
          ppA (Partial b) f = Partial $ \bs -> ppA (b bs) f

