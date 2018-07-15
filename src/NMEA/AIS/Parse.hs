{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS.Parse 
  ( parseAIS
  ) where

import Prelude hiding (map, take, foldl, drop, head, length, repeat)
import Control.Applicative ((<|>))
import Data.Char (ord)
import Data.Attoparsec.ByteString.Lazy
import Data.Bits (bit, xor, (.|.), (.&.), complement)
import qualified Data.ByteString as B
import Text.Printf (printf)

import NMEA.AIS
import NMEA.AIS.PositionReportClassA
import NMEA.AIS.Internal.Parse.ByteStringHelpers
import NMEA.AIS.Internal.Parse.SixBits
import qualified NMEA.AIS.Internal.Parse.BitsTraverse as A

parseMessage :: Parser [AIS]
parseMessage = do
  many' parseAIS

decimal :: Integral a => Parser a
decimal = B.foldl' step 0 `fmap` takeWhile1 isDecimal
  where step a c = a * 10 + fromIntegral (fromEnum c - 48)

parseAIS :: Parser AIS
parseAIS = do
  char '!'
  (consumed, gen) <- match $ do
    talker <- decodeTalkerID
    string "VDM" <|> string "VDO"
    char ','
    sentanceCount <- decimal :: Parser Int
    char ','
    sentanceIndex <- decimal :: Parser Int
    char ','
    messageID <- Just <$> decimal <|> return Nothing :: Parser (Maybe Int)
    char ','
    channelCode <- decodeChannel
    char ','
    content <- decodeContent
    char ','
    digit
    return $ AIS talker channelCode content
  char '*'
  h <- hexadecimal
  let chVal = checksum consumed
  if h == chVal
     then return gen
     else fail $ printf "AIS Checksum mismatch. Expected %d, got %d." h chVal

decodeTalkerID :: Parser TalkerID
decodeTalkerID = matchTalker <$> take 2
  where matchTalker :: B.ByteString -> TalkerID
        matchTalker sp = case sp of
          "AB" -> AB
          "AD" -> AD
          "AI" -> AI
          "AN" -> AN
          "AR" -> AR
          "AS" -> AS
          "AT" -> AT
          "AX" -> AX
          "BS" -> BS
          "SA" -> SA

decodeChannel :: Parser ChannelCode
decodeChannel =
  (<|>) (char 'A' <|> char '1' >> return A)
        (char 'B' <|> char '2' >> return B)

decodeContent :: Parser AISContent
decodeContent = do
  t <- takeTill $ (==) $ ch ','
  let v = d6 $ B.head t
  if v == 1 || v == 2 || v == 3
     then let (A.Done bs a) = A.parseBits' posClassA (decodeAscii' t)
           in return $ PosClassA a
     else return $ Raw $ decodeAscii t

checksum :: B.ByteString -> Int
checksum = B.foldl (\i c -> fromEnum c `xor` i) 0
{-# INLINE checksum #-}

