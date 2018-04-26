{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS.Parse where

import Prelude hiding (map, take, foldl, drop, head, length, repeat)
import Control.Applicative ((<|>))
import Data.Char (ord, chr)
import Data.Attoparsec.Text.Lazy
import Data.Text hiding (take, unfoldr)
import Data.Bits (xor, shift, (.|.), (.&.))
import Data.ByteString (ByteString, unfoldr)
import Data.Word (Word8)
import Text.Printf (printf)

import NMEA.AIS
import NMEA.AIS.ManeuverIndicator
import NMEA.AIS.NavigationStatus
import NMEA.AIS.PositionReportClassA

parseMessage :: Parser [AIS]
parseMessage = do
  many' parseAIS

parseAIS :: Parser AIS
parseAIS = do 
  char '!'
  (consumed, gen) <- match $ do
    talker <- decodeTalkerID
    string "VDM" <|> "VDO"
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

d6 :: Char -> Char
d6 c = let c' = ord c - 48
        in if c' > 40
              then chr $ c' - 8
              else chr c'
{-# INLINE d6 #-}

-- | Decodes six-bit encoding. Subtracts 48 from the ASCII value, if the result
--   is greater than 40 then substracts 8.
decodeSixBits :: Text -> Text
decodeSixBits = map d6

decodeTalkerID :: Parser TalkerID
decodeTalkerID = matchTalker <$> take 2
  where matchTalker :: Text -> TalkerID
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
  t <- takeTill $ (==) ','
  let v = ord . d6 $ head t
  if v == 1 || v == 2 || v == 3
     then PosClassA <$> decodePosClassA v
     else return $ Raw $ decodeSixBits t

decodePosClassA :: Int -> Parser PositionReportClassA
decodePosClassA v = do
  return $ PRCA 
    { type' = v
    , repeat = 0
    , mmsi = 0
    , status = UNDER_WAY
    , turn = 0
    , speed = Nothing
    , accuracy = False
    , lon = 0
    , lat = 0
    , course = Nothing
    , heading = 0
    , second = Nothing
    , maneuver = (ManeuverIndicator 0)
    , raim = False
    , radio = 0
   } 

checksum :: Text -> Int
checksum = foldl (\i c -> ord c `xor` i ) 0

