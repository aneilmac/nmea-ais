{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS.Parse where

import Text.Printf (printf)
import Prelude hiding (map, take, foldl, drop, head, length)
import Control.Applicative ((<|>))
import Data.Char (ord, chr)
import Data.Attoparsec.Text.Lazy
import Data.Text hiding (take, unfoldr)
import Data.Bits (xor, shift, (.|.), (.&.))
import Data.ByteString (ByteString, unfoldr)
import Data.Word (Word8)

import NMEA.AIS

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

-- | Decodes six-bit encoding. Subtracts 48 from the ASCII value, if the result
--   is greater than 40 then substracts 8.
decodeSixBits :: Text -> Text
decodeSixBits = map $ \c -> let c' = ord c - 48 in if c' > 40
                                                      then chr $ c' - 8
                                                      else chr c'

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

toWord8ByteString :: Text -> ByteString
toWord8ByteString = unfoldr append8
  where append8 :: Text -> Maybe (Word8, Text)
        append8 "" = Nothing
        append8 t 
          | length t > 1 = 
              let a = wordHead t
                  t' = drop 1 t
                  b = wordHead t'
               in Just (shift a 2 .|. (b .&. 3), t')
          | otherwise = Just (wordHead t `shift` 2, "")
          where wordHead :: Text -> Word8
                wordHead = toEnum . ord . head

decodeChannel :: Parser ChannelCode
decodeChannel =
  (<|>) (char 'A' <|> char '1' >> return A)
        (char 'B' <|> char '2' >> return B)

decodeContent :: Parser AISContent
decodeContent = do
  takeTill $ (==) ','
  return AISContent

checksum :: Text -> Int
checksum = foldl (\i c -> ord c `xor` i ) 0

