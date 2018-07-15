{-# LANGUAGE PatternSynonyms #-}
module NMEA.AIS.PositionReportClassA
  ( PositionReportClassA (..)
  , NavigationStatus (..)
  , Second (..)
  , pattern UNDER_WAY
  , pattern AT_ANCHOR
  , pattern NOT_UNDER_COMMAND
  , pattern CONSTRAINED_BY_DRAUGHT
  , pattern MOORED
  , pattern AGROUND
  , pattern FISHING
  , pattern SAILING
  , toLat
  , toLon
  , toSpeedOverGround
  , SpeedOverGround
  , ManeuverIndicator (..)
  , posClassA
  ) where

import Data.Ratio
import qualified NMEA.AIS.Internal.Parse.BitsTraverse as A

data Second = NotAvailable
            | ManualInputMode
            | DeadReckoningMode
            | Inoperative
            | Second Int
            deriving (Show, Eq)

toSecond :: Int -> Second
toSecond 60 = NotAvailable
toSecond 61 = ManualInputMode
toSecond 62 = DeadReckoningMode
toSecond 63 = Inoperative
toSecond i  = Second i

data PositionReportClassA = PRCA
  { type'    :: Int
  , repeat'  :: Int
  , mmsi     :: Int
  , status   :: NavigationStatus
  , turn     :: Float
  , speed    :: Maybe SpeedOverGround
  , accuracy :: Bool
  , lon      :: Maybe Float
  , lat      :: Maybe Float
  , course   :: Maybe Float
  , heading  :: Maybe Int
  , second   :: Second
  , maneuver :: ManeuverIndicator
  , raim     :: Bool
  , radio    :: Int
  }
  deriving (Show, Eq)

newtype NavigationStatus = NavigationStatus Int deriving (Eq)

pattern UNDER_WAY              = NavigationStatus 0
pattern AT_ANCHOR              = NavigationStatus 1
pattern NOT_UNDER_COMMAND      = NavigationStatus 3
pattern CONSTRAINED_BY_DRAUGHT = NavigationStatus 4
pattern MOORED                 = NavigationStatus 5
pattern AGROUND                = NavigationStatus 6
pattern FISHING                = NavigationStatus 7
pattern SAILING                = NavigationStatus 8

newtype SpeedOverGround = SOG Int

toSpeedOverGround :: Int -> Maybe SpeedOverGround
toSpeedOverGround i = let s = SOG i
                       in if s < minBound || s > maxBound
                             then Nothing 
                             else Just s

instance Bounded SpeedOverGround where
  minBound = SOG 0
  maxBound = SOG 1022

instance Enum SpeedOverGround where
  toEnum i         = SOG i
  fromEnum (SOG i) = i

instance Eq SpeedOverGround where
  (SOG i) == (SOG j) = i == j

instance Ord SpeedOverGround where
  compare (SOG i) (SOG j) = compare i j

instance Num SpeedOverGround where
  (SOG i) + (SOG j) = SOG $ i + j
  (SOG i) * (SOG j) = SOG $ i * j
  abs (SOG i)       = SOG $ abs i
  signum (SOG i)    = SOG $ signum i
  fromInteger i     = SOG $ fromInteger i
  negate (SOG i)    = SOG $ negate i

instance Real SpeedOverGround where
  toRational (SOG i) = toInteger i % 10

instance Show SpeedOverGround where
  show s@(SOG i)
    | i >= 1022 =  ">=102.2 knots"
    | otherwise = "~" ++ (show $ toDbForm s) ++ " knots"
    where toDbForm x = (fromRational . toRational) x :: Float

instance Fractional SpeedOverGround where
  recip (SOG i)  = fromRational $ 10 % toInteger i
  fromRational r = SOG $ round $ (fromRational r :: Float) * 10

instance Show NavigationStatus where
  show (NavigationStatus 0)  = "Under way using engine"
  show (NavigationStatus 1)  = "At anchor"
  show (NavigationStatus 2)  = "Not under command"
  show (NavigationStatus 3)  = "Restricted manoeuvrability"
  show (NavigationStatus 4)  = "Constrained by her draught"
  show (NavigationStatus 5)  = "Moored"
  show (NavigationStatus 6)  = "Aground"
  show (NavigationStatus 7)  = "Engaged in Fishing"
  show (NavigationStatus 8)  = "Under way sailing"
  show (NavigationStatus 9)  = "Reserved for future amendment of Navigational \
                               \Status for HSC"
  show (NavigationStatus 10) = "Reserved for future amendment of Navigational \
                               \Status for WIG"
  show (NavigationStatus 11) = "Reserved for future use"
  show (NavigationStatus 12) = "Reserved for future use"
  show (NavigationStatus 13) = "Reserved for future use"
  show (NavigationStatus 14) = "AIS-SART is active"
  show (NavigationStatus 15) = "Not defined (default)"

newtype ManeuverIndicator = ManeuverIndicator Int deriving (Eq)

instance Show ManeuverIndicator where
  show (ManeuverIndicator 0) = "Not available (default)"
  show (ManeuverIndicator 1) = "No special maneuver" -- American English
  show (ManeuverIndicator 2) = "Special maneuver (such as regional passing \
                               \arrangement)"

toLon :: Int -> Maybe Float
toLon 0x6791AC0 = Nothing
toLon i = Just $ (fromRational . toRational $ i) / 600000.0

toLat :: Int -> Maybe Float
toLat 0x3412140  = Nothing
toLat i = Just $ (fromRational . toRational $ i) / 600000.0

posClassA :: A.BitsTraverse PositionReportClassA
posClassA = do
  type'    <- A.grabUnsignedBE 6
  repeat'  <- A.grabUnsignedBE 2
  mmsi     <- A.grabUnsignedBE 30
  status   <- A.grabUnsignedBE 4

  turn     <- A.grabSignedBE 8

  speed    <- A.grabUnsignedBE 10

  accuracy <- A.grabBool

  lon      <- A.grabSignedBE 28
  lat      <- A.grabSignedBE 27

  course   <- A.grabUnsignedBE 12
  heading  <- A.grabUnsignedBE 9
  second   <- A.grabUnsignedBE 6
  maneuver <- A.grabUnsignedBE 2

  A.skip 3

  raim     <- A.grabBool

  radio    <- A.grabUnsignedBE 19

  return $ PRCA
    { type' = type'
    , repeat' = repeat'
    , mmsi = mmsi
    , status = NavigationStatus status
    , turn = fromIntegral turn
    , speed = toSpeedOverGround speed
    , accuracy = accuracy
    , lon = toLon lon
    , lat = toLat lat
    , course = if course   == 3600
                  then Nothing else Just $ (fromRational . toRational) course
    , heading = if heading == 511 
                   then Nothing else Just heading
    , second = toSecond second
    , maneuver = ManeuverIndicator maneuver
    , raim = raim
    , radio = radio
    }
