{-# LANGUAGE PatternSynonyms #-}
module NMEA.AIS.PositionReportClassA where

import Data.Time.Clock

data PositionReportClassA = PRCA
  { type'    :: Int
  , repeat   :: Int
  , mmsi     :: Int
  , status   :: NavigationStatus
  , turn     :: Float
  , speed    :: Maybe Float
  , accuracy :: Bool
  , lon      :: Float
  , lat      :: Float
  , course   :: Maybe Float
  , heading  :: Int
  , second   :: Maybe Int
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
