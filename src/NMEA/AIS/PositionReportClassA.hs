module NMEA.AIS.PositionReportClassA where

import NMEA.AIS.NavigationStatus
import NMEA.AIS.ManeuverIndicator
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

