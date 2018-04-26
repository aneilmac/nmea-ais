module NMEA.AIS.ManeuverIndicator where

newtype ManeuverIndicator = ManeuverIndicator Int deriving (Eq)

instance Show ManeuverIndicator where
  show (ManeuverIndicator 0) = "Not available (default)"
  show (ManeuverIndicator 1) = "No special maneuver" -- American English
  show (ManeuverIndicator 2) = "Special maneuver (such as regional passing \
                               \arrangement)"

