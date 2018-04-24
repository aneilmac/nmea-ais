{-# LANGUAGE OverloadedStrings #-}
module NMEA.AIS where

import Data.Text

data TalkerID = AB | AD | AI | AN | AR | AS | AT | AX | BS | SA deriving (Eq)

instance Show TalkerID where
  show AB = "NMEA 4.0 Base AIS station"
  show AD = "MMEA 4.0 Dpendent AIS Base Station"
  show AI = "Mobile AIS station"
  show AN = "NMEA 4.0 Aid to Navigation AIS station"
  show AR = "NMEA 4.0 AIS Receiving Station"
  show AS = "NMEA 4.0 Limited Base Station"
  show AT = "NMEA 4.0 AIS Transmitting Station"
  show AX = "NMEA 4.0 Repeater AIS station"
  show BS = "Base AIS station (deprecated in NMEA 4.0)"
  show SA = "NMEA 4.0 Physical Shore AIS Station"

data ChannelCode = A | B deriving (Eq, Show)

mhz :: ChannelCode -> Text
mhz A = "161.975Mhz (87B)"
mhz B = "162.025Mhz (88B)"


data AIS = AIS
  { talkerId :: TalkerID
  , channelCode :: ChannelCode
  , content :: AISContent
  } deriving (Eq, Show)


data AISContent = AISContent deriving (Eq, Show)

