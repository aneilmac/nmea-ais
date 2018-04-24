module NMEA.AIS.ShipType where

newtype ShipType = ShipType Int deriving (Eq) 
instance Show ShipType where
  show (ShipType a)
    | a == 0           =  "Not available (default)"
    | a > 0 && a < 20  = "Reserved for future use"
    | a == 20          = "Wing in ground (WIG), all ships of this type"
    | a == 21          = "Wing in ground (WIG), Hazardous category A"
    | a == 22          = "Wing in ground (WIG), Hazardous category B"
    | a == 23          = "Wing in ground (WIG), Hazardous category C"
    | a == 24          = "Wing in ground (WIG), Hazardous category D"
    | a > 24 && a < 30 = "Wing in ground (WIG), Reserved for future use"
    | a == 30          = "Fishing"
    | a == 31          = "Towing"
    | a == 32          = "Towing: length exceeds 200m or breadth exceeds 25m"
    | a == 33          = "Dredging or underwater ops"
    | a == 34          = "Diving ops"
    | a == 35          = "Military ops"
    | a == 36          = "Sailing"
    | a == 37          = "Pleasure Craft"
    | a == 38          = "Reserved"
    | a == 39          = "Reserved"
    | a == 40          = "High speed craft (HSC), all ships of this type"
    | a == 41          = "High speed craft (HSC), Hazardous category A"
    | a == 42          = "High speed craft (HSC), Hazardous category B"
    | a == 43          = "High speed craft (HSC), Hazardous category C"
    | a == 44          = "High speed craft (HSC), Hazardous category D"
    | a > 44 && a < 49 = "High speed craft (HSC), Reserved for future use"
    | a == 49          = "High speed craft (HSC), No additional information"
    | a == 50          = "Pilot Vessel"
    | a == 51          = "Search and Rescue vessel"
    | a == 52          = "Tug"
    | a == 53          = "Port Tender"
    | a == 54          = "Anti-pollution equipment"
    | a == 55          = "Law Enforcement"
    | a == 56          = "Spare - Local Vessel"
    | a == 57          = "Spare - Local Vessel"
    | a == 58          = "Medical Transport"
    | a == 59          = "Noncombatant ship according to RR Resolution No. 18"
    | a == 60          = "Passenger, all ships of this type"
    | a == 61          = "Passenger, Hazardous category A"
    | a == 62          = "Passenger, Hazardous category B"
    | a == 63          = "Passenger, Hazardous category C"
    | a == 64          = "Passenger, Hazardous category D"
    | a > 64 && a < 69 = "Passenger, Reserved for future use"
    | a == 69          = "Passenger, No additional information"
    | a == 70          = "Cargo, all ships of this type"
    | a == 71          = "Cargo, Hazardous category A"
    | a == 72          = "Cargo, Hazardous category B"
    | a == 73          = "Cargo, Hazardous category C"
    | a == 74          = "Cargo, Hazardous category D"
    | a > 74 && a < 79 = "Cargo, Reserved for future use"
    | a == 79          = "Cargo, No additional information"
    | a == 80          = "Tanker, all ships of this type"
    | a == 81          = "Tanker, Hazardous category A"
    | a == 82          = "Tanker, Hazardous category B"
    | a == 83          = "Tanker, Hazardous category C"
    | a == 84          = "Tanker, Hazardous category D"
    | a > 84 && a < 89 = "Tanker, Reserved for future use"
    | a == 89          = "Tanker, No additional information"
    | a == 90          = "Other Type, all ships of this type"
    | a == 91          = "Other Type, Hazardous category A"
    | a == 92          = "Other Type, Hazardous category B"
    | a == 93          = "Other Type, Hazardous category C"
    | a == 94          = "Other Type, Hazardous category D"
    | a > 94 && a < 99 = "Other Type, Reserved for future use"
    | a == 99          = "Other Type, no additional information"
    | otherwise        = show $ ShipType 0

wingInGround :: ShipType -> Bool
wingInGround (ShipType i) = i > 19 && i < 30

highSpeedCraft :: ShipType -> Bool
highSpeedCraft (ShipType i) = i > 39 && i < 50

passenger :: ShipType -> Bool
passenger (ShipType i) = i > 59 && i < 70

cargo :: ShipType -> Bool
cargo (ShipType i) = i > 69 && i < 80

tanker :: ShipType -> Bool
tanker (ShipType i) = i > 79 && i < 90

otherType :: ShipType -> Bool
otherType (ShipType i) = i > 89 && i < 100

