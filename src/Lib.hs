module Lib where

import Control.Applicative (liftA3)

newtype RPM =
  RPM Integer
  deriving (Show, Eq, Ord)

data Gear = Gear
  { minimumRPM :: RPM
  , maximumRPM :: RPM
  } deriving (Show)

data GearBox = GearBox
  { lowerGears :: [Gear]
  , currentGear :: Gear
  , higherGears :: [Gear]
  }

instance Show GearBox where
  show gearBox = "Current Gear: " ++ currentNumber ++ "/" ++ totalNumber
    where
      (lower, current, higher) = getGears gearBox
      currentNumber = show $ length lower + 1
      totalNumber = show $ length lower + length higher + 1

data Error =
  GearBoxMustContainAtLeastOneGear

data GearPerformance
  = Ideal
  | RPMTooLow
  | RPMTooHigh

mkGearBox :: [Gear] -> Either Error GearBox
mkGearBox [] = Left GearBoxMustContainAtLeastOneGear
mkGearBox (firstGear:otherGears) = Right (GearBox [] firstGear otherGears)

run :: GearBox -> RPM -> GearBox
run gearBox rpm =
  case gearPerformance current rpm of
    RPMTooLow -> shiftDown gearBox
    RPMTooHigh -> shiftUp gearBox
    Ideal -> gearBox
  where
    (lower, current, higher) = getGears gearBox

getGears :: GearBox -> ([Gear], Gear, [Gear])
getGears = liftA3 (,,) lowerGears currentGear higherGears

gearPerformance :: Gear -> RPM -> GearPerformance
gearPerformance gear rpm
  | rpm < minimumRPM gear = RPMTooLow
  | rpm > maximumRPM gear = RPMTooHigh
  | otherwise = Ideal

shiftDown :: GearBox -> GearBox
shiftDown gearBox =
  case lower of
    [] -> gearBox
    (g:gs) -> GearBox gs g (current : higher)
  where
    (lower, current, higher) = getGears gearBox

shiftUp :: GearBox -> GearBox
shiftUp gearBox =
  case higher of
    [] -> gearBox
    (g:gs) -> GearBox (current : lower) g gs
  where
    (lower, current, higher) = getGears gearBox
