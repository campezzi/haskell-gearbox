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
  show gearBox =
    "Current Gear: " ++ show currentNumber ++ "/" ++ show totalNumber
    where
      (lower, current, higher) = getGears gearBox
      currentNumber = length lower + 1
      totalNumber = currentNumber + length higher

data GearPerformance
  = Optimal
  | RPMTooLow
  | RPMTooHigh

optimizePerformance :: GearBox -> RPM -> GearBox
optimizePerformance gearBox rpm =
  case gearPerformance (currentGear gearBox) rpm of
    RPMTooLow -> shiftDown gearBox
    RPMTooHigh -> shiftUp gearBox
    Optimal -> gearBox

getGears :: GearBox -> ([Gear], Gear, [Gear])
getGears = liftA3 (,,) lowerGears currentGear higherGears

gearPerformance :: Gear -> RPM -> GearPerformance
gearPerformance gear rpm
  | rpm < minimumRPM gear = RPMTooLow
  | rpm > maximumRPM gear = RPMTooHigh
  | otherwise = Optimal

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
