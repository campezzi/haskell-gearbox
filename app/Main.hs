module Main where

import Control.Monad (forever)
import Control.Monad.Trans.State
import Lib
import Text.Read (readMaybe)

main :: IO ((), GearBox)
main = runStateT (forever iteration) genericGearBox

genericGear :: Gear
genericGear = Gear (RPM 500) (RPM 2000)

genericGearBox :: GearBox
genericGearBox = GearBox [] genericGear (replicate 4 genericGear)

iteration :: StateT GearBox IO ()
iteration =
  StateT $ \gearBox -> do
    putStrLn "Enter RPM: "
    parsedInput <- readMaybe <$> getLine
    case parsedInput of
      Nothing -> do
        putStrLn "Please enter a number."
        return ((), gearBox)
      Just rpm -> do
        print updatedGearBox
        return ((), updatedGearBox)
        where updatedGearBox = run gearBox (RPM rpm)
