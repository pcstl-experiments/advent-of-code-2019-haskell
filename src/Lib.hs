module Lib
    ( totalFuelRequirement
    ) where

import System.IO (isEOF)

getModuleMass :: IO (Maybe Integer)
getModuleMass = do
  done <- isEOF
  if done
    then return Nothing
    else Just <$> read <$> getLine

fuelRequirement :: Integer -> Integer
fuelRequirement x = (x `div` 3) - 2

totalFuelRequirement :: IO Integer
totalFuelRequirement = addFurtherRequirements 0
  where addFurtherRequirements acc =
          getModuleMass >>= 
            maybe
              (pure acc)
              (\mass -> addFurtherRequirements $ acc + (fuelRequirement mass))

main :: IO ()
main = (show <$> totalFuelRequirement) >>= putStrLn
