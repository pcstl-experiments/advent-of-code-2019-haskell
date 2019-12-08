module Lib
    ( totalFuelRequirements
    ) where

import System.IO (isEOF)

-- gets the mass of a single module from the input
getModuleMass :: IO (Maybe Integer)
getModuleMass = do
  done <- isEOF
  if done
    then return Nothing
    else Just <$> read <$> getLine

-- gets the fuel a given mass would need if fuel had 0 mass
naiveFuelRequirement :: Integer -> Integer
naiveFuelRequirement mass = max 0 ((mass `div` 3) - 2)

-- gets the fuel a given mass *actually* needs, taking fuel weight into consideration
moduleFuelRequirement :: Integer -> Integer
moduleFuelRequirement mass =
  moduleFuelRequirement' mass 0
    where moduleFuelRequirement' residual acc =
            let fuelMass = naiveFuelRequirement residual in
              if fuelMass == 0
              then acc
              else moduleFuelRequirement' fuelMass (acc + fuelMass)

-- adds up all fuel requirements for all the mass
totalFuelRequirements :: IO Integer
totalFuelRequirements = addFurtherRequirements 0
  where addFurtherRequirements acc =
          getModuleMass >>= 
            maybe
              (pure acc)
              (\mass -> addFurtherRequirements $ acc + (moduleFuelRequirement mass))
