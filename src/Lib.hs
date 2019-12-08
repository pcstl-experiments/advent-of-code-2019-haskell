module Lib
    ( totalFuelRequirements
    ) where

import System.IO (isEOF)

-- gets the fuel a given mass would need if fuel had 0 mass
naiveFuelRequirement :: Integer -> Integer
naiveFuelRequirement mass = max 0 ((mass `div` 3) - 2)

-- makes an infinite list with the fuel requirements for a module,
-- followed by the fuel requirements for its fuel, and so on.
moduleFuelRequirements :: Integer -> [Integer]
moduleFuelRequirements = tail . iterate naiveFuelRequirement

-- gets the sum of a possibly infinite strictly decreasing list
sumDecreasing :: [Integer] -> Integer
sumDecreasing l = sumDecreasing' l 0
  where sumDecreasing' (x:xs) acc
          | x > 0     = sumDecreasing' xs (x+acc)
          | otherwise = acc

-- computes the full fuel requirements for a given mass
moduleFuelRequirement :: Integer -> Integer
moduleFuelRequirement = sumDecreasing . moduleFuelRequirements

-- gets the mass of a single module from the input
getModuleMass :: IO (Maybe Integer)
getModuleMass = do
  done <- isEOF
  if done
    then return Nothing
    else Just <$> read <$> getLine

-- adds up all fuel requirements for all the mass
totalFuelRequirements :: IO Integer
totalFuelRequirements = addFurtherRequirements 0
  where
    addFurtherRequirements acc =
      getModuleMass >>=
        maybe
          (pure acc)
          (\mass -> addFurtherRequirements $ acc + (moduleFuelRequirement mass))
