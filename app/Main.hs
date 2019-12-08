module Main where

import Lib

main :: IO ()
main = (show <$> totalFuelRequirement) >>= putStrLn
