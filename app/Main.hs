module Main where

import Lib

main :: IO ()
main = (show <$> totalFuelRequirements) >>= putStrLn
