module Exercise1 where

import Data.Function (fix)
import Test.QuickCheck

properties :: ([String] -> [[String]]) -> [Property]
properties f = [propModel f]

propModel f = property $ \xs -> not (null xs) ==> f xs == map words xs
