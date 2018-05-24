module Exercise2 where

import Data.Function (fix)
import Data.List (intercalate)
import Test.QuickCheck

properties :: ([Int] -> String) -> [Property]
properties f = [propModel f]

model :: [Int] -> String
model xs = "+" ++ intercalate "+" (map (flip replicate '-' ) xs) ++ "+"

-- Input is a non-empty list of positive integers
propModel f = property $ \ps -> not (null ps) ==> (let xs = map getPositive ps in f xs == model xs)
