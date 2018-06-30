module Exercises where

import Language.Haskell.Compiler.Helium (AskelleOptions(AskelleOptions))
import Language.Haskell.Syntax (Name(Ident))
import Language.Haskell.Transformations.Transformation(NormOptions(..))

import Common

data Exercise
    = Ex1
    | Ex2
    | Ex3
    | Ex4
    | Ex5
    | Ex6
    | Ex7
    | Ex8
    deriving (Eq, Show)

allExercises :: [Exercise]
allExercises = [Ex1, Ex2, Ex3, Ex4, Ex5, Ex6, Ex7, Ex8]

fnName :: Exercise -> String
fnName Ex1 = "parseTable"
fnName Ex2 = "printLine"
fnName Ex3 = "printField"
fnName Ex4 = "printRow"
fnName Ex5 = "columnWidths"
fnName Ex6 = "printTable"
fnName Ex7 = "select"
fnName Ex8 = "project"

exNumber :: Exercise -> Int
exNumber Ex1 = 1
exNumber Ex2 = 2
exNumber Ex3 = 3
exNumber Ex4 = 4
exNumber Ex5 = 5
exNumber Ex6 = 6
exNumber Ex7 = 7
exNumber Ex8 = 8

subDir :: Exercise -> String
subDir ex = "../fp-practicals-ask-elle/2017-assignment1-lists/exercise" ++ show (exNumber ex)

solDir :: Exercise -> String
solDir ex = "./solutions/" ++ show (exNumber ex)

typeSig :: Exercise -> String
typeSig Ex1 = "[String] -> [[String]]"
typeSig Ex2 = "[Int] -> String"
typeSig Ex3 = "Int -> String -> String"
typeSig Ex4 = "[(Int, String)] -> String"
typeSig Ex5 = "[[String]] -> [Int]"
typeSig Ex6 = "[[String]] -> String"
typeSig Ex7 = "String -> String -> [[String]] -> [[String]]"
typeSig Ex8 = "[String] -> [[String]] -> [[String]]"

options :: Exercise -> AskelleOptions
options Ex5 = AskelleOptions True (imports Ex5)
options ex = toOptions $ imports ex

normOptions :: Exercise -> NormOptions
normOptions ex = NormOptions
    { mainFns = [Ident $ fnName ex ]
    , removeBaseCase = ex `elem` removeBase
    }
    where removeBase = [Ex1, Ex2, Ex4, Ex5, Ex6, Ex7, Ex8]

imports :: Exercise -> [Import]
imports Ex1 = importsCommon
imports Ex2 = imports Ex1
imports Ex3 = ("printLine", "[Int] -> String") : imports Ex2
imports Ex4 = ("printField", "Int -> String -> String") : imports Ex3
imports Ex5 = ("printRow", "[(Int, String)] -> String") : imports Ex4
imports Ex6 = [ ("columnWidths", "[[String]] -> [Int]")
           , ("getColumnWidths", "[[String]] -> [Int]")
           , ("columnWidth", "[[String]] -> [Int]")
           ] ++ imports Ex5
imports Ex7 = imports Ex6
imports Ex8 = imports Ex7

importsCommon :: [Import]
importsCommon =
    [ ("Field", "String")
    , ("Row", "[String]")
    , ("Column", "[String]")
    , ("Table", "[[String]]")
    , ("isDigit", "Char -> Bool")
    , ("isNumber", "Char -> Bool")
    , ("intercalate", "[a] -> [[a]] -> [a]")
    , (">>>", "(a -> b) -> (b -> c) -> (a -> c)")
    , ("transpose", "[[a]] -> [[a]]")
    , ("toUpper", "Char -> Char")
    , ("toLower", "Char -> Char")
    , ("elemIndex", "a -> [a] -> Maybe Int")
    , ("elemIndices", "a -> [a] -> [Int]")
    , ("find", "(a -> Bool) -> [a] -> Maybe a")
    , ("isNothing", "Maybe a -> Bool")
    , ("isJust", "Maybe a -> Bool")
    , ("fromJust", "Maybe a -> a")
    , ("fromMaybe", "a -> Maybe a -> a")
    , ("maybeToList", "Maybe a -> [a]")
    , ("mapMaybe", "(a -> Maybe b) -> [a] -> [b]")
    , ("fromIntegral", "a -> b") -- Helium doesn't support the `Integral` typeclass
    ]
