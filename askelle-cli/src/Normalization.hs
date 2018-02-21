{-# OPTIONS -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Normalization where

-- Experiments around normalization in Ask-Elle

import Prelude hiding (mod)

import Language.Haskell.Compile (safeCompile')
import Language.Haskell.Compiler.Helium (AskelleOptions)
import qualified Language.Haskell.Compiler.Helium as H
import Language.Haskell.Equality (normalise')
import Language.Haskell.PrettyPrint (pprint)
import Language.Haskell.Syntax (Name(Ident), Module(Module), Body(Body))
import Language.Haskell.Transformations.Transformation

import Control.Monad.Writer (Writer, runWriter)
import Data.Either (partitionEithers)
import Data.List (partition, sortBy, intercalate)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import System.FilePath ((</>), takeBaseName, splitDirectories) -- splitDirectories is necessary for data processing with R
import System.Directory (listDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Common

main = undefined

-- Misc
mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
sortWithDesc f = sortBy (\x y -> compare (f y) (f x))

data MyDiag = Match
            | Unrecognized
            | CompileError String
            | NormalizedAway
            | UnknownError
            deriving Eq

instance Show MyDiag where
    show Match = "MATCH"
    show Unrecognized = "NO MATCH"
    show (CompileError e) = "COMPILE ERROR: " ++ e
    show NormalizedAway = "PROGRAM WAS NORMALIZED AWAY"
    show UnknownError = "UNKNOWN ERROR"

debugFileDiagPair :: (FilePath, MyDiag) -> (FilePath, MyDiag)
debugFileDiagPair pair@(file, diagnosis) = unsafePerformIO $ do
    putStrLn $ file ++ " - " ++ (show diagnosis)
    return pair

--
-- Testing
--

importsCommon :: [Import]
importsCommon =
        [ ("isDigit", "Char -> Bool")
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

imports1 = importsCommon
imports2 = imports1
imports3 = ("printLine", "[Int] -> String") : imports2
imports4 = ("printField", "Int -> String -> String") : imports3
imports5 = ("printRow", "[(Int, String)] -> String") : imports4
imports6 = [ ("columnWidths", "[[String]] -> [Int]")
           , ("getColumnWidths", "[[String]] -> [Int]")
           , ("columnWidth", "[[String]] -> [Int]")
           ] ++ imports5
imports7 = imports6
imports8 = imports7

allExercises =
    [ ("parseTable", "../solutions/1", "../fp-practicals-ask-elle/2017-assignment1-lists/exercise1", imports1)
    , ("printLine", "../solutions/2", "../fp-practicals-ask-elle/2017-assignment1-lists/exercise2", imports2)
    , ("", "../solutions/3", "../fp-practicals-ask-elle/2017-assignment1-lists/exercise3", imports3)
    , ("", "../solutions/4", "../fp-practicals-ask-elle/2017-assignment1-lists/exercise4", imports4)
    , ("", "../solutions/5", "../fp-practicals-ask-elle/2017-assignment1-lists/exercise5", imports5)
    , ("", "../solutions/6", "../fp-practicals-ask-elle/2017-assignment1-lists/exercise6", imports6)
    , ("", "../solutions/7", "../fp-practicals-ask-elle/2017-assignment1-lists/exercise7", imports7)
    , ("", "../solutions/8", "../fp-practicals-ask-elle/2017-assignment1-lists/exercise8", imports8)]

-- Tests exercise number x
test :: Int -> IO ()
test x = testAll !! (x - 1)

-- Note that this will not run anything, since it returns a list of IO
testAll = map testExercise allExercises
    where
        testExercise (funcName, solPath, subPath, imports) = normalizeAndGroup funcName solPath subPath imports

prog :: String
prog = "double xs = map (* 2) xs"

prog2 = "double [] = []\ndouble (x:xs) = (x * 2) : double xs"

normProg :: String -> String -> IO ()
normProg fname source = do
    let (normalized, transformations) = runWriter $ normalizeProgram' fname source
    putStrLn $ unlines $ map show transformations
    putStrLn $ normalized

normalizeProgram :: String -> String -> String
normalizeProgram fname = fst . runWriter . normalizeProgram' fname

normalizeProgram' :: String -> String -> Writer [Transformation] String
normalizeProgram' fname p =
    let Right p' = normalizeM fname p []
    in  pprintM <$> p'
    where
        pprintM (Just m) = pprint m
        pprintM Nothing = "Program was normalized away"

--
-- Normalize and group submissions
--

normalizeAndGroupAll = mapM_ f (zip allExercises [1..])
    where
        f ((a, b, c, d), i) = do
            putStrLn $ "=========="
            putStrLn $ "Exercise " ++ show i
            putStrLn $ "=========="
            normalizeAndGroup a b c d


normalizeAndGroup :: String -> String -> String -> [Import] -> IO ()
normalizeAndGroup funcName solDir subDir imports = do
    -- Load and normalize model solutions
    (modelSolutions, solErrors) <- mapFst (fmap head) <$> loadAndNormalize funcName solDir imports
    if not (null solErrors)
        then do
            putStrLn "The following model solutions could not be compiled:"
            mapM_ (\(fname, err) -> (putStrLn $ "* " ++ takeBaseName fname) >> putStrLn err) solErrors
            error "Cannot continue unless all model solutions compile"
        else return ()

    -- Load and normalize submissions
    (submissions, subErrors) <- loadAndNormalize funcName subDir imports

    -- Split submissions (so we can report first all recognized groups and then the rest)
    let recognized = Map.intersection submissions modelSolutions
    let unrecognized = Map.difference submissions modelSolutions

    -- General stats
    let compileSuccess = sum $ map length (Map.elems submissions)
    let compileError = length subErrors
    let recognizedSubmissions = sum (map length (Map.elems recognized))
    let unrecognizedSubmissions = compileSuccess - recognizedSubmissions
    putStrLn $ "Valid submissions: " ++ show compileSuccess ++ " (" ++ show compileError ++ " not compiling)"
    putStrLn $ "Recognized submissions: " ++ show recognizedSubmissions
    putStrLn $ "Unrecognized submissions: " ++ show unrecognizedSubmissions
    putStrLn $ "Number of groups: " ++ show (Map.size submissions)
    putStrLn ""

    -- Report recognized submissions, grouped by model solution
    putStrLn "Below follows a list of submissions categorized by group"
    putStrLn "========================================================"

    --putStrLn $ "Group sizes: " ++ show (sortWithDesc id $ map length $ Map.elems submissions)

    -- Generate text file with the grouped student answers, for data analysis
    -- Take the name of the exercise as name of the file
    {-
    let freqFilePath = "../data/baseline/" ++ last (splitDirectories subDir)
    let groups = sortWithDesc length $ Map.elems submissions -- :: [[_]]
    writeFile freqFilePath $ unlines $ map (unlines . map takeBaseName) groups
    writeFile (freqFilePath ++ ".freqs") $ unlines $ map (show . length) groups
    writeFile (freqFilePath ++ ".buckets") $ intercalate " "  $ map show $ bucketize $ map length groups
    -}

    -- Report the groups corresponding to the model solutions
    mapM_ (showSolution modelSolutions) (sortWithDesc (length . snd) $ Map.toList recognized)
    -- Report the rest of the groups
    mapM_ (showSolution modelSolutions) (sortWithDesc (length . snd) $ Map.toList unrecognized)
    -- Report errors
    if compileError > 0 then do
        putStrLn "ERRORS"
        putStrLn "======"
        mapM_ (\(fname, err) -> putStrLn $ "* " ++ takeBaseName fname ++ ": " ++ err) subErrors
    else return ()

    where
        showSolution :: Map Module String -> (Module, [String]) -> IO ()
        showSolution modelSolutions (m, files) = do
            case Map.lookup m modelSolutions of
                Just s -> putStr $ "Model solution `" ++ takeBaseName s ++ "` "
                Nothing -> putStr $ "Unnamed group "
            putStrLn $ "(" ++ show (length files) ++ "):"
            mapM_ (\name -> putStrLn $ "* " ++ takeBaseName name) files
        --
        {-
        bucketize :: [Int] -> [Int]
        bucketize xs = let b1 = bucket (== 1)
                           b2 = bucket (== 2)
                           b3 = bucket (\x -> 2 < x && x <= 4)
                           b4 = bucket (\x -> 4 < x && x <= 8)
                           b5 = bucket (\x -> 8 < x && x <= 16)
                           b6 = bucket (\x -> 16 < x && x <= 32)
                           b7 = bucket (\x -> 32 < x && x <= 64)
                           b8 = bucket (\x -> 64 < x)
                       in  [b1, b2, b3, b4, b5, b6, b7, b8]
            where
                bucket :: (Int -> Bool) -> Int
                bucket f = length $ filter f xs
        -}

-- Like `loadSources`, but normalizes and groups the files
loadAndNormalize :: String -> String -> [Import] -> IO (Map Module [String], [(String, String)])
loadAndNormalize funcName path imports = do
    sources <- loadSources path
    let (errors, normalized) = partitionEithers $ map normaliseTup sources
    let maps = map (\(fname, mod) -> Map.singleton mod [fname]) normalized
    return $ (Map.unionsWith (++) maps, errors)
    where
        normaliseTup :: (String, String) -> Either (String, String) (String, Module)
        normaliseTup (fname, source) = case normalize funcName source imports of
                                            Left err -> Left (fname, err)
                                            Right Nothing  -> Left (fname, "Program normalized to nothing")
                                            Right (Just m) -> Right (fname, m)

-- Similar to `diag`, but also does reporting
reportDiag :: String -> String -> String -> [Import] -> IO ()
reportDiag funcName solutionsPath submissionsPath imports = do
    -- NOTE: we could compile stuff here instead of deferring that to the DiagStrategy
    fileDiagPairs <- diag funcName solutionsPath submissionsPath imports
    -- Show progress (useful to find programs that trigger an infinite loop)
    --let fileDiagPairs = map debugFileDiagPair fileDiagPairs'
    let (expected, notExpected) = partition (isExpected . snd) fileDiagPairs
        (unrecognized, compileError) = partition (isUnrecognized . snd) notExpected

    -- Report results
    putStrLn $ "Files: " ++ show (length expected + length notExpected)
    putStrLn $ "Matched: " ++ show (length expected)

    -- Show rejected programs that don't trigger compile errors
    putStrLn $ "Unrecognized: " ++ show (length unrecognized)
    mapM_ (reportFile . fst) unrecognized

    -- Show programs that fail to compile
    putStrLn $ "Compile error: " ++ show (length compileError)
    mapM_ (\(fname, diagnosis) -> do reportFile fname; putStrLn (show diagnosis)) compileError

    where
        reportFile fname = putStrLn $ "* " ++ takeBaseName fname
        -- Match = recognized by the model
        isExpected :: MyDiag -> Bool
        isExpected Match = True
        isExpected _ = False
        -- Unexpected = compiles correctly but doesn't match the model
        isUnrecognized :: MyDiag -> Bool
        isUnrecognized Unrecognized = True
        isUnrecognized _ = False

-- Check all submissions in `submissionsPath` against the solutions in `solutionsPath`
diag :: String -> String -> String -> [Import] -> IO [(FilePath, MyDiag)]
diag funcName solutionsPath submissionsPath imports = do
    solutionSources <- map snd <$> loadSources solutionsPath
    submissionSources <- loadSources submissionsPath

    -- Normalize model solutions
    let Right solutionsM = mapM (\s -> normalize funcName s imports) solutionSources
    let Just solutions = sequence solutionsM

    -- Normalize and check submissions
    return $ map (mapSnd (check solutions . (\s -> normalize funcName s imports))) submissionSources

    where
        check :: [Module] -> Either String (Maybe Module) -> MyDiag
        check _         (Left e) = CompileError e
        check _         (Right Nothing) = NormalizedAway
        check solutions (Right (Just m)) = if m `elem` solutions then Match else Unrecognized
