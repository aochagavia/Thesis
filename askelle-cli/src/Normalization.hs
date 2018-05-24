module Normalization where

-- Experiments around normalization in Ask-Elle

import Prelude hiding (mod)

import Language.Haskell.Compile (safeCompile')
import Language.Haskell.Compiler.Helium (AskelleOptions)
import qualified Language.Haskell.Compiler.Helium as H
import Language.Haskell.PrettyPrint (pprint)
import Language.Haskell.Syntax (Module)
import Language.Haskell.Transformations.Transformation

import Data.Either (partitionEithers)
import Data.List (partition, sortBy, sort)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import System.FilePath (takeBaseName)
--import System.Directory (listDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Common
import Correctness
import qualified Exercises as Ex

main = undefined

-- Misc
mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
sortWithDesc f = sortBy (\x y -> compare (f y) (f x))

--
-- Testing
--

-- Note that this will not run anything, since it returns a list of IO
testAll = map normEx Ex.allExercises

--
-- Utilities to normalize single programs
--

-- normProg Ex.Ex2 "123456"
normProg :: Ex.Exercise -> String -> IO ()
normProg ex fileName = do
    source <- readFile $ Ex.subDir ex ++ "/" ++ fileName ++ ".hs"
    let normOpts = Ex.normOptions ex
        askelleOpts = Ex.options ex
        (normalized, transformations) = runNormIgnore normOpts $ normalizePprint normOpts askelleOpts source
    let Right original = safeCompile' source askelleOpts
    putStrLn $ unlines ["Original:", pprint original]
    putStrLn $ unlines $ map show $ transformations
    putStrLn $ unlines $ ["Normalized:", normalized]

-- Like normProg, but checks whether the program matches an existing solution
normProg' :: Ex.Exercise -> String -> IO ()
normProg' ex fileName = do
    source <- readFile $ Ex.subDir ex ++ "/" ++ fileName ++ ".hs"
    let normOpts = Ex.normOptions ex
        askelleOpts = Ex.options ex
        (normalized, _) = runNormIgnore normOpts $ normalizeMod normOpts askelleOpts source
    --putStrLn $ unlines $ map show $ transformations
    --putStrLn $ normalized
    (modelSolutions, []) <- mapFst (fmap head) <$> loadAndNormalize normOpts (Ex.solDir ex) askelleOpts
    case Map.lookup normalized modelSolutions of
        Nothing -> putStrLn $ unlines ["Matches no model solution", pprint normalized]
        Just x -> putStrLn $ "Matches model solution " ++ x

normSol :: Ex.Exercise -> String -> IO ()
normSol ex fileName = do
    source <- readFile $ Ex.solDir ex ++ "/" ++ fileName ++ ".hs"
    let normOpts = Ex.normOptions ex
        askelleOpts = Ex.options ex
        (normalized, transformations) = runNormIgnore normOpts $ normalizePprint normOpts askelleOpts source
    putStrLn $ unlines $ map show $ transformations
    putStrLn $ normalized

normSource :: Ex.Exercise -> String -> IO ()
normSource ex source = do
    let (normOpts, askelleOpts) = (Ex.normOptions ex, Ex.options ex)
    let (normalized, transformations) = runNormIgnore normOpts $ normalizePprint normOpts askelleOpts source
    putStrLn $ unlines $ map show $ transformations
    putStrLn $ normalized

--
-- Normalization helpers
--

-- Normalize a program and return it as a pretty printed string
normalizePprint :: NormOptions -> AskelleOptions -> String -> NormPass String
normalizePprint (NormOptions { mainFns }) opts p =
    case normalizeM mainFns p opts of
        Right p' -> pprintM <$> p'
        Left e -> error $ "Something went wrong: " ++ e
    where
        pprintM (Just m) = unlines [pprint m, show m]
        pprintM Nothing = "Program was normalized away"

-- Normalize a program and return it as a module
normalizeMod :: NormOptions -> AskelleOptions -> String -> NormPass Module
normalizeMod (NormOptions { mainFns }) opts p =
    case normalizeM mainFns p opts of
        Right p' -> fromMaybe (error "Program was normalized away") <$> p'
        Left e -> error $ "Something went wrong: " ++ e

-- Runs a normalization, filtering out some transformations
runNormIgnore :: NormOptions -> NormPass a -> (a, [Transformation])
runNormIgnore opts norm =
    let (normalized, transformations) = runNorm opts norm
    in (normalized, filter (not . ignore) transformations)
    where
        ignore (DesugarExpr {}) = True
        ignore (RemovePatParens {}) = True
        ignore _ = False

--
-- Normalize and group whole exercises
--

-- Normalize all exercises
normAll = mapM_ f Ex.allExercises
    where
        f ex = do
            putStrLn $ "=========="
            putStrLn $ "Exercise " ++ show (Ex.exNumber ex)
            putStrLn $ "=========="
            normEx ex

-- Normalize a single exercise
normEx :: Ex.Exercise -> IO ()
normEx ex = do
    let opts = Ex.normOptions ex
        solDir = Ex.solDir ex
        subDir = Ex.subDir ex
        --imports = Ex.imports ex
        options = Ex.options ex
    -- Load and normalize model solutions
    (modelSolutions, solErrors) <- mapFst (fmap head) <$> loadAndNormalize opts solDir options
    if not (null solErrors)
        then do
            putStrLn "The following model solutions could not be compiled:"
            mapM_ (\(fname, err) -> (putStrLn $ "* " ++ takeBaseName fname) >> putStrLn err) solErrors
            error "Cannot continue unless all model solutions compile"
        else return ()

    -- Load and normalize submissions
    (submissions, subErrors) <- loadAndNormalize opts subDir options

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
    if compileError > 0 then do
        putStrLn "ERRORS"
        putStrLn "======"
        mapM_ (\(fname, err) -> putStrLn $ "* " ++ takeBaseName fname ++ ": " ++ err) subErrors
    else return ()

    --checkPrograms ex submissions

    -- Report recognized submissions, grouped by model solution
    putStrLn "Below follows a list of submissions categorized by group"
    putStrLn "========================================================"

    --putStrLn $ "Group sizes: " ++ show (sortWithDesc id $ map length $ Map.elems submissions)

    -- Generate text file with the grouped student answers, for data analysis
    -- Take the name of the exercise as name of the file

    {-
    let dataPath = "../data/round3/" ++ last (splitDirectories subDir)
    let groups = sortWithDesc length $ Map.elems submissions -- :: [[_]]
    writeFile dataPath $ unlines $ map (unlines . map takeBaseName) groups
    -}

    -- Report the groups corresponding to the model solutions
    mapM_ (showSolution modelSolutions) (sortWithDesc (length . snd) $ Map.toList recognized)
    -- Report the rest of the groups
    let (singletons, rest) = partition (\(_, xs) -> length xs == 1) $ (sortWithDesc (length . snd) $ Map.toList unrecognized)
    mapM_ (showSolution modelSolutions) rest

    putStrLn $ "Singleton groups (" ++ show (length singletons) ++ "):"
    let singletonNames = map (takeBaseName . head . snd ) singletons
    mapM_ (\name -> putStrLn $ "* " ++ name) $ sort singletonNames

    where
        showSolution :: Map Module String -> (Module, [String]) -> IO ()
        showSolution modelSolutions (m, files) = do
            case Map.lookup m modelSolutions of
                Just s -> putStr $ "Model solution `" ++ takeBaseName s ++ "` "
                Nothing -> putStr $ "Unnamed group "
            putStrLn $ "(" ++ show (length files) ++ "):"
            mapM_ (\name -> putStrLn $ "* " ++ takeBaseName name) files
        --

-- Like `loadSources`, but normalizes and groups the files
loadAndNormalize :: NormOptions -> String -> H.AskelleOptions -> IO (Map Module [String], [(String, String)])
loadAndNormalize opts path options = do
    sources <- loadSources path
    let (errors, normalized) = partitionEithers $ map normaliseTup sources
    let maps = map (\(fname, mod) -> Map.singleton mod [fname]) normalized
    return $ (Map.unionsWith (++) maps, errors)
    where
        normaliseTup :: (String, String) -> Either (String, String) (String, Module)
        normaliseTup (fname, source) = case normalize opts source options of
            Left err -> Left (fname, err)
            Right Nothing  -> Left (fname, "Program normalized to nothing")
            Right (Just m) -> unsafePerformIO $ do
                --putStrLn $ "Normalized: " ++ fname
                return $ Right (fname, m)
