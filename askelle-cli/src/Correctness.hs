module Correctness where

import Data.Either (partitionEithers)
import Data.List (intercalate, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

import System.IO.Unsafe
import System.FilePath (takeBaseName)

import Test.QuickCheck

import Language.Haskell.Compile (safeCompile')
import Language.Haskell.Syntax
import Language.Haskell.PrettyPrint (pprint)

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

import qualified Exercises as Ex

import Debug.Trace

data CheckError = CheckError
    { source :: String
    , normalized :: Module
    , message :: String }

instance Show CheckError where
    show (CheckError s _ msg) = unlines $
        [ s ++ ":"
        , msg
        ]

checkPrograms :: Ex.Exercise -> Map Module [String] -> IO ()
checkPrograms ex clusters = do
    {-
    let progs = map buildProgram $ Map.toList clusters

    -- Ensure that the programs still compile
    let (errors, ok) = partitionEithers $ map (compileProgram (Ex.options ex)) progs
    if not (null errors)
        then do
            putStrLn $ "Errors found: "
            putStrLn $ unlines errors
        else
            putStrLn "All normalized programs compile!"
    -}

    -- Test each program against QuickCheck properties
    -- Note: we need to add imports for the necessary functions in the ExerciseX.hs file
    let (qcErrors, _) = partitionEithers $ map (\(m, s:_) -> runTest ex (takeBaseName s) m) $ Map.toList clusters
    if not (null qcErrors)
        then do
            putStrLn $ "QC errors found: " ++ (show (length qcErrors))
            let sortedErrors = sortBy (\e e' -> compare (source e) (source e')) qcErrors
            mapM_ (\f -> putStrLn $ "* " ++ source f) sortedErrors
            putStrLn $ "\nDetails:\n"
            mapM_ (putStrLn . show) sortedErrors
        else
            putStrLn "All QuickCheck tests pass"

    where
        buildProgram (norm, sources) =
            let header = "-- " ++ intercalate ", " sources
                body = pprint norm
            in unlines [header, body]
        compileProgram options prog =
            case safeCompile' prog options of
                Left e -> Left (prog ++ "\n" ++ e)
                r -> r

-- This function will tell us whether the exercise configuration is all right
-- It is useful to run it before checkPrograms
testExConfigs = map (\ex -> testEx ex $ Ex.fnName ex ++ " = undefined") Ex.allExercises

testEx :: Ex.Exercise -> String -> Either CheckError ()
testEx ex s =
    let Right m = safeCompile' s (Ex.options ex)
    in runTest ex "<unknown>" m

runTest :: Ex.Exercise -> String -> Module -> Either CheckError ()
runTest ex source m = unsafePerformIO $ do
    result <- run (testInterpreted ex m)
    case result of
        Left e -> return $ Left $ CheckError source m $ show e
        Right r -> r >>= \r' -> case r' of
            GaveUp  {} -> return $ Left $ CheckError source m "Gave up"
            Success {} -> return $ Right ()
            Failure {output = txt} -> return $ Left $ CheckError source m txt
            _ -> return $ Left $ CheckError source m "Dont know"

testInterpreted :: Ex.Exercise -> Module -> Interpreter (IO Result)
testInterpreted e m = do
    loadModules [ "./qc-properties/Exercise" ++ show (Ex.exNumber e) ++ ".hs"
                , "./qc-properties/SampleTables.hs"]
    setTopLevelModules ["Exercise" ++ show (Ex.exNumber e)]
    setImports ["Prelude", "Test.QuickCheck", "Data.Char", "Data.Maybe", "Data.List", "SampleTables"]
    interpret (testString m (Ex.fnName e)) (as :: IO Result)

testString :: Module -> String -> String
testString (Module _ (Body ds)) fun =
    let asd = pprint $ Let ds {-in-}  $ app "quickCheckWithResult"
                    [ Var (Ident "stdArgs {chatty = False}")  -- misusing an identifier here...
                    , Paren $ app "within"
                    [ Lit (LInt 3000000)
                    , Paren $ app "conjoin"
                    [ Paren $ app "properties" [v fun] ] ] ]
    in asd --trace asd asd
    where
    v     = Var . Ident
    app f = App (v f)
testString _ _ = error "Cannot test empty module!"

run :: InterpreterT IO a -> IO (Either InterpreterError a)
run = unsafeRunInterpreterWithArgs ["-fno-warn-unrecognised-pragmas"]
