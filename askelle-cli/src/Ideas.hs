{-# OPTIONS -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Ideas where

-- Experiments around the integration of the IDEAS framework and Ask-Elle

import Ideas.Common.Library (
    Strategy, Context, Exercise, Prefix
    , describe, equivalence, label, newId, emptyPath, inContext, prefixPaths)
import qualified Ideas.Common.Library as I
import qualified Ideas.Service.Diagnose as D
import Ideas.Service.State (State, firsts, exercise, stateTerm, stateContext)
import Control.Monad (forever)
import Control.Monad.Identity (runIdentity)
import Data.Maybe (fromJust)
import Data.List (unfoldr)
import Language.Haskell.Compile (typeOf, compile', safeCompile')
import Language.Haskell.Exercises (hsAlternatives, solutions, hsDescribe, hsExercise, hsStrategy, fromHsStrategy)
import Language.Haskell.PrettyPrint (pprint)
import Language.Haskell.Syntax (Module(Module), Name(Ident))
import Language.Haskell.Transformations (removeHoles)
import Service.Utils (pathsToState, replayState)
import Service.AdaptedServices (allfirsts, onefirst)
import Service.DeepDiagnose (matchingPrefixes)
import qualified Service.ExtraServices as ES
import ScriptAnalysis (generateScript)

import Common

initialize :: String -> [String] -> [Import] -> (State Module, Exercise Module)
initialize fnName solutions imports =
    let ex = mkExercise fnName solutions imports
        s  = fromJust $ pathsToState [emptyPath] ex
    in (s, ex)

-- Note: these exercises have no config
mkExercise :: String -> [String] ->  [Import] -> Exercise Module
mkExercise fnName solutions imports =
    let desc  = "Exercise has no description"
        eId   = describe desc $ newId $ "<nameless-exercise>"
        strat = mkStrategy (map (\s -> compile' s (toOptions imports)) solutions)
        tp    = runIdentity $ typeOf fnName $ head solutions
    in hsExercise (Ident fnName) tp "<dummy-path>" eId (describe desc $ label fnName strat)


-- | Generate a strategy from a Haskell source file
mkStrategy :: [Module] -> Strategy (Context Module)
mkStrategy solutions =
    let desc    = "Strategy has no description"
        name    = "<nameless-strategy>"
        modelId = newId $ name
        hss     = map (hsDescribe desc modelId . hsStrategy) solutions
    in fromHsStrategy [Ident name] (hsAlternatives hss)

printExerciseSteps :: String -> String -> IO ()
printExerciseSteps fnName model = printSteps $ fst $ initialize fnName [model] []

printSteps s = do
    putStrLn . pprint . stateTerm $ s
    case allfirsts s of
        Left msg -> putStrLn msg
        Right [] -> return ()
        Right ((info, s', env):_) -> do
            -- Info tells us about the rule that was used!
            putStrLn $ show info
            putStrLn $ show env
            printSteps s'

ideasCheckStrategy fnName model submission =
    -- Init model solution
    let (state, ex) = initialize fnName [model] []
    -- Compile submission
        (Right submissionModule) = safeCompile' submission (toOptions [])
    -- Put it into context
    -- Is this the right thing to do?
        ctx = inContext ex submissionModule
    -- Replay state
        state' = fromJust $ replayState state
        ex' = exercise state'
        --ctx' = stateContext state'
    -- Calculate paths?
        (expPfx, done) = matchingPrefixes ex' ctx
        --
        paths = prefixPaths expPfx
    in (expPfx, done)

parseProg :: IO String
parseProg = do
    line <- getLine
    if line == "END"
        then return ""
        else ((line ++ "\n") ++) <$> parseProg

removeHoles' = do
    forever $ do
        source <- parseProg
        let Right submission = safeCompile' source (toOptions [])
        putStrLn $ pprint $ removeHoles submission
        putStrLn $ show submission
        putStrLn $ show $ removeHoles submission

askelleCli = do
    --putStrLn "Enter the model solution"
    let fnName = "double"
    let model = unlines ["double = map (* 2)"]
    --let model = "parseTable = foldr (\\x acc -> words x : acc)"
    --let model2 = "parseTable xs = map words xs"
    let (initState, exercise) = initialize fnName [model] []
    let script = generateScript exercise fnName
    let rules = I.ruleset exercise
    putStrLn $ show rules
    putStrLn $ show initState
    putStrLn $ unlines $ map pprint (solutions exercise)
    forever $ do
        source <- getLine
        let Right submission = safeCompile' source (toOptions [])
        --putStrLn $ "Ready: " ++ show (I.isReady exercise submission)
        let sCtx = inContext exercise submission
        --let newCtxs = map (\r -> I.applyAll r sCtx) rules
        --putStrLn $ unlines . map show $ newCtxs
        --putStrLn $ show sCtx
        --putStrLn $ I.prettyPrinterContext exercise sCtx
        let (_, msg, newState, _) = ES.feedbacktextdeep script initState sCtx
        --putStrLn $ show msg
        let nextSteps = map (("- " ++) . show . fst) $ ES.allfirststext script newState Nothing
        if null nextSteps
            then putStrLn "Finished"
            else putStrLn "Possible next steps:" >> putStrLn (unlines nextSteps)
    --putStrLn $ show $ D.diagnose initState sCtx Nothing
