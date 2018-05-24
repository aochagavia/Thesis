module Common (
    Import,
    toOptions, loadSources,
    normalize, normalizeM
) where

import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)

import Language.Haskell.Compile (safeCompile')
import qualified Language.Haskell.Compiler.Helium as H
import qualified Language.Haskell.Equality as E
import Language.Haskell.Syntax (Module(Module), Body(Body), Name)
import Language.Haskell.Transformations.Transformation (NormPass, NormOptions(..), execNorm)

type Import = (String, String)

toOptions :: [Import] -> H.AskelleOptions
toOptions imports = H.AskelleOptions
                  { H.filterTypeSigs = False
                  , H.imports = imports
                  }

loadSources :: String -> IO [(String, String)]
loadSources path = do
    fileNames <- map (path </>) <$> listDirectory path
    files <- mapM readFile $ filter (\f -> takeExtension f == ".hs") fileNames
    return $ zip fileNames files

normalize :: NormOptions -> String -> H.AskelleOptions -> Either String (Maybe Module)
normalize opts@(NormOptions { mainFns }) source options = execNorm opts <$> normalizeM mainFns source options

-- Note: we treat programs that normalize to nothing as compile errors
normalizeM :: [Name] -> String -> H.AskelleOptions -> Either String (NormPass (Maybe Module))
normalizeM mainFns source options =
    fmap filterEmptyModule <$> E.normalize' mainFns <$> safeCompile' source options
    where
        isEmpty (Module _ (Body [])) = True
        isEmpty _ = False
        filterEmptyModule :: Module -> Maybe Module
        filterEmptyModule m = if isEmpty m then Nothing else Just m
