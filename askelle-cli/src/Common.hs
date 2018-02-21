module Common (
    Import,
    toOptions, loadSources,
    normalize, normalizeM
) where

import Control.Monad.Writer (Writer, runWriter)

import System.Directory (listDirectory)
import System.FilePath ((</>), takeBaseName, splitDirectories)

import Language.Haskell.Compile (safeCompile')
import qualified Language.Haskell.Compiler.Helium as H
import Language.Haskell.Equality (normalise')
import Language.Haskell.Syntax (Module(Module), Body(Body), Name(Ident))
import Language.Haskell.Transformations.Transformation (Transformation)

type Import = (String, String)

toOptions :: [Import] -> H.AskelleOptions
toOptions imports = H.AskelleOptions
                  { H.filterTypeSigs = True
                  , H.imports = imports
                  }

loadSources :: String -> IO [(String, String)]
loadSources path = do
    fileNames <- map (path </>) <$> listDirectory path
    files <- mapM readFile fileNames
    return $ zip fileNames files

normalize :: String -> String -> [Import] -> Either String (Maybe Module)
normalize fname source imports = fst . runWriter <$> normalizeM fname source imports

-- Note: we treat programs that normalize to nothing as compile errors
normalizeM :: String -> String -> [Import] -> Either String (Writer [Transformation] (Maybe Module))
normalizeM fname source imports =
    fmap filterEmptyModule <$> normalise' [Ident fname] <$> safeCompile' source (toOptions imports)
    where
        isEmpty (Module _ (Body [])) = True
        isEmpty _ = False
        filterEmptyModule :: Module -> Maybe Module
        filterEmptyModule m = if isEmpty m then Nothing else Just m
