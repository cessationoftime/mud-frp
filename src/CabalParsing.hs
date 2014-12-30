-----------------------------------------------------------------------------
--
-- Module      :  CabalParsing
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module CabalParsing where
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version
import Distribution.ModuleName
import Data.Maybe
import Data.List (nub)
import Control.Applicative ((<$>))
ghc = [7,6,3]

data CabalPackage = CabalPackage {
   packageName :: String
  ,packageVersion :: String
  ,packageDescription :: String
  ,packageDepends :: [String]
  ,packageModules :: [ModuleName]
} deriving (Show,Eq)

parseCabalPackage :: FilePath -> IO CabalPackage
parseCabalPackage file = do
  pkg <- readPackageDescription silent file
  finalPkg <- return $ finalizePD pkg
  return $ CabalPackage
    (display $ pkgName $ package finalPkg)
    (display $ pkgVersion $ package finalPkg)
    (description finalPkg)
    [display pn | Just l <- [library finalPkg], Dependency pn _ <- targetBuildDepends $ libBuildInfo l]
    (getModules finalPkg)
  where
  finalizePD pkg = case finalizePackageDescription [] (const True) (Platform I386 Linux) (CompilerId GHC (Version ghc [])) [] pkg of
    Left _ -> flattenPackageDescription pkg
    Right (pkg,_) -> pkg

getModules :: PackageDescription -> [ModuleName]
getModules pd =
  let libM = concat $ libModules <$> (maybeToList $ library pd) :: [ModuleName]
      exeM = concat $ exeModules <$> executables pd :: [ModuleName]
  in nub $ libM ++ exeM




