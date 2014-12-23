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


ghc = [7,6,3]

data CabalPackage = CabalPackage {
   packageName :: String
  ,packageVersion :: String
  ,packageDescription :: String
  ,packageDepends :: [String]
} deriving Show


parse :: FilePath -> IO CabalPackage
parse file = do
  pkg <- readPackageDescription silent file
  finalPkg <- return $ finalizePD pkg
  return $ Cabal
  (display $ pkgName $ package finalPkg)
  (display $ pkgVersion $ package finalPkg)
  (description finalPkg)
  [display pn | Just l <- [library finalPkg], Dependency pn _ <- targetBuildDepends $ libBuildInfo l]
  where
  finalizePD pkg = case finalizePackageDescription [] (const True) (Platform I386 Linux) (CompilerId GHC (Version ghc [])) [] pkg of
    Left _ -> flattenPackageDescription pkg
    Right (pkg,_) -> pkg
