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

module CabalParsing (moduleFiles,getCabalBuildInfos, CabalBuildInfo,OpResult) where
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
--import Data.Aeson
import Control.Monad.State
import Data.List (nub)
import Control.Applicative ((<$>))
import Language.Haskell.BuildWrapper.Cabal (CabalBuildInfo)
import Language.Haskell.BuildWrapper.Base (Verbosity(..),BuildWrapperState(..), WhichCabal(..),OpResult,BuildWrapper)
import qualified Language.Haskell.BuildWrapper.Cabal as BW
ghc = [7,6,3]


--type OpResult a=(a,[BWNote])
--type BuildWrapper=StateT BuildWrapperState IO

moduleFiles :: [CabalBuildInfo] -> [FilePath]
moduleFiles buildInfos = nub $ concatMap (map snd . BW.cbiModulePaths) buildInfos

-- | given a cabalFilePath get the buildInfos for the cabal file
getCabalBuildInfos :: FilePath -> IO (OpResult [CabalBuildInfo])
getCabalBuildInfos cabalFile = do
  let tempFolder = ".dist-buildwrapper"
      cabalPath = "cabal"
--      cabalFile = "/home/cvanvranken/Documents/leksahWorkspace/WXDiffCtrl-0.0.1/WXDiffCtrl.cabal"
      verbosity = Normal
      cabalFlags = ""
      cabalOption = []
      logCabal = True

  runCmd (BuildWrapperState tempFolder cabalPath cabalFile verbosity cabalFlags cabalOption logCabal) cabalBuildInfos


-- | BuildWrapperState represents a command, take an initial state and evaluate a BuildWrapper function
runCmd:: BuildWrapperState -> BuildWrapper a -> IO a
runCmd initialState bwFunc= do
  resultJson <- evalStateT bwFunc initialState
  return resultJson

-- get build info for each component
cabalBuildInfos :: BuildWrapper (OpResult [CabalBuildInfo])
cabalBuildInfos = do
  (mBuildInfos,bwns)<- BW.withCabal Source BW.getAllFiles
  return $ case mBuildInfos of
    Just buildInfos->(buildInfos,bwns)
    Nothing ->([],bwns);

--get the files of all modules
getModuleFiles :: BuildWrapper (OpResult [FilePath])
getModuleFiles = BW.getFilesToCopy

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


buildInfos :: PackageDescription -> [BuildInfo]
buildInfos pd =
    let libM = libBuildInfo <$> (maybeToList $ library pd) :: [BuildInfo]
        exeM = buildInfo <$> executables pd :: [BuildInfo]
    in libM ++ exeM




