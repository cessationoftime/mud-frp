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

module CabalParsing (moduleFiles,getCabalBuildInfos,getCabalBuildInfosAsync, CabalBuildInfo,OpResult, newCabalLock, CabalLock) where
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
import Control.Concurrent
import Control.Exception.Base (SomeException)
import qualified Control.Concurrent.Lock as L
import Data.Either
ghc = [7,6,3]

newtype CabalLock = CabalLock { unCabLock :: L.Lock }

newCabalLock :: IO CabalLock
newCabalLock = CabalLock <$> L.new

--type OpResult a=(a,[BWNote])
--type BuildWrapper=StateT BuildWrapperState IO

moduleFiles :: [CabalBuildInfo] -> [FilePath]
moduleFiles buildInfos = nub $ concatMap (map snd . BW.cbiModulePaths) buildInfos

-- | given a cabalFilePath get the buildInfos for the cabal file
getCabalBuildInfos :: (?cl :: CabalLock) =>
   FilePath -> IO (OpResult [CabalBuildInfo])
getCabalBuildInfos cabalFile = do
  let tempFolder = ".dist-buildwrapper"
      cabalPath = "cabal"
--      cabalFile = "/home/cvanvranken/Documents/leksahWorkspace/WXDiffCtrl-0.0.1/WXDiffCtrl.cabal"
      verbosity = Normal
      cabalFlags = ""
      cabalOption = []
      logCabal = True

  runCmd (BuildWrapperState tempFolder cabalPath cabalFile verbosity cabalFlags cabalOption logCabal) cabalBuildInfos

getCabalBuildInfosAsync :: (?cl :: CabalLock) =>
  (Either SomeException (OpResult [CabalBuildInfo]) -> IO ()) -> FilePath -> IO ThreadId
getCabalBuildInfosAsync onTerminate cabalFile = do
  let tempFolder = ".dist-buildwrapper"
      cabalPath = "cabal"
--      cabalFile = "/home/cvanvranken/Documents/leksahWorkspace/WXDiffCtrl-0.0.1/WXDiffCtrl.cabal"
      verbosity = Normal
      cabalFlags = ""
      cabalOption = []
      logCabal = True
  runCmdAsync (BuildWrapperState tempFolder cabalPath cabalFile verbosity cabalFlags cabalOption logCabal) cabalBuildInfos onTerminate

-- TODO: lock runCmdAsync, so that commands must queue up if a command is running.
runCmdAsync :: (?cl :: CabalLock) =>
   BuildWrapperState -> BuildWrapper a -> (Either SomeException a -> IO ()) -> IO ThreadId
runCmdAsync initialState bwFunc onTerminate =
  let rCmd = runCmd initialState bwFunc
  in forkFinally rCmd onTerminate

-- | BuildWrapperState represents a command, take an initial state and evaluate a BuildWrapper function
runCmd :: (?cl :: CabalLock) =>
  BuildWrapperState -> BuildWrapper a -> IO a
runCmd initialState bwFunc= do
  L.acquire . unCabLock $ ?cl
  resultJson <- evalStateT bwFunc initialState
  L.release . unCabLock $ ?cl
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




