{-# LANGUAGE CPP#-}
module Paths (getDataFile) where

import System.FilePath
import System.IO.Unsafe


-- using cabal
import System.Environment.Executable
import System.Info
import qualified Paths_mudFrp (getDataDir)

getDataDir   = Paths_mudFrp.getDataDir


getDataFile :: FilePath -> FilePath
getDataFile x = unsafePerformIO $ fmap (</> x) getDataDir
