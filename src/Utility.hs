-----------------------------------------------------------------------------
--
-- Module      :  Utility
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

module Utility where

import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import System.IO (readFile)
import System.IO.Error

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

readCatch :: FilePath -> IO String
readCatch fp = catchIOError (readFile fp) handleError
  where
  handleError :: IOError -> IO String
  handleError e = if isDoesNotExistError e then return "" else ioError e
