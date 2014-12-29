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


liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return
