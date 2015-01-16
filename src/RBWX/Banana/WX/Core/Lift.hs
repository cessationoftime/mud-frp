-----------------------------------------------------------------------------
--
-- Module      :  RBWX.Lift
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

module RBWX.Banana.WX.Core.Lift
  (RBWX.Banana.WX.Core.Lift.windowGetId,
   RBWX.Banana.WX.Core.Lift.auiNotebookGetCurrentPage,
   WindowId(..),
   module WX_,
   module WXCore_,
   module WxClasses
  ) where
import Graphics.UI.WX as WX_
 hiding (Timer,frame, Event)

import Graphics.UI.WXCore as WXCore_
  hiding (Event, windowGetId,auiNotebookGetCurrentPage)

import Graphics.UI.WXCore as WXCore
  hiding (Event)

import Graphics.UI.WX.Classes as WxClasses
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad

windowGetId :: forall a. Window a -> IO WindowId
windowGetId w =  do id <- WXCore.windowGetId w
                    return $ WindowId id

auiNotebookGetCurrentPage :: AuiNotebook a ->  MaybeT IO (Window ())
auiNotebookGetCurrentPage n = do
      currentPage <- lift $ WXCore.auiNotebookGetCurrentPage n
      if objectIsNull currentPage then mzero else return currentPage



