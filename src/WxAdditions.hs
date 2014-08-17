
-----------------------------------------------------------------------------
--
-- Module      :  WxAdditions
-- Copyright   :
-- License     :
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module WxAdditions

where

import Graphics.UI.WX (Event)
--import Graphics.UI.WXCore hiding (Event)
import RBWX.RBWX hiding (Event)

keyOnDownEvent :: Event (Window a) (EventKey -> IO ())
keyOnDownEvent = newEvent "keyOnDown" windowGetOnKeyDown1 windowOnKeyDown

-- | Get the current translated key handler of a window.
windowGetOnKeyDown1 :: Window a -> IO (EventKey -> IO ())
windowGetOnKeyDown1 window
  = unsafeWindowGetHandlerState window wxEVT_KEY_DOWN (\eventKey -> return ())

-- AUI Notebook PageClose
auiNotebookOnPageCloseEvent :: Event (AuiNotebook a) (IO ())
auiNotebookOnPageCloseEvent = newEvent "auiNotebookOnPageClose" auiNotebookGetOnPageClose auiNotebookOnPageClose

auiNotebookGetOnPageClose :: AuiNotebook a -> IO (IO ())
auiNotebookGetOnPageClose notebook
  = unsafeWindowGetHandlerState notebook wxEVT_AUINOTEBOOK_PAGE_CLOSE propagateEvent

auiNotebookOnPageClose :: AuiNotebook a -> IO () -> IO ()
auiNotebookOnPageClose notebook eventHandler
  = windowOnEvent notebook [wxEVT_AUINOTEBOOK_PAGE_CLOSE] eventHandler (const eventHandler)



-- AUINotebook PageClosed
auiNotebookOnPageClosedEvent :: Event (AuiNotebook a) (WindowId -> IO ())
auiNotebookOnPageClosedEvent = newEvent "auiNotebookOnPageClosed" auiNotebookGetOnPageClosed auiNotebookOnPageClosed

auiNotebookGetOnPageClosed :: AuiNotebook a -> IO (WindowId -> IO ())
auiNotebookGetOnPageClosed notebook
  = unsafeWindowGetHandlerState notebook wxEVT_AUINOTEBOOK_PAGE_CLOSED (const propagateEvent)

auiNotebookOnPageClosed :: AuiNotebook a -> (WindowId -> IO ()) -> IO ()
auiNotebookOnPageClosed notebook eventHandler
  = windowOnEvent notebook [wxEVT_AUINOTEBOOK_PAGE_CLOSED] eventHandler closeHandler
       where closeHandler event = do
               window <- fromCloseEvent (objectCast event)
               eventHandler window

fromCloseEvent :: AuiNotebookEvent a -> IO (WindowId)
fromCloseEvent event = do
 -- et <- eventGetEventType event
  obj <- eventGetEventObject event
  id <- windowGetId (objectCast obj)
  return id
