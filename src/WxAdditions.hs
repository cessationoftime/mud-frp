
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
auiNotebookOnPageCloseEvent :: Event (AuiNotebook a) (EventAuiNotebook -> IO ())
auiNotebookOnPageCloseEvent = newAuiEvent "auiNotebookOnPageClose" wxEVT_AUINOTEBOOK_PAGE_CLOSE

-- AUINotebook PageClosed
auiNotebookOnPageClosedEvent :: Event (AuiNotebook a) (EventAuiNotebook -> IO ())
auiNotebookOnPageClosedEvent = newAuiEvent "auiNotebookOnPageClosed" wxEVT_AUINOTEBOOK_PAGE_CLOSED

-- AUINotebook PageChange
auiNotebookOnPageChangingEvent :: Event (AuiNotebook a) (EventAuiNotebook -> IO ())
auiNotebookOnPageChangingEvent = newAuiEvent "auiNotebookOnPageChanging" wxEVT_AUINOTEBOOK_PAGE_CHANGING

-- AUINotebook PageChanged
auiNotebookOnPageChangedEvent :: Event (AuiNotebook a) (EventAuiNotebook -> IO ())
auiNotebookOnPageChangedEvent = newAuiEvent "auiNotebookOnPageChanged" wxEVT_AUINOTEBOOK_PAGE_CHANGED

data WindowSelection = WindowSelection WindowId (Window ())
data EventAuiNotebook = EventAuiNotebook {
    nbCurrent   :: WindowSelection,    -- Current Selection from Notebook
    nbChangeNew :: WindowSelection,    -- NewSelected Window from Event
    nbChangeOld :: WindowSelection     -- Old Selected Page from Event
   }





--- Utility -------------------------------------

newAuiEvent :: String -> EventId -> Event (AuiNotebook a) (EventAuiNotebook -> IO ())
newAuiEvent s evId = newEvent s (auiGetOn evId) (auiOn evId)

auiOn :: EventId ->  AuiNotebook a -> (EventAuiNotebook -> IO ()) -> IO ()
auiOn eventId notebook eventHandler
  = windowOnEvent notebook [eventId] eventHandler closeHandler
       where closeHandler event = do
               window <- fromAuiNotebookEvent (objectCast event)
               eventHandler window

auiGetOn :: EventId -> AuiNotebook a -> IO (EventAuiNotebook -> IO ())
auiGetOn eventId notebook
  = unsafeWindowGetHandlerState notebook eventId (const propagateEvent)


fromAuiNotebookEvent :: AuiNotebookEvent a -> IO EventAuiNotebook
fromAuiNotebookEvent event = do
  notebookObj <- eventGetEventObject event
  selection <- bookCtrlEventGetSelection event
  oldSelection <- bookCtrlEventGetOldSelection event
  cSelection <- auiNotebookGetSelection (objectCast notebookObj)
  new <- newSel notebookObj selection
  old <- newSel notebookObj oldSelection
  current <- newSel notebookObj cSelection
  return $ EventAuiNotebook current new old
  where newSel nb sel = do
          pg <- auiNotebookGetPage (objectCast nb) sel
          id <- windowGetId pg
          return $ WindowSelection id pg



