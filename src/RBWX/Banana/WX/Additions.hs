module RBWX.Banana.WX.Additions

where

import Graphics.UI.WX (Event)
--import Graphics.UI.WXCore hiding (Event)
import RBWX.Banana.WX.Core.Core hiding (Event)
import Data.Maybe
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad
--import Maybes
keyOnDownEvent :: Event (Window a) (EventKey -> IO ())
keyOnDownEvent = newEvent "keyOnDown" windowGetOnKeyDown1 windowOnKeyDown

-- | Get the current translated key handler of a window.
windowGetOnKeyDown1 :: Window a -> IO (EventKey -> IO ())
windowGetOnKeyDown1 window
  = unsafeWindowGetHandlerState window wxEVT_KEY_DOWN (\eventKey -> return ())

-- AUI Notebook PageClose
notebookOnPageCloseEvent :: Event (AuiNotebook a) (EventAuiNotebook -> IO ())
notebookOnPageCloseEvent = newAuiEvent "auiNotebookOnPageClose" wxEVT_AUINOTEBOOK_PAGE_CLOSE

-- AUINotebook PageClosed
--notebookOnPageClosedEvent :: Event (AuiNotebook a) (EventAuiNotebook -> IO ())
--notebookOnPageClosedEvent = newAuiEvent "auiNotebookOnPageClosed" wxEVT_AUINOTEBOOK_PAGE_CLOSED

-- AUINotebook PageChange
notebookOnPageChangingEvent :: Event (AuiNotebook a) ((Int,Int) -> IO ())
notebookOnPageChangingEvent = newAuiEvent_Changing "auiNotebookOnPageChanging" wxEVT_AUINOTEBOOK_PAGE_CHANGING


--split into two events. ChangedTo and ChangedFrom based on the eventObject

-- AUINotebook PageChanged
notebookOnPageChangedEvent :: Event (AuiNotebook a) (EventAuiNotebook -> IO ())
notebookOnPageChangedEvent = newAuiEvent "auiNotebookOnPageChanged" wxEVT_AUINOTEBOOK_PAGE_CHANGED

data WindowSelection = WindowSelection (Maybe WindowId) (Maybe (Window ())) deriving (Show)
data EventAuiNotebook = EventAuiNotebook {
    nbCurrent   :: WindowSelection,    -- Current Selection from Notebook
    nbChangeNew :: WindowSelection,    -- NewSelected Window from Event
    nbChangeOld :: WindowSelection     -- Old Selected Page from Event
   } deriving (Show)
noWindowSelection :: WindowSelection
noWindowSelection = WindowSelection Nothing Nothing



--- Utility -------------------------------------

newAuiEvent :: String -> EventId -> Event (AuiNotebook a) (EventAuiNotebook -> IO ())
newAuiEvent s evId = newEvent s (auiGetOn evId) (auiOn s evId)

auiOn :: String -> EventId ->  AuiNotebook a -> (EventAuiNotebook -> IO ()) -> IO ()
auiOn s eventId notebook eventHandler
  = windowOnEvent notebook [eventId] eventHandler closeHandler
       where closeHandler event = do
               window <- fromAuiNotebookEvent s (objectCast event)
               eventHandler window

auiGetOn :: EventId -> AuiNotebook a -> IO (EventAuiNotebook -> IO ())
auiGetOn eventId notebook
  = unsafeWindowGetHandlerState notebook eventId (const skipCurrentEvent)


fromAuiNotebookEvent :: String -> AuiNotebookEvent a -> IO EventAuiNotebook
fromAuiNotebookEvent s event = do
  notebook <- objectCast `fmap` (eventGetEventObject event)
  evType <- eventGetEventType event
  selection <- bookCtrlEventGetSelection event
  oldSelection <- bookCtrlEventGetOldSelection event

  -- crashes when switching tabs (not opening or closing on the Changing event)
  cSelection <- if evType == wxEVT_AUINOTEBOOK_PAGE_CHANGING then (return 0) else  notebookGetSelection notebook



  -- crashes when switching tabs (not opening or closing on the Changing event)
 -- let cPage =  notebookGetCurrentPage notebook
 -- let pageId = window2Selection cPage
--  cp <- runMaybeT cPage
 -- pi <- runMaybeT pageId
 -- let winSelection = WindowSelection pi cp
 -- infoDialog notebook (show cSelection ++ " " ++ show selection ++ " " ++ show oldSelection ++ " " ++ show noteId) s
  pgCount <- notebookGetPageCount notebook
  current <- newSel notebook cSelection pgCount
  new <- newSel notebook selection pgCount
  old <- newSel notebook oldSelection pgCount
  let ean = EventAuiNotebook current new old
 -- infoDialog notebook (changeType ++ " " ++ show cSelection ++ " " ++ show selection ++ " " ++ show oldSelection) (s ++ "\n" ++ show ean ++ "\n\n" )
  return ean
  where newSel nb sel count =
          if sel < count && sel /= wxNOT_FOUND then do
          pg <- notebookGetPage nb sel
          id <- windowGetId pg
          return $ WindowSelection (Just id) (Just pg)
          else return noWindowSelection

window2Selection :: MaybeT IO (Window ()) -> MaybeT IO WindowId
window2Selection mbW = do
  id <- liftM windowGetId mbW
  id2 <- lift id
  return id2


newAuiEvent_Changing :: String -> EventId -> Event (AuiNotebook a) ((Int,Int) -> IO ())
newAuiEvent_Changing s evId = newEvent s (auiGetOn_Changing evId) (auiOn_Changing s evId)

auiGetOn_Changing :: EventId -> AuiNotebook a -> IO ((Int,Int) -> IO ())
auiGetOn_Changing eventId notebook
  = unsafeWindowGetHandlerState notebook eventId (const skipCurrentEvent)

auiOn_Changing :: String -> EventId ->  AuiNotebook a -> ((Int,Int) -> IO ()) -> IO ()
auiOn_Changing s eventId notebook eventHandler
  = windowOnEvent notebook [eventId] eventHandler closeHandler
       where closeHandler event = do
               window <- fromAuiNotebookEvent_Changing s (objectCast event)
               eventHandler window

fromAuiNotebookEvent_Changing :: String -> AuiNotebookEvent a -> IO (Int,Int)
fromAuiNotebookEvent_Changing s event = do
  notebook <- objectCast `fmap` (eventGetEventObject event)
  evType <- eventGetEventType event
  selection <- bookCtrlEventGetSelection event
  oldSelection <- bookCtrlEventGetOldSelection event

{-
  -- crashes when switching tabs (not opening or closing on the Changing event)
  cSelection <- if evType == wxEVT_NOTEBOOK_PAGE_CHANGING then (return 0) else  notebookGetSelection notebook

  let changeType = case (cSelection,selection,oldSelection) of
         (c,s,o) | evType == wxEVT_NOTEBOOK_PAGE_CHANGING && o == -1 && c == -1 -> "ChangingTo"
         (c,s,o)| evType == wxEVT_NOTEBOOK_PAGE_CHANGED && o == -1  -> "ChangedTo"
         (c,s,o)| evType == wxEVT_NOTEBOOK_PAGE_CHANGING && c == s -> "ChangingTo"
         (c,s,o)| evType == wxEVT_NOTEBOOK_PAGE_CHANGING && c == o -> "ChangingFrom"

         (c,s,o)| evType == wxEVT_NOTEBOOK_PAGE_CHANGING && c == o -> "ChangingFrom"
         (c,s,o)| evType == wxEVT_NOTEBOOK_PAGE_CHANGED  && c == s -> "ChangedTo"
         (c,s,o) | evType == wxEVT_NOTEBOOK_PAGE_CHANGED && c == o -> "ChangedFrom"
         _ -> "Other"
-}


  -- crashes when switching tabs (not opening or closing on the Changing event)
 -- let cPage =  notebookGetCurrentPage notebook
 -- let pageId = window2Selection cPage
--  cp <- runMaybeT cPage
 -- pi <- runMaybeT pageId
 -- let winSelection = WindowSelection pi cp
  --infoDialog notebook (show selection ++ " " ++ show oldSelection) s
  pgCount <- notebookGetPageCount notebook  -- causes crashing
 -- current <- newSel notebook cSelection pgCount
  if evType == wxEVT_AUINOTEBOOK_PAGE_CHANGING then return (selection,oldSelection) else return (-9,-9)
 -- infoDialog notebook (changeType ++ " " ++ show cSelection ++ " " ++ show selection ++ " " ++ show oldSelection) (s ++ "\n" ++ show ean ++ "\n\n" )
  --return (selection,oldSelection)


