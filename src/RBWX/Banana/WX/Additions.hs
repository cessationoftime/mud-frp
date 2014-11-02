module RBWX.Banana.WX.Additions

where

import Graphics.UI.WX (Event)
import qualified Graphics.UI.WXCore as Core (Event)
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

windowOnDestroy :: Window a -> (Core.Event () -> IO ()) -> IO ()
windowOnDestroy window eventHandler
  = windowOnEvent window [wxEVT_DESTROY] (const eventHandler) (\ev -> eventHandler ev)

-- AUI Notebook PageClose
notebookOnPageCloseEvent :: Event (AuiNotebook a) (Maybe EventAuiNotebook -> IO ())
notebookOnPageCloseEvent = newAuiEvent "auiNotebookOnPageClose" wxEVT_AUINOTEBOOK_PAGE_CLOSE

-- AUINotebook PageClosed
notebookOnPageClosedEvent :: Event (AuiNotebook a) (Maybe EventAuiNotebook -> IO ())
notebookOnPageClosedEvent = newAuiEvent "auiNotebookOnPageClosed" wxEVT_AUINOTEBOOK_PAGE_CLOSED

-- AUINotebook PageChange
notebookOnPageChangingEvent :: Event (AuiNotebook a) (Maybe EventAuiNotebook -> IO ())
notebookOnPageChangingEvent = newAuiEvent "auiNotebookOnPageChanging" wxEVT_AUINOTEBOOK_PAGE_CHANGING

-- AUINotebook PageChanged
notebookOnPageChangedEvent :: Event (AuiNotebook a) (Maybe EventAuiNotebook -> IO ())
notebookOnPageChangedEvent = newAuiEvent "auiNotebookOnPageChanged" wxEVT_AUINOTEBOOK_PAGE_CHANGED

data WindowSelection = WindowSelection (Maybe WindowId) (Maybe (Window ())) deriving (Show)
data EventAuiNotebook = EventAuiNotebook {
    nbCurrent   :: WindowSelection,    -- Current Selection from Notebook
    nbChangeNew :: WindowSelection,    -- NewSelected Window from Event
    nbChangeOld :: WindowSelection     -- Old Selected Page from Event
   } deriving (Show)
noWindowSelection :: WindowSelection
noWindowSelection = WindowSelection Nothing Nothing

-- | given an event and a window or frame. determine if the eventObject of the event is the given window
eventObjectIsWindow :: Object a -> Object b -> IO Bool
eventObjectIsWindow ev win = do eventObj <- eventGetEventObject (objectCast ev)
                                return (objectCast eventObj == win)

eventOfClass  :: Object a ->  IO String
eventOfClass event =  let ev =  objectCast event
                in do evObject <-   eventGetEventObject ev
                      cInfo <- objectGetClassInfo evObject
                      classInfoGetClassNameEx cInfo

--- Utility -------------------------------------

newAuiEvent :: String -> EventId -> Event (AuiNotebook a) (Maybe EventAuiNotebook -> IO ())
newAuiEvent s evId = newEvent s (auiGetOn evId) (auiOn s evId)

auiOn :: String -> EventId ->  AuiNotebook a -> (Maybe EventAuiNotebook -> IO ()) -> IO ()
auiOn s eventId notebook eventHandler
  = windowOnEvent notebook [eventId] eventHandler closeHandler
       where closeHandler event = do
               cName <-  eventOfClass event
               evAuiNoteMaybe :: Maybe EventAuiNotebook <-
                   case cName of
                      "wxAuiNotebook" ->
                         Just `fmap` fromAuiNotebookEvent notebook s (objectCast event)
                      "wxAuiTabCtrl" -> return Nothing -- I match on this only because I may want to do something with this case someday.  This case exists at least for the aui page changing event.
                      _ -> return Nothing
               eventHandler evAuiNoteMaybe

auiGetOn :: EventId -> AuiNotebook a -> IO (Maybe EventAuiNotebook -> IO ())
auiGetOn eventId notebook
  = unsafeWindowGetHandlerState notebook eventId (const skipCurrentEvent)


fromAuiNotebookEvent :: AuiNotebook a -> String -> AuiNotebookEvent a -> IO EventAuiNotebook
fromAuiNotebookEvent notebook s event = do
  --NOTE: eventGetEventObject does not always return an auiNotebook on the page changing event
 -- evObject <- eventGetEventObject event
  evType <- eventGetEventType event
  selection <- bookCtrlEventGetSelection event
  oldSelection <- bookCtrlEventGetOldSelection event

  cSelection <- auiNotebookGetSelection notebook



 -- let cPage =  notebookGetCurrentPage notebook
 -- let pageId = window2Selection cPage
--  cp <- runMaybeT cPage
 -- pi <- runMaybeT pageId
 -- let winSelection = WindowSelection pi cp
  pgCount <- auiNotebookGetPageCount notebook
  current <- newSel notebook cSelection pgCount
  new <- newSel notebook selection pgCount
  old <- newSel notebook oldSelection pgCount
  let ean = EventAuiNotebook current new old
  return ean
  where newSel nb sel count =
          if sel < count && sel /= wxNOT_FOUND then do
          pg <- auiNotebookGetPage nb sel
          id <- windowGetId pg
          return $ WindowSelection (Just id) (Just pg)
          else return noWindowSelection

window2Selection :: MaybeT IO (Window ()) -> MaybeT IO WindowId
window2Selection mbW = do
  id <- liftM windowGetId mbW
  id2 <- lift id
  return id2

--TODO: add AuiTabCtrl functions to wxc
