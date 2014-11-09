module RBWX.Banana.WX.Additions

where

import Graphics.UI.WX (Event)
import qualified Graphics.UI.WXCore as WX (Event)
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

{-
data EventAuiNotebook = AuiNotebookAllowDnd { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookBeginDrag  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookBgDclick  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookButton  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookDragDone  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookDragMotion  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookEndDrag  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookPageChanged  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookPageChanging  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookPageClose  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookPageClosed  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookTabMiddleDown  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookTabMiddleUp  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookTabRightDown  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookTabRightUp  { newSel :: WindowSelection, oldSel :: WindowSelection }
                      | AuiNotebookUnknown
                      deriving (Show, Eq)

auiNotebookEvents :: [(Int, AuiNotebookEvent a -> IO EventAuiNotebook)]
auiNotebookEvents
  = [(wxEVT_AUINOTEBOOK_ALLOW_DND, withSelection  AuiNotebookAllowDnd)
    ,(wxEVT_AUINOTEBOOK_BEGIN_DRAG, withSelection AuiNotebookBeginDrag)
    ,(wxEVT_AUINOTEBOOK_BG_DCLICK, withSelection AuiNotebookBgDclick)
    ,(wxEVT_AUINOTEBOOK_BUTTON, withSelection AuiNotebookButton)
    ,(wxEVT_AUINOTEBOOK_DRAG_DONE,  withSelection AuiNotebookDragDone)
    ,(wxEVT_AUINOTEBOOK_DRAG_MOTION, withSelection AuiNotebookDragMotion)
    ,(wxEVT_AUINOTEBOOK_END_DRAG, withSelection AuiNotebookEndDrag)
    ,(wxEVT_AUINOTEBOOK_PAGE_CHANGED, withSelection AuiNotebookPageChanged)
    ,(wxEVT_AUINOTEBOOK_PAGE_CHANGING, withSelection AuiNotebookPageChanging)
    ,(wxEVT_AUINOTEBOOK_PAGE_CLOSE, withSelection AuiNotebookPageClose)
    ,(wxEVT_AUINOTEBOOK_PAGE_CLOSED, withSelection AuiNotebookPageClosed)
    ,(wxEVT_AUINOTEBOOK_TAB_MIDDLE_DOWN,withSelection AuiNotebookTabMiddleDown)
    ,(wxEVT_AUINOTEBOOK_TAB_MIDDLE_UP, withSelection AuiNotebookTabMiddleUp)
    ,(wxEVT_AUINOTEBOOK_TAB_RIGHT_DOWN, withSelection AuiNotebookTabRightDown)
    ,(wxEVT_AUINOTEBOOK_TAB_RIGHT_UP,  withSelection AuiNotebookTabRightUp)]
    where
          fromSelId selId ev
            = do eventObj <- eventGetEventObject ev
                 pg <- auiNotebookGetPage (objectCast eventObj) selId
                 id <- windowGetId pg
                 return $ WindowSelection selId $ Just $ PageWindow id  pg
          withSelection eventAN auiNEvent
            = do selection <- bookCtrlEventGetSelection auiNEvent
                 oldSelection <- bookCtrlEventGetOldSelection auiNEvent
                 winSel <- fromSelId selection auiNEvent
                 winOldSel <- fromSelId oldSelection auiNEvent
                 return $ eventAN winSel winOldSel


-- | given an eventList and the default (unknown) event. Return a function to lookup events in that list.
fromLookup2Event :: [(Int,b)] -> b -> (WX.Event a -> IO b)
fromLookup2Event eventList unknownEvent wxEvent
 = do tp <- eventGetEventType wxEvent
      return $ case lookup tp eventList of
        Just myevent  ->  myevent
        Nothing   -> unknownEvent

fromAuiNotebookEvent :: AuiNotebookEvent q -> IO EventAuiNotebook
fromAuiNotebookEvent  anEvent
    = do tp <- eventGetEventType anEvent
         case lookup tp auiNotebookEvents of
           Just f  -> f anEvent
           Nothing -> return AuiNotebookUnknown

-- AUI Notebook PageClose
auiNotebookOnPageCloseEvent :: Event (AuiNotebook a) (Maybe EventAuiNotebook -> IO ())
auiNotebookOnPageCloseEvent = newAuiEvent "auiNotebookOnPageClose" wxEVT_AUINOTEBOOK_PAGE_CLOSE

-- AUINotebook PageClosed
auiNotebookOnPageClosedEvent :: Event (AuiNotebook a) (Maybe EventAuiNotebook -> IO ())
auiNotebookOnPageClosedEvent = newAuiEvent "auiNotebookOnPageClosed" wxEVT_AUINOTEBOOK_PAGE_CLOSED

-- AUINotebook PageChange
auiNotebookOnPageChangingEvent :: Event (AuiNotebook a) (Maybe EventAuiNotebook -> IO ())
auiNotebookOnPageChangingEvent = newAuiEvent "auiNotebookOnPageChanging" wxEVT_AUINOTEBOOK_PAGE_CHANGING

-- AUINotebook PageChanged
auiNotebookOnPageChangedEvent :: Event (AuiNotebook a) (Maybe EventAuiNotebook -> IO ())
auiNotebookOnPageChangedEvent = newAuiEvent "auiNotebookOnPageChanged" wxEVT_AUINOTEBOOK_PAGE_CHANGED

data PageWindow = PageWindow { winId :: WindowId, win :: Window ()} deriving (Show, Eq)
data WindowSelection = WindowSelection Int (Maybe PageWindow) deriving (Show, Eq)
noWindowSelection :: WindowSelection
noWindowSelection = WindowSelection wxNOT_FOUND Nothing
-}
-- | given an event and a window or frame. determine if the eventObject of the event is the given window
eventObjectIsWindow :: WX.Event a -> Object b -> IO Bool
eventObjectIsWindow ev win = do eventObj <- eventGetEventObject ev
                                return (objectCast eventObj == win)
{-
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
  = windowOnEvent notebook [eventId] handler (const handler)
       where handler = withCurrentEvent (\event -> do
               cName <-  eventOfClass event

               evAuiNoteMaybe :: Maybe EventAuiNotebook <-
                   case cName of
                      "wxAuiNotebook" ->
                         Just `fmap` fromAuiNotebookEvent (objectCast event)
                         -- ofAuiNotebookEvent notebook (objectCast event)
                      "wxAuiTabCtrl" -> return Nothing -- I match on this only because I may want to do something with this case someday.  This case exists at least for the aui page changing event.
                      _ -> return Nothing
               eventHandler evAuiNoteMaybe
               )

auiGetOn :: EventId -> AuiNotebook a -> IO (Maybe EventAuiNotebook -> IO ())
auiGetOn eventId notebook
  = unsafeWindowGetHandlerState notebook eventId (const skipCurrentEvent)

-}

window2Selection :: MaybeT IO (Window ()) -> MaybeT IO WindowId
window2Selection mbW = do
  id <- liftM windowGetId mbW
  id2 <- lift id
  return id2

--TODO: add AuiTabCtrl functions to wxc
