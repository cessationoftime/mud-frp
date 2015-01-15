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

keyOnDownEvent :: Event (Window a) (EventKey -> IO ())
keyOnDownEvent = newEvent "keyOnDown" windowGetOnKeyDown1 windowOnKeyDown

treeOnTreeEvent :: Event (TreeCtrl a) (EventTree -> IO ())
treeOnTreeEvent = newEvent "treeOnTreeEvent" treeCtrlGetOnTreeEvent treeCtrlOnTreeEvent

-- | Get the current translated key handler of a window.
windowGetOnKeyDown1 :: Window a -> IO (EventKey -> IO ())
windowGetOnKeyDown1 window
  = unsafeWindowGetHandlerState window wxEVT_KEY_DOWN (\eventKey -> return ())

-- | given an event and a window or frame. determine if the eventObject of the event is the given window
eventObjectIsWindow :: WX.Event a -> Object b -> IO Bool
eventObjectIsWindow ev win = do eventObj <- eventGetEventObject ev
                                return (objectCast eventObj == win)

window2Selection :: MaybeT IO (Window ()) -> MaybeT IO WindowId
window2Selection mbW = do
  id <- liftM windowGetId mbW
  id2 <- lift id
  return id2

frameMax :: [Prop (Frame ())] -> IO (Frame ())
frameMax props = frameEx (frameDefaultStyle .+. wxMAXIMIZE) props objectNull

newtype ThreadAsyncTrigger = ThreadAsyncTrigger { unThreadAsyncTrigger :: IO ()}

cabalBuildInfoAsyncEventId = wxID_HIGHEST+777 -- the custom event ID


threadAsyncEvent :: Int -> (Event (Window a) (IO ()),Frame () -> ThreadAsyncTrigger)
threadAsyncEvent eveId = (newEvent "threadAsyncEvent" (threadAsyncGetOnEvent eveId) (threadAsyncOnEvent eveId),threadAsyncTrigger eveId)
  where
  threadAsyncOnEvent eveId win io = evtHandlerOnMenuCommand win eveId io
  threadAsyncGetOnEvent eveId win  = evtHandlerGetOnMenuCommand win eveId

  threadAsyncTrigger :: Int -> Frame () -> ThreadAsyncTrigger
  threadAsyncTrigger eveId frame = ThreadAsyncTrigger $ createAsyncTrigger >>= (evtHandlerAddPendingEvent frame)
    where createAsyncTrigger = commandEventCreate wxEVT_COMMAND_MENU_SELECTED eveId


 {-
gui = do f <- frame [text := "custom event sample"]
         bt <- button f [text := "click to invoke a custom event"]
         set f [layout := column 1 [hfill (widget bt)]]
         set bt [on command := onClick f]
         registerMyEvent f (putStrLn "The custom event is fired!!")
         return ()
         where
         onClick f = do
           ev <- createMyEvent
           evtHandlerAddPendingEvent f ev
           return ()
-}
