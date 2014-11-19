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

