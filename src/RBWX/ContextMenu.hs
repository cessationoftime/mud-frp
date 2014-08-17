-----------------------------------------------------------------------------
--
-- Module      :  RBWX.ContextMenu
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

module RBWX.ContextMenu where


import Reactive.Banana
import Reactive.Banana.WX
import RBWX.Lift hiding (Event, newEvent)
import Graphics.UI.WX as WXC

contextMenuEvent :: WXC.Event (Window a) (IO ())
contextMenuEvent = WXC.newEvent "contextMenuEvent" windowGetOnContextMenu windowOnContextMenu



-- |wire together the contextMenuEvent and displaying the the given menu as a context
wireupContextMenu :: Frameworks t => Window a -> Menu b -> Moment t ()
wireupContextMenu win menu = do
  eContextMenu <- event0 win contextMenuEvent
  reactimate $ contextMenuPopup menu win <$ eContextMenu

contextMenuPopup = flip menuPopup wxDefaultPosition


wxDefaultPosition :: Point
wxDefaultPosition = pointNull
