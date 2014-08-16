
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

import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Event)


keyOnDownEvent :: Event (Window a) (EventKey -> IO ())
keyOnDownEvent = newEvent "keyOnDown" windowGetOnKeyDown1 windowOnKeyDown

-- | Get the current translated key handler of a window.
windowGetOnKeyDown1 :: Window a -> IO (EventKey -> IO ())
windowGetOnKeyDown1 window
  = unsafeWindowGetHandlerState window wxEVT_KEY_DOWN (\eventKey -> return ())


contextMenuEvent :: Event (Window a) (IO ())
contextMenuEvent = newEvent "contextMenuEvent" windowGetOnContextMenu windowOnContextMenu

wxDefaultPosition :: Point
wxDefaultPosition = pointNull

contextMenuPopup = flip menuPopup wxDefaultPosition

-- from defs.h
wxID_ANY :: Int
wxID_ANY = -1
