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

module RBWX.Lift
  (RBWX.Lift.menuSub,
   RBWX.Lift.menuItem,
   RBWX.Lift.menuQuit,
   RBWX.Lift.menuPane,
   RBWX.Lift.statusField,
   RBWX.Lift.frame,
   RBWX.Lift.menuLine,
   RBWX.Lift.set,
   RBWX.Lift.button,
   RBWX.Lift.panel,
   RBWX.Lift.timer,
   module WX_,
   module WXCore,
   module WxClasses,
   module WxAdditions
  ) where
import WxAdditions
import Graphics.UI.WX as WX_
 hiding (panel,button,Timer,set,menuLine,frame,statusField,menuSub,menuPane,menuItem,menuQuit, Event,
         timer)
 -- (Prop((:=)), start, statusBar, layout, command, on, menuBar, menu, tabTraversal,
 --  mouse, interval, repaint, timer, bitmap, drawBitmap, paint,styledTextCtrl
 -- )
import Graphics.UI.WXCore as WXCore
  hiding (Event)

 -- (fill, sz, widget, styledTextCtrlAddText, white, minsize, column, margin,
--  mousePos,Point2(Point),panelSetFocus,EventMouse(MouseLeftDown),keyKey, Key(KeyLeft,KeyRight,KeyUp,KeyDown),
 -- point,Window, Bitmap, Panel,DC, rgb, wxSTC_LEX_HASKELL,
 -- )
import Graphics.UI.WX.Classes as WxClasses
 -- ( help, text, bgcolor,resizeable, close
 -- )

import qualified Graphics.UI.WX as WX
import Reactive.Banana
import Reactive.Banana.WX hiding (newEvent)


menuSub :: Frameworks t => Menu b -> Menu a -> [Prop (MenuItem ())] -> Moment t (MenuItem ())
menuSub = liftIO3 WX.menuSub

menuItem :: Frameworks t => Menu a -> [Prop (MenuItem ())] -> Moment t (MenuItem ())
menuItem = liftIO2 WX.menuItem

menuQuit :: Frameworks t => Menu a -> [Prop (MenuItem ())] -> Moment t (MenuItem ())
menuQuit = liftIO2 WX.menuQuit

menuPane :: Frameworks t => [Prop (Menu ())] -> Moment t (Menu ())
menuPane = liftIO1 WX.menuPane

panel :: Frameworks t => Window a -> [Prop (Panel ())] -> Moment t (Panel ())
panel = liftIO2 WX.panel

statusField :: Frameworks t => [Prop (StatusField)] -> Moment t (StatusField)
statusField = liftIO1 WX.statusField

frame :: Frameworks t => [Prop (Frame ())] -> Moment t (Frame ())
frame = liftIO1 WX.frame

timer :: Frameworks t =>  Window a -> [Prop WX.Timer ] -> Moment t (WX.Timer)
timer = liftIO2 WX.timer

menuLine :: Frameworks t => Menu () -> Moment t ()
menuLine = liftIO1 WX.menuLine

set :: Frameworks t => w -> [Prop w] -> Moment t ()
set = liftIO2 WX.set

button :: Frameworks t => Window a -> [Prop (Button ())] -> Moment t (Button ())
button = liftIO2 WX.button

---------------

liftIO1 :: Frameworks t => (a -> IO b) -> a -> Moment t b
liftIO1 funct = liftIO . funct

liftIO2 :: Frameworks t => (a -> b -> IO c) -> a -> b -> Moment t c
liftIO2 funct aa = liftIO . funct aa

liftIO3 :: Frameworks t => (a -> b -> c -> IO d) -> a -> b -> c -> Moment t d
liftIO3 funct aa bb = liftIO . funct aa bb
