{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
--
-- Module      :  RBWX.Banana
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

module RBWX.Banana.Banana(
module RBWX.Banana.WX.Additions,
  module Core,
  module Reactive.Banana,
  module Reactive.Banana.WX,
  eCloseNotebookPage,
  eClosedNotebookPage,
  eChangedNotebookPage,
  eChangingNotebookPage,
  eEventTreeCtrl,
  newThreadAsyncEvent,
  eSTCEvent
  --eNotebookPage,
  --eActiveNotebookPage,
  --bActiveNotebookPage
	) where
import Reactive.Banana
import Reactive.Banana.WX
import RBWX.Banana.WX.Additions
import RBWX.Banana.WX.Core.Core as Core hiding (identity, Identity, empty, newEvent)

--import Graphics.UI.WX.Events as WX (Event)

eCloseNotebookPage :: Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
eCloseNotebookPage notebook =  event1 notebook auiNotebookOnPageCloseEvent

eClosedNotebookPage ::  Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
eClosedNotebookPage notebook =  event1 notebook auiNotebookOnPageClosedEvent

eChangedNotebookPage ::  Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
eChangedNotebookPage notebook =  event1 notebook auiNotebookOnPageChangedEvent

eChangingNotebookPage ::  Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
eChangingNotebookPage notebook =  event1 notebook auiNotebookOnPageChangingEvent

eEventTreeCtrl ::  Frameworks t => Core.TreeCtrl a -> Moment t (Event t EventTree)
eEventTreeCtrl treeCtrl =  event1 treeCtrl treeOnTreeEvent

newThreadAsyncEvent :: Frameworks t => Int -> Frame () -> Moment t (Event t (),ThreadAsyncTrigger)
newThreadAsyncEvent eveId frame = do
  let (eve,trigger) = threadAsyncEvent eveId
  evet <- event0 frame eve
  return (evet,trigger frame)

eSTCEvent ::  Frameworks t => StyledTextCtrl () -> Moment t (Event t EventSTC)
eSTCEvent styledTextCtrl =  event1 styledTextCtrl stcEvent
