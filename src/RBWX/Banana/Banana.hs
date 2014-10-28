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
  --eCloseNotebookPage,
  eClosedNotebookPage,
  eChangedNotebookPage
  --eChangingNotebookPage,
  --eNotebookPage,
  --eActiveNotebookPage,
  --bActiveNotebookPage
	) where
import Reactive.Banana
import Reactive.Banana.WX
import RBWX.Banana.WX.Additions
import RBWX.Banana.WX.Core.Core as Core hiding (identity, Identity, empty, newEvent)

--import Graphics.UI.WX.Events as WX (Event)

--eCloseNotebookPage :: Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
--eCloseNotebookPage notebook =  event1 notebook notebookOnPageCloseEvent

eClosedNotebookPage ::  Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
eClosedNotebookPage notebook =  event1 notebook notebookOnPageClosedEvent

eChangedNotebookPage ::  Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
eChangedNotebookPage notebook =  event1 notebook notebookOnPageChangedEvent

--eChangingNotebookPage ::  Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
--eChangingNotebookPage notebook =  event1 notebook notebookOnPageChangingEvent
