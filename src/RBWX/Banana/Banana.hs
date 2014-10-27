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
  --eClosedNotebookPage,
  eChangedNotebookPage,
  eChangingNotebookPage,
  eNotebookPage,
  eActiveNotebookPage,
  bActiveNotebookPage
	) where
import Reactive.Banana
import Reactive.Banana.WX
import RBWX.Banana.WX.Additions
import RBWX.Banana.WX.Core.Core as Core hiding (identity, Identity, empty, newEvent)


--eCloseNotebookPage :: Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
--eCloseNotebookPage notebook =  event1 notebook notebookOnPageCloseEvent

--eClosedNotebookPage ::  Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
--eClosedNotebookPage notebook =  event1 notebook notebookOnPageClosedEvent

eChangedNotebookPage ::  Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
eChangedNotebookPage notebook =  event1 notebook notebookOnPageChangedEvent

eChangingNotebookPage ::  Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
eChangingNotebookPage notebook =  event1 notebook notebookOnPageChangingEvent

eNotebookPage :: Frameworks t => Core.AuiNotebook a -> Moment t (Event t EventAuiNotebook)
eNotebookPage notebook = do
  --close <- eCloseNotebookPage notebook
 -- closed <- eClosedNotebookPage notebook
  changed <- eChangedNotebookPage notebook
  changing <- eChangingNotebookPage notebook
  return $ unions [changed,changing]

eActiveNotebookPage :: Frameworks t => Core.AuiNotebook a -> Moment t (Event t WindowSelection)
eActiveNotebookPage notebook = do
  e <- eNotebookPage notebook
  return $ (\(EventAuiNotebook current _ _) -> current ) `fmap` e

bActiveNotebookPage :: Frameworks t => Core.AuiNotebook a -> Moment t (Behavior t (Maybe WindowSelection))
bActiveNotebookPage notebook = do
  e <- eActiveNotebookPage notebook
  let b = stepper Nothing (Just <$> e )
  return b


