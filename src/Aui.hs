-----------------------------------------------------------------------------
--
-- Module      :  Aui
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

module Aui where
import Reactive.Banana
import Reactive.Banana.WX
import Dialogs
import SourceEditor
import Controls.Mud.MapEditor
import RBWX.RBWX
import System.FilePath
import WxAdditions

data NotebookPage = SourceNotebookPage WindowId SourceEditorCtrl FilePath

newNotebook :: Frame a -> IO (AuiNotebook ())
newNotebook frame1 = do
-- create the notebook off-window to avoid flicker
  (Size x y) <- windowGetClientSize frame1
  notebook <- auiNotebookCreate frame1 wxID_ANY (Point x y) (Size 430 200) (wxAUI_NB_DEFAULT_STYLE .+. wxAUI_NB_TAB_EXTERNAL_MOVE .+. wxNO_BORDER)
  -- Freeze during notebook setup to avoid flickering/redrawing
  windowFreeze notebook

  --additional Notebook setup here

  ---------------------------

  windowThaw notebook
  return (notebook)






addNewSourcePage :: AuiNotebook () -> FilePath -> IO NotebookPage
addNewSourcePage notebook filePath = do
  sourceEditorCtrl <- sourceEditor notebook []
  _ <- auiNotebookAddPageWithBitmap notebook sourceEditorCtrl (takeFileName filePath) True nullBitmap
  id <- windowGetId sourceEditorCtrl
  return $ SourceNotebookPage id sourceEditorCtrl filePath

addSourcePage :: AuiNotebook () -> FilePath -> IO NotebookPage
addSourcePage notebook filePath = do
  snp@(SourceNotebookPage _ sourceEditorCtrl _) <- addNewSourcePage notebook filePath
  sourceEditorLoadFile sourceEditorCtrl filePath
  return snp

  --index <- auiNotebookGetPageIndex notebook sourceEditorCtrl

class IsNotebookPage a where
  isNotebookPage :: a -> NotebookPage -> Bool

instance IsNotebookPage WindowId where
  isNotebookPage winId (SourceNotebookPage id _ _) = winId == id

instance IsNotebookPage WindowSelection where
  isNotebookPage (WindowSelection winId _) (SourceNotebookPage id _ _) = winId == id



