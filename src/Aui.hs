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
import Dialogs
import SourceEditor
import Controls.Mud.MapEditor
import RBWX.RBWX
import System.FilePath

data NotebookPage = SourceNotebookPage WindowId SourceEditorCtrl FilePath

newNotebook :: Frame a -> IO (AuiNotebook ())
newNotebook frame1 = do
-- create the notebook off-window to avoid flicker
  (Size x y) <- windowGetClientSize frame1
  notebook <- auiNotebookCreate frame1 wxID_ANY (Point x y) (Size 430 200) (wxAUI_NB_DEFAULT_STYLE .+. wxAUI_NB_TAB_EXTERNAL_MOVE .+. wxNO_BORDER)
--  notebook <- notebookCreate frame1 wxID_ANY (Rect x y (x+430) (y+200)) (wxNO_BORDER)
  -- Freeze during notebook setup to avoid flickering/redrawing
  --windowFreeze notebook

  --additional Notebook setup here

  ---------------------------

 -- windowThaw notebook
  return (notebook)

addNewSourcePage :: AuiNotebook () -> FilePath -> IO NotebookPage
addNewSourcePage notebook filePath = do
  windowFreeze notebook
  snp@(SourceNotebookPage _ sourceEditorCtrl _) <- internalAddNewSourcePage notebook filePath
  windowThaw notebook
  return snp

internalAddNewSourcePage :: AuiNotebook () -> FilePath -> IO NotebookPage
internalAddNewSourcePage notebook filePath = do
  sourceEditorCtrl <- sourceEditor notebook []
  _ <- auiNotebookAddPageWithBitmap notebook sourceEditorCtrl (takeFileName filePath) True nullBitmap
  id <- windowGetId sourceEditorCtrl

  return $ SourceNotebookPage id sourceEditorCtrl filePath

addSourcePage :: AuiNotebook () -> FilePath -> IO NotebookPage
addSourcePage notebook filePath = do
  windowFreeze notebook
  snp@(SourceNotebookPage _ sourceEditorCtrl _) <- internalAddNewSourcePage notebook filePath
  sourceEditorLoadFile sourceEditorCtrl filePath
  windowThaw notebook
  return snp

  --index <- auiNotebookGetPageIndex notebook sourceEditorCtrl

class IsNotebookPage a where
  isNotebookPage :: a -> NotebookPage -> Bool

instance IsNotebookPage WindowId where
  isNotebookPage winId (SourceNotebookPage id _ _) = winId == id

instance IsNotebookPage WindowSelection where
  isNotebookPage (WindowSelection (Just winId) _) (SourceNotebookPage id _ _) = winId == id
  isNotebookPage (WindowSelection Nothing _) (SourceNotebookPage id _ _) = False



