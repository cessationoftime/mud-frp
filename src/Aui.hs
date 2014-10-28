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
{-# LANGUAGE Rank2Types #-}

module Aui where
import Dialogs
import SourceEditor
import Controls.Mud.MapEditor
import RBWX.RBWX
import System.FilePath
import Data.List (find)
import Data.Maybe (fromJust)

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
 -- windowFreeze notebook
  snp@(SourceNotebookPage _ sourceEditorCtrl _) <- internalAddNewSourcePage notebook filePath
--  windowThaw notebook
  return snp

internalAddNewSourcePage :: AuiNotebook () -> FilePath -> IO NotebookPage
internalAddNewSourcePage notebook filePath = do
  sourceEditorCtrl <- sourceEditor notebook []
  _ <- auiNotebookAddPageWithBitmap notebook sourceEditorCtrl (takeFileName filePath) True nullBitmap
  id <- windowGetId sourceEditorCtrl

  return $ SourceNotebookPage id sourceEditorCtrl filePath

addSourcePage :: AuiNotebook () -> FilePath -> IO NotebookPage
addSourcePage notebook filePath = do
 -- windowFreeze notebook
  snp@(SourceNotebookPage _ sourceEditorCtrl _) <- internalAddNewSourcePage notebook filePath
  sourceEditorLoadFile sourceEditorCtrl filePath
 -- windowThaw notebook
  return snp


class IsNotebookPage a where
  isNotebookPage :: a -> NotebookPage -> Bool

instance IsNotebookPage WindowId where
  isNotebookPage winId (SourceNotebookPage id _ _) = winId == id

instance IsNotebookPage WindowSelection where
  isNotebookPage (WindowSelection (Just winId) _) (SourceNotebookPage id _ _) = winId == id
  isNotebookPage (WindowSelection Nothing _) (SourceNotebookPage id _ _) = False


data NotebookEvents t = NotebookEvents {
  -- | newFileDialog OK button press, conducts FilePath of selected file
  newFileDialogOk :: Event t FilePath,
  -- | new page added to notebook, conducts the page added
  newNoteBookPage :: Event t NotebookPage,
  -- | openFileDialog OK button press, conducts FilePath of selected file
  openFileDialogOk :: Event t FilePath,
  -- | newly opened page added to notebook, conducts the page added
  openNoteBookPage  :: Event t NotebookPage,
  -- | page added to notebook (open or new), conducts the page added
  addedNoteBookPage :: Event t NotebookPage,
  -- | notebook page changed but not added
  changedNoteBookPage :: Event t (Maybe NotebookPage),
  -- | the currently selected NotebookPage has been switched, changed or added (active tab)
  switchedNoteBookPage :: Event t (Maybe NotebookPage),
  -- | notebook page closed
  closedNoteBookPage :: Event t (Maybe NotebookPage),
  -- | pages currently in the notebook
  pages :: Behavior t [NotebookPage]
  }


createNotebookEvents :: Frameworks t => AuiNotebook () -> Frame () -> Event t () -> Event t () -> Moment t (NotebookEvents t)
createNotebookEvents notebook frame1 eNewMenuItem eOpenMenuItem = do
    eNewFileOk :: Event t FilePath <- openFileDialogOkEvent frame1 eNewMenuItem
    eNewNotebookPage :: Event t NotebookPage <- addNewSourcePage notebook `mapIOreaction` eNewFileOk
    eOpenFileOk :: Event t FilePath <- openFileDialogOkEvent frame1 eOpenMenuItem
    eOpenNotebookPage :: Event t NotebookPage <- addSourcePage notebook `mapIOreaction` eOpenFileOk
    eClosedEventAuiNoteBook :: Event t EventAuiNotebook <- event1 notebook notebookOnPageClosedEvent
    eChangedEventAuiNoteBook :: Event t EventAuiNotebook <- eChangedNotebookPage notebook
    let eAddedNotebookPage  :: Event t NotebookPage = eNewNotebookPage `union` eOpenNotebookPage
        eChangedNotebookPage :: Event t (Maybe NotebookPage) = fromEventAui2NotebookPage eChangedEventAuiNoteBook
        eClosedNotebookPage :: Event t (Maybe NotebookPage) = fromEventAui2NotebookPage eClosedEventAuiNoteBook
        eSwitchedNotebookPage  :: Event t (Maybe NotebookPage) = (Just `fmap` eAddedNotebookPage) `union` eChangedNotebookPage

        findPage :: [NotebookPage] -> WindowSelection -> Maybe NotebookPage
        findPage notes winSelect = find (isNotebookPage winSelect) notes

        fromEventAui2NotebookPage :: Event t EventAuiNotebook -> Event t (Maybe NotebookPage)
        fromEventAui2NotebookPage ev = (findPage <$> bPages) <@> evWindowSelect
           where evWindowSelect = (\(EventAuiNotebook nbCurrent _ _) -> nbCurrent) `fmap` ev

        filterNotPage (EventAuiNotebook _ newSel _) =  filter (not . isNotebookPage newSel)

        bPages :: Behavior t [NotebookPage]
        bPages = accumB [] $
            (add <$> eAddedNotebookPage) `union` (filterNotPage <$> eClosedEventAuiNoteBook)
          where
            add  nb nbs = nb:nbs

    return $ NotebookEvents eNewFileOk eNewNotebookPage eOpenFileOk eOpenNotebookPage eAddedNotebookPage eChangedNotebookPage eSwitchedNotebookPage eClosedNotebookPage bPages

