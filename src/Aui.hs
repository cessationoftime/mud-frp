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
import Data.List (find,partition)
import Data.Maybe (fromJust,listToMaybe,maybeToList)

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
  matchesNotebookPage :: a -> NotebookPage -> Bool

instance IsNotebookPage WindowId where
  matchesNotebookPage winId (SourceNotebookPage id _ _) = winId == id

instance IsNotebookPage WindowSelection where
  matchesNotebookPage (WindowSelection (Just winId) _) (SourceNotebookPage id _ _) = winId == id
  matchesNotebookPage (WindowSelection Nothing _) (SourceNotebookPage id _ _) = False


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
  -- | notebook page has been changed but not added, also doesn't trigger when pages hit zero
  changedNoteBookPage :: Event t (Maybe NotebookPage),
  -- | the currently selected NotebookPage has been changed or added (active tab), conducts Nothing when last page is about to close
  -- | NOTE: this function is inconsistent, such that most events occur after the fact, except for when the last page closes, which occurs prior to closing.
  switchedNoteBookPage :: Event t (Maybe NotebookPage),
  -- | notebook page is about to close, conducts the page about to close
  closeNoteBookPage :: Event t (Maybe NotebookPage),
  -- | last page in the notebook is about to close, conducts the page about to close
  lastClosedNoteBookPage :: Event t (Maybe NotebookPage),
  -- | pages currently in the notebook, and the last closed page
  pages :: Behavior t ([NotebookPage],Maybe NotebookPage)
  }


createNotebookEvents :: Frameworks t => AuiNotebook () -> Frame () -> Event t () -> Event t () -> Moment t (NotebookEvents t)
createNotebookEvents notebook frame1 eNewMenuItem eOpenMenuItem = do
    eNewFileOk :: Event t FilePath <- openFileDialogOkEvent frame1 eNewMenuItem
    eNewNotebookPage :: Event t NotebookPage <- addNewSourcePage notebook `mapIOreaction` eNewFileOk
    eOpenFileOk :: Event t FilePath <- openFileDialogOkEvent frame1 eOpenMenuItem
    eOpenNotebookPage :: Event t NotebookPage <- addSourcePage notebook `mapIOreaction` eOpenFileOk
    eCloseEventAuiNoteBook :: Event t EventAuiNotebook <- event1 notebook notebookOnPageCloseEvent
    eChangedEventAuiNoteBook :: Event t EventAuiNotebook <- eChangedNotebookPage notebook
    let eAddedNotebookPage  :: Event t NotebookPage = eNewNotebookPage `union` eOpenNotebookPage
        eChangedNotebookPage :: Event t (Maybe NotebookPage) =
            fromWindowSelection2NotebookPage $ (\(EventAuiNotebook nbCurrent _ _) -> nbCurrent) `fmap` eChangedEventAuiNoteBook
        eSwitchedNotebookPage  :: Event t (Maybe NotebookPage) =  (Nothing <$ eLastClose) `union` (Just `fmap` (eAddedNotebookPage `union` (filterJust eChangedNotebookPage)))
        eCloseNotebookPage :: Event t (Maybe NotebookPage) =
          fromWindowSelection2NotebookPage $ (\(EventAuiNotebook _ newSel _) -> newSel) `fmap` eCloseEventAuiNoteBook

        findPage :: [NotebookPage] -> WindowSelection -> Maybe NotebookPage
        findPage notes winSelect = find (matchesNotebookPage winSelect) notes

        fromWindowSelection2NotebookPage :: Event t WindowSelection -> Event t (Maybe NotebookPage)
        fromWindowSelection2NotebookPage ev = (findPage <$> (\(a,b) -> a ++ (maybeToList b) ) `fmap` bPages) <@> ev


        filterNotPage :: EventAuiNotebook -> [NotebookPage] -> ([NotebookPage],Maybe NotebookPage)
        filterNotPage (EventAuiNotebook _ newSel _) pages =
          let (m,n) = partition (not . matchesNotebookPage newSel) pages
          in  (m,listToMaybe n)


        -- | represents the current pages and the last closed page
        bPages :: Behavior t ([NotebookPage],Maybe NotebookPage)
        bPages = accumB ([],Nothing) $
            (add `fmap` eAddedNotebookPage) `union` (remove `fmap` eCloseEventAuiNoteBook {- this must be the close event, rather than the closed event. As the close event provides more information. -})
          where
            add nb (nbs,mbClosed) = (nb:nbs,mbClosed)
            remove nbSelect (nbs,mbClosed)  = filterNotPage nbSelect nbs

        -- | indicates that there is one page left in the notebook
        bOnePageLeft :: Behavior t Bool
        bOnePageLeft = (\(nps,_) -> length nps == 1) `fmap` bPages

        eLastClose :: Event t (Maybe NotebookPage)
        eLastClose = whenE bOnePageLeft eCloseNotebookPage

    return $ NotebookEvents eNewFileOk eNewNotebookPage eOpenFileOk eOpenNotebookPage eAddedNotebookPage eChangedNotebookPage eSwitchedNotebookPage eCloseNotebookPage eLastClose bPages

