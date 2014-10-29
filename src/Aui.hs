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


createSourcePage :: AuiNotebook () -> FilePath -> IO NotebookPage
createSourcePage notebook filePath = do
  sourceEditorCtrl <- sourceEditor notebook []
  id <- windowGetId sourceEditorCtrl
  return $ SourceNotebookPage id sourceEditorCtrl filePath

addEmptySourceTab :: AuiNotebook () -> NotebookPage -> IO ()
addEmptySourceTab notebook (SourceNotebookPage id sourceEditorCtrl filePath) = do
  _ <- auiNotebookAddPageWithBitmap notebook sourceEditorCtrl (takeFileName filePath) True nullBitmap
  return ()

addSourcePage :: AuiNotebook () -> NotebookPage -> IO ()
addSourcePage notebook pg@(SourceNotebookPage _ sourceEditorCtrl filePath) = do
  addEmptySourceTab notebook pg
  sourceEditorLoadFile sourceEditorCtrl filePath
  return ()


class IsNotebookPage a where
  matchesNotebookPage :: a -> NotebookPage -> Bool

instance IsNotebookPage WindowId where
  matchesNotebookPage winId (SourceNotebookPage id _ _) = winId == id

instance IsNotebookPage WindowSelection where
  matchesNotebookPage (WindowSelection (Just winId) _) (SourceNotebookPage id _ _) = winId == id
  matchesNotebookPage (WindowSelection Nothing _) (SourceNotebookPage id _ _) = False

-- TODO: make newFileDialogOK conduct a NotebookPage, make openFileDialogOk conduct a notebookPage
-- TODO: fix the inconsistency of switchedNotebookPage
data NotebookEvents t = NotebookEvents {
  -- | on newFileDialog OK button press, conducts FilePath of selected file
  new1FileDialogOk :: Event t FilePath,
  -- | on newFileDialog OK button press, conducts page to be added
  new2FileDialogNotebookPage  :: Event t NotebookPage,
  -- | on new page added to notebook, conducts the page added
  new3NoteBookPage :: Event t NotebookPage,
  -- | on openFileDialog OK button press, conducts FilePath of selected file
  open1FileDialogOk :: Event t FilePath,
  -- | on openFileDialog OK button press, conducts the page to be added
  open2FileDialogNotebookPage  :: Event t NotebookPage,
  -- | on newly opened page added to notebook, conducts the page added
  open3NoteBookPage  :: Event t NotebookPage,
  -- | on page added to notebook, conducts the page added
  openOrNewNoteBookPage :: Event t NotebookPage,
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

--TODO: can we combine this with creation of the AuiNotebook itself?
createNotebookEvents :: Frameworks t => AuiNotebook () -> Frame () -> Event t () -> Event t () -> Moment t (NotebookEvents t)
createNotebookEvents notebook frame1 eNewMenuItem eOpenMenuItem = do
    eNewFileDialogOk :: Event t FilePath <- openFileDialogOkEvent frame1 eNewMenuItem
    eNewFileDialogNotebookPage :: Event t NotebookPage <- (createSourcePage notebook) `mapIOreaction` eNewFileDialogOk
    eNewNotebookPage :: Event t NotebookPage <- (addEmptySourceTab notebook) `ioReaction`  eNewFileDialogNotebookPage
    eOpenFileDialogOk :: Event t FilePath <- openFileDialogOkEvent frame1 eOpenMenuItem
    eOpenFileDialogNotebookPage :: Event t NotebookPage <- (createSourcePage notebook) `mapIOreaction` eOpenFileDialogOk
    eOpenNotebookPage :: Event t NotebookPage <- (addSourcePage notebook) `ioReaction` eOpenFileDialogNotebookPage
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

    return $ NotebookEvents eNewFileDialogOk eNewFileDialogNotebookPage eNewNotebookPage
                            eOpenFileDialogOk eOpenFileDialogNotebookPage eOpenNotebookPage
                            eAddedNotebookPage eChangedNotebookPage eSwitchedNotebookPage
                            eCloseNotebookPage eLastClose bPages

