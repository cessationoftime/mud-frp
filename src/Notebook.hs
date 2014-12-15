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

module Notebook (
 createNotebook
,matchesNotebookPage
,NotebookOutputs(NotebookOutputs)
,NotebookPage(..)
,AuiNotebookChange(..)
) where
import Dialogs
import SourceEditor
import MapEditor
import RBWX.RBWX
import System.FilePath
import Data.List (find,partition)
import Data.Maybe (fromJust,listToMaybe,maybeToList)
import EventInputs (NotebookInput,NotebookChange (..), justOpenPage, justNewPage)
newNotebook :: Window a -> IO (AuiNotebook ())
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

data NotebookPage = SourceNotebookPage WindowId SourceEditorCtrl FilePath
data AuiNotebookChange = AuiNotebookChange { newPage :: (Maybe NotebookPage), oldPage :: (Maybe NotebookPage) }
emptyNotebookChange = AuiNotebookChange Nothing Nothing


class IsNotebookPage a where
  matchesNotebookPage :: a -> NotebookPage -> Bool

instance IsNotebookPage WindowId where
  matchesNotebookPage winId (SourceNotebookPage id _ _) = winId == id

instance IsNotebookPage WindowSelection where
  matchesNotebookPage (WindowSelection _ (Just (PageWindow winId _))) (SourceNotebookPage id _ _) = winId == id
  matchesNotebookPage (WindowSelection _ Nothing) (SourceNotebookPage id _ _) = False

-- TODO: fix the inconsistency of change events
data NotebookOutputs t = NotebookOutputs {
  -- | on newFileDialog OK button press, conducts page to be added
  newDialogPage  :: Event t NotebookPage,
  -- | on new page added to notebook, conducts the page added
  newNoteBookPage :: Event t NotebookPage,
  -- | on openFileDialog OK button press, conducts the page to be added
  openFileDialogOkNotebookPage  :: Event t NotebookPage,
  -- | on newly opened page added to notebook, conducts the page added
  openNoteBookPage  :: Event t NotebookPage,
   -- | on about to add page to notebook, conducts the page to be added
  addNoteBookPage :: Event t NotebookPage,
  -- | on page added to notebook, conducts the page added
  addedNoteBookPage :: Event t NotebookPage,
  -- | the currently selected NotebookPage is about to be changed or added (active tab), conducts the page changing from, should conduct Nothing when the first page is about to be opened
  changingNoteBookPage :: Event t AuiNotebookChange,
  -- | the currently selected NotebookPage has been changed or added (active tab), conducts the page changing to, conducts Nothing when the last page has closed
  changedNoteBookPage :: Event t AuiNotebookChange,
  -- | notebook page is about to close, conducts the page about to close
  closeNoteBookPage :: Event t AuiNotebookChange,
   -- | notebook page is about to close, conducts the page about to close
  closedNoteBookPage :: Event t (Maybe NotebookPage),
  -- | last page in the notebook is about to close, conducts the page about to close
  lastCloseNoteBookPage :: Event t AuiNotebookChange,
  -- | last page in the notebook has been closed, conducts the closed page
  lastClosedNoteBookPage :: Event t (Maybe NotebookPage),
  -- | pages currently in the notebook, and the last closed page
  pages :: Behavior t ([NotebookPage],Maybe NotebookPage)
  }

{-
createControl :: Frameworks t => Window d -> [a] -> (Window c -> IO ())  -> Moment t (b)
createControl frame1 inputs setupIO = let input = unite nbcs in do
    c <- liftIO $ newNotebook frame1
    liftIO $ setupIO c
    createOutputs c frame1 input
-}

createNotebook :: (Frameworks t)  => Frame () -> NotebookInput t -> (AuiNotebook () -> IO ())  -> Moment t (NotebookOutputs t)
createNotebook frame1 input setupIO = do
    notebook <- liftIO $ newNotebook frame1
    liftIO $ setupIO notebook
    outputs notebook frame1 input

--TODO: can we combine this with creation of the AuiNotebook itself?
outputs :: Frameworks t => AuiNotebook () -> Frame () -> NotebookInput t -> Moment t (NotebookOutputs t)
outputs notebook frame1 input = do
    let eNew = filterJust $ justNewPage <$> input
        eOpen = filterJust $ justOpenPage <$> input
    eNewFileDialogNotebookPage :: Event t NotebookPage <- (createSourcePage notebook) `mapIOreaction` eNew
    eNewNotebookPage :: Event t NotebookPage <- (addEmptySourceTab notebook) `ioReaction`  eNewFileDialogNotebookPage
    eOpenFileDialogNotebookPage :: Event t NotebookPage <- (createSourcePage notebook) `mapIOreaction` eOpen
    eOpenNotebookPage :: Event t NotebookPage <- (addSourcePage notebook) `ioReaction` eOpenFileDialogNotebookPage
    eCloseEventAuiNoteBook :: Event t EventAuiNotebook <- eCloseNotebookPage notebook
    eClosedEventAuiNoteBook :: Event t EventAuiNotebook <- eClosedNotebookPage notebook
    eChangingEventAuiNoteBook :: Event t EventAuiNotebook <- eChangingNotebookPage notebook
    eChangedEventAuiNoteBook :: Event t EventAuiNotebook <- eChangedNotebookPage notebook
    let eAddNotebookPage  :: Event t NotebookPage = eNewFileDialogNotebookPage `union` eOpenFileDialogNotebookPage
        eAddedNotebookPage  :: Event t NotebookPage = eNewNotebookPage `union` eOpenNotebookPage
        eChangedNotebookPage  :: Event t AuiNotebookChange =
          let eChanged = fromWindowSelection2NotebookPage eChangedEventAuiNoteBook
          in (emptyNotebookChange <$ eLastClosed) `union` eChanged
        eChangingNotebookPage :: Event t AuiNotebookChange =
          let eChanging = fromWindowSelection2NotebookPage eChangingEventAuiNoteBook
          in (emptyNotebookChange <$ eLastClose) `union` eChanging
        eCloseNotebookPage :: Event t AuiNotebookChange =
          fromWindowSelection2NotebookPage eCloseEventAuiNoteBook
        eClosedNotebookPage :: Event t (Maybe NotebookPage) =
          bClosedPage <@ eClosedEventAuiNoteBook

        findPage :: WindowSelection -> [NotebookPage] -> Maybe NotebookPage
        findPage  winSelect notes = find (matchesNotebookPage winSelect) notes

        makeChange :: ([NotebookPage],Maybe NotebookPage) -> EventAuiNotebook  -> AuiNotebookChange
        makeChange (a,b) ean =
          let new = newSel ean
              old = oldSel ean
              pageList :: [NotebookPage] = a ++ (maybeToList b)
          in  AuiNotebookChange (findPage new pageList) (findPage old pageList)

        fromWindowSelection2NotebookPage :: Event t EventAuiNotebook -> Event t AuiNotebookChange
        fromWindowSelection2NotebookPage e = (makeChange `fmap` bPages) <@> e

        filterNotPage :: EventAuiNotebook -> [NotebookPage] -> ([NotebookPage],Maybe NotebookPage)
        filterNotPage ean pages =
          let (m,n) = partition (not . matchesNotebookPage (newSel ean)) pages
          in  (m,listToMaybe n)


        -- | represents the current pages and the last closed page, but in random order
        bPages :: Behavior t ([NotebookPage],Maybe NotebookPage)
        bPages = accumB ([],Nothing) $
            (add `fmap` eAddNotebookPage) `union` (remove `fmap` eCloseEventAuiNoteBook {- this must be the close event, rather than the closed event. As the close event provides more information. -})
          where
            add nb (nbs,mbClosed) = (nb:nbs,mbClosed)
            remove nbSelect (nbs,mbClosed)  = filterNotPage nbSelect nbs

        bClosedPage :: Behavior t (Maybe NotebookPage)
        bClosedPage = (\(_,closedPage) -> closedPage) `fmap` bPages

        -- | check there is the x number of pages left in the notebook
        bPageLeft :: Int -> Behavior t Bool
        bPageLeft x = (\(nps,_) -> length nps == x) `fmap` bPages

        -- | event when last page is about to close
        eLastClose :: Event t AuiNotebookChange
        eLastClose = whenE (bPageLeft 1) eCloseNotebookPage

        -- | event when last page has closed
        eLastClosed :: Event t (Maybe NotebookPage)
        eLastClosed = whenE (bPageLeft 0) eClosedNotebookPage

    return $ NotebookOutputs eNewFileDialogNotebookPage eNewNotebookPage
                            eOpenFileDialogNotebookPage eOpenNotebookPage
                            eAddedNotebookPage eAddedNotebookPage
                            eChangingNotebookPage
                            eChangedNotebookPage
                            eCloseNotebookPage eClosedNotebookPage
                            eLastClose eLastClosed
                            bPages

