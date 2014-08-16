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


sourceNotebook :: forall t a. Frameworks t => Frame a -> Moment t (AuiNotebook ())
sourceNotebook frame1 = do
-- create the notebook off-window to avoid flicker
  (Size x y) <- liftIO $ windowGetClientSize frame1
  notebook <- liftIO $ auiNotebookCreate frame1 wxID_ANY (Point x y) (Size 430 200) (wxAUI_NB_DEFAULT_STYLE .+. wxAUI_NB_TAB_EXTERNAL_MOVE .+. wxNO_BORDER)
  -- Freeze
  liftIO $ windowFreeze notebook


  sourceEditorCtrl <- liftIO $ sourceEditor frame1 []
  added <- liftIO $ auiNotebookAddPageWithBitmap notebook sourceEditorCtrl "source page1" False nullBitmap

  {-
  reactimate $ (styledTextCtrlAddText sourceEditorCtrl) ("what the fuck\n") <$ (unions [eDoItMenuButton,eAutoMenuItem] )

  bFilePath :: Behavior t (Maybe FilePath) <- wireupSourceEditorOpenFileDialog frame1 sourceEditorCtrl eOpenButton
  let eSaveFilePath :: Event t (Maybe FilePath) =  bFilePath <@ eSaveButton
      doSave :: StyledTextCtrl () -> Maybe FilePath -> IO()
      doSave s  (Just x) = styledTextCtrlSaveFile s x >> return ()
      doSave s  Nothing = return ()
  reactimate $ doSave sourceEditorCtrl <$> eSaveFilePath


    -- diffButton event
  eDiffGo :: Event t ()  <- event0 diffGo command
  bStyle :: Behavior t Bool <- behavior mapEditor tabTraversal

  let
      eStyle :: Event t Bool = bStyle <@ eDiffGo
      eStringStyle :: Event t String = show <$> eStyle
      setStyleText :: String -> IO () =(styledTextCtrlAddText sourceEditorCtrl)
  reactimate $ setStyleText <$> eStringStyle

  -}
  liftIO $ windowThaw notebook
  return (notebook)
