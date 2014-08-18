import Reactive.Banana
import Reactive.Banana.WX
import Dialogs
import SourceEditor
import Controls.Mud.MapEditor
import RBWX.RBWX
import Aui
import WxAdditions
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
-- TODO: setup wxAUI
main :: IO ()
main = start mainNetwork


{-----------------------------------------------------------------------------
    Compile and actuate Reactive Network
------------------------------------------------------------------------------}
-- main
mainNetwork :: IO ()
mainNetwork = do

 --   cmb <- comboBox frame1 [items := ["item1","item2"]]

  --  set open  [on command :=  (sourceEditorOpenFileDialog frame1 (\fd r -> handlerDialogFinish (fd,r)) sourceEditorCtrl)]

   -- set save  [on command :=  (sourceEditorFileSave sourceEditorCtrl  >> return ())]


    network <- compile networkDescription
    actuate network


 -- event network
networkDescription :: forall t. Frameworks t => Moment t ()
networkDescription = do
    -- Layout
    frame1 <- frame [ text  := "Editor for the Functional Interactive Fiction Engine (E-FIFE)"
      , resizeable := True]

    aui <- liftIO  $ auiManagerCreate frame1 wxAUI_MGR_DEFAULT

    status <- statusField [text := "Loading MUD Editor"]
    set frame1 [statusBar := [status]]
    diffGo <- button frame1 [text := "Go"]
    -- Menu layout
    fileMenu  <- menuPane      [ text := "File" ]
    newMenuItem   <- menuItem fileMenu [ text := "&New\tCtrl+N", help := "New file" ]
    openMenuItem <- menuItem fileMenu [ text      := "&Open\tCtrl+O"
                           , help      := "Open file"
               --            , checkable := True
                           ]
    saveMenuItem   <- menuItem fileMenu [ text := "&Save\tCtrl+S", help := "Save file" ]

    doMenu  <- menuPane      [ text := "Do" ]
    doItMenuItem   <- menuItem doMenu [ text := "Do It", help := "DoIt" ]
    menuSub fileMenu doMenu []

    menuLine fileMenu
    quit  <- menuQuit fileMenu [help := "Quit the ide"]

    -- Menu events
       -- NOTE: DO NOT USE "command" event for menuItems. It WILL cause duplicate event firings on menus attached to the menubar, but not sub-menus or context menus.
    eSaveMenuItem :: Event t ()  <- event0 frame1 $ menu saveMenuItem
    eOpenMenuItem :: Event t ()  <- event0 frame1 $ menu openMenuItem
    eNewMenuItem :: Event t ()  <- event0 frame1 $ menu newMenuItem
    eDoItMenuButton :: Event t ()  <- event0 frame1 $ menu doItMenuItem

    --Context Menu
    (mapEditor,eAutoContextItem) <- mapEditorIO frame1

    -- Notebook layout

    notebook <- liftIO $ newNotebook frame1
    added1 <- liftIO $ auiManagerAddPane aui notebook wxCENTER "Source Pane"
    added2 <- liftIO $ auiManagerAddPane aui mapEditor wxTOP "map Pane"
    added3 <- liftIO $ auiManagerAddPane aui diffGo wxRIGHT "Diff Button"
   -- set new   [on command := mainNetwork]
    set quit  [on command := close frame1]
    set frame1 [menuBar := [fileMenu]]
    -- Events




    liftIO $ auiManagerUpdate aui

    eNewFileOk :: Event t FilePath  <- openFileDialogOkEvent frame1 eNewMenuItem
    eNewNotebookPage :: Event t NotebookPage <- addNewSourcePage notebook `mapIOreaction` eNewFileOk

    eOpenFileOk :: Event t FilePath <- openFileDialogOkEvent frame1 eOpenMenuItem
    eOpenNotebookPage :: Event t NotebookPage <- addSourcePage notebook `mapIOreaction` eOpenFileOk

    eCloseNotebookPage :: Event t EventAuiNotebook <- event1 notebook auiNotebookOnPageClosedEvent

    let eLoadNotebookPage :: Event t NotebookPage =  eNewNotebookPage `union` eOpenNotebookPage

    let eSaveFilePath :: Event t (Maybe FilePath) =  bFilePath <@ eSaveMenuItem
        bFilePath :: Behavior t (Maybe FilePath) =  stepper Nothing (Just <$> eOpenFileOk)
        doSave :: StyledTextCtrl () -> Maybe FilePath -> IO()
        doSave s  (Just x) = styledTextCtrlSaveFile s x >> return ()
        doSave s  Nothing = return ()

        ePages :: Event t [NotebookPage]
        ePages = accumE [] $
            (add <$> eLoadNotebookPage) `union` (remove <$> eCloseNotebookPage)
          where
            add  nb nbs = nb:nbs
            remove (EventAuiNotebook _ newSel _) nbs =  filter (isNotebookPage newSel) nbs


    -- diffButton event
    eDiffGo :: Event t ()  <- event0 diffGo command
    bStyle :: Behavior t Bool <- behavior mapEditor tabTraversal
{-
    let
        eStyle :: Event t Bool = bStyle <@ eDiffGo
        eStringStyle :: Event t String = show <$> eStyle
        setStyleText :: String -> IO () =(styledTextCtrlAddText sourceEditorCtrlOfPage)

    reactimate $ setStyleText <$> eStringStyle
         reactimate $ (styledTextCtrlAddText sourceEditorCtrlOfPage) ("what the fuck\n") <$ (unions [eDoItMenuButton,eAutoContextItem] )

    reactimate $ doSave sourceEditorCtrlOfPage <$> eSaveFilePath
-}
    return ()

    -- status bar
    --   let bstatus :: Behavior t String
     --   bstatus = (\r -> "total Blocks: unknown") <$> bBlockMap
       -- bstatus = (\r -> "total Blocks: " ++ show (length r)) <$> bBlockMap
    --     set status [text :== "total Blocks: unknown"]


