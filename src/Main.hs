import Reactive.Banana
import Reactive.Banana.WX
import Dialogs
import SourceEditor
import Controls.Mud.MapEditor
import RBWX.RBWX
import Aui
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
-- TODO: setup wxAUI
main :: IO ()
main = start mainNetwork


{-----------------------------------------------------------------------------
    Game Logic
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

    fileMenu  <- menuPane      [ text := "File" ]
    new   <- menuItem fileMenu [ text := "&New\tCtrl+N", help := "New file" ]
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
    (mapEditor,eAutoMenuItem) <- mapEditorIO frame1
    diffGo <- button frame1 [text := "Go"]
    sourceEditorCtrl <- sourceNotebook frame1
    added1 <- liftIO $ auiManagerAddPane aui sourceEditorCtrl wxCENTER "Source Pane"
    added2 <- liftIO $ auiManagerAddPane aui mapEditor wxTOP "map Pane"
    added3 <- liftIO $ auiManagerAddPane aui diffGo wxRIGHT "Diff Button"
    set new   [on command := mainNetwork]
    set quit  [on command := close frame1]
    set frame1 [menuBar := [fileMenu]]
    -- Events


    -- NOTE: DO NOT USE "command" event for menuItems. It WILL cause duplicate event firings on menus attached to the menubar, but not sub-menus or context menus.
    eSaveButton :: Event t ()  <- event0 frame1 $ menu saveMenuItem
    eOpenButton :: Event t ()  <- event0 frame1 $ menu openMenuItem
    eDoItMenuButton :: Event t ()  <- event0 frame1 $ menu doItMenuItem

    liftIO $ auiManagerUpdate aui



    -- status bar
    --   let bstatus :: Behavior t String
     --   bstatus = (\r -> "total Blocks: unknown") <$> bBlockMap
       -- bstatus = (\r -> "total Blocks: " ++ show (length r)) <$> bBlockMap
    --     set status [text :== "total Blocks: unknown"]


