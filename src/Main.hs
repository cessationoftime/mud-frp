import Dialogs
import SourceEditor
import MapEditor
import RBWX.RBWX
import Notebook
import Data.List (find)
import Data.Maybe (fromMaybe,maybeToList)
import WorkspaceBrowser
import EventInputs
import AuiManager

-- TODO: Haskelletor

-- TODO: create open workspace (*.n6)
-- TODO: and add existing project (*.n6proj,*.nix, *.cabal) menu item, to the workspace context menu

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
-- TODO: setup wxAUI
main :: IO ()
main = start mainNetwork

--testi
{-----------------------------------------------------------------------------
    Compile and actuate Reactive Network
------------------------------------------------------------------------------}
-- main
mainNetwork :: IO ()
mainNetwork = do

 --   cmb <- comboBox frame1 [items := ["item1","item2"]]

    network <- compile networkDescription
    actuate network

 -- event network
networkDescription :: forall t. Frameworks t => Moment t ()
networkDescription = do
    -- Layout
    frame1 <- liftIO $ frameMax [ text  := "Editor for the Functional Interactive Fiction Engine (E-FIFE)"
      , resizeable := True]

    status <- statusField [text := "Loading MUD Editor"]
    setM frame1 [statusBar := [status]]

    -- Menu layout
    fileMenu  <- menuPane      [ text := "File" ]
    newMenuItem   <- menuItem fileMenu [ text := "&New\tCtrl+N", help := "New file" ]
    openMenuItem <- menuItem fileMenu [ text      := "&Open\tCtrl+O"
                           , help      := "Open file"
               --            , checkable := True
                           ]

    saveMenuItem   <- menuItem fileMenu [ text := "&Save\tCtrl+S", help := "Save file" ]
    saveAllMenuItem   <- menuItem fileMenu [ text := "SaveAll", help := "Save All Files" ]

    doMenu  <- menuPane      [ text := "Do" ]
    doItMenuItem   <- menuItem doMenu [ text := "Do It", help := "DoIt" ]
    menuSub fileMenu doMenu []

    menuLine fileMenu
    quit  <- menuQuit fileMenu [help := "Quit the ide"]

    -- [[[[ AuiManager setup
  --  (auiEvent1 :: Event t [(Window (), Int, String)] ,addPanehandler) <- newEvent



    aui <- createAuiManager frame1
   -- auiManagerOutputs <- outputs aui []
    let addPane w b c =  withUnderlying aui (\a -> auiManagerAddPane a w b c >> return ())
  --  let addPane b c w =  auiManagerAddPane aui w b c >> return ()


    ---- AuiManager setup ]
-----------------------------------------------------------------


       -- NOTE: DO NOT USE "command" event for menuItems. It WILL cause duplicate event firings on menus attached to the menubar, but not sub-menus or context menus.
    eSaveMenuItem :: Event t ()  <- event0 frame1 $ menu saveMenuItem
    eSaveAllMenuItem :: Event t ()  <- event0 frame1 $ menu saveAllMenuItem
    eOpenMenuItem :: Event t ()  <- event0 frame1 $ menu openMenuItem
    eNewMenuItem :: Event t ()  <- event0 frame1 $ menu newMenuItem
    eDoItMenuButton :: Event t ()  <- event0 frame1 $ menu doItMenuItem

    --  [[[[ MapEditor
    (mapEditor,eAutoContextItem) <- mapEditorIO frame1
    _ <- liftIO $ addPane (objectCast mapEditor) wxTOP  "Map Pane"
    --  MapEditor ]]]]

    -- [[[[ Notebook

    eNewDialogOk <- fileDialogOkEvent New "NewSource.hs" [Haskell] frame1 eNewMenuItem
    eOpenDialogOk <- fileDialogOkEvent Open "" [Haskell] frame1 eOpenMenuItem

    let notebookInputs = unions [(NewPage <$> eNewDialogOk),(OpenPage <$> eOpenDialogOk),
                                 (Save <$ eSaveMenuItem),(SaveAll <$ eSaveAllMenuItem)]



    NotebookOutputs _ _ _ _ _ _
      eChangingNotebookPage
      eChangedNotebookPage eCloseNotebookPage eClosedNotebookPage eLastClose eLastClosed
      bPages <- createNotebook frame1 notebookInputs (\n -> addPane (objectCast n) wxCENTER "Source Pane")

     --  Notebook ]]]]

    -- [[[[ TreeCtrl

    workspaceBrowserOutputs <- createWorkspaceBrowser frame1 never (\(WorkspaceBrowser p) -> addPane (objectCast p) wxRIGHT "Workspace Browser")

     -- TreeCtrl ]]]]

   -- set new   [on command := mainNetwork]
    setM quit  [on command := close frame1]
    setM frame1 [menuBar := [fileMenu]]



    -- [[[[ AuiManager setup
  --  (adder1,handler1) <- liftIO newAddHandler
  --  auiEvent1 <- fromAddHandler adder1

  --  (adder2,handler2) <- liftIO newAddHandler
  --  auiEvent2 <- fromAddHandler adder2


   -- let auiManagerStartupInput = AuiManagerInputs auiEvent1 auiEvent2


  --  aui <- createAuiManager frame1



    liftIO $ withUnderlying aui auiManagerUpdate

    ---- AuiManager setup ]]]]

    -- Events




    let bActiveNBPage = stepper Nothing (newPage `fmap` eChangedNotebookPage)
    let eSaveNBPage :: Event t (Maybe NotebookPage) =  bActiveNBPage <@ eSaveMenuItem

        doSave :: Maybe NotebookPage -> IO()
        doSave (Just (SourceNotebookPage _ ctrl fp)) = styledTextCtrlSaveFile ctrl fp >> return ()
        doSave Nothing = return ()

    reactimate $ doSave <$> eSaveNBPage

    let showNBMaybe :: Maybe NotebookPage -> FilePath
        showNBMaybe (Just (SourceNotebookPage _ _ fp) ) = fp
        showNBMaybe _ = ""

    sink status [text :== showNBMaybe <$> bActiveNBPage  ]

  --      bTotal :: Behavior t Int
  --      bTotal = accumB 0 $ (+1) <$ eClosedNotebookPage

  --  sink status [text :== show <$> bTotal ]

    return ()
    -- diffButton event
    --eDiffGo :: Event t ()  <- event0 diffGo command
    --bStyle :: Behavior t Bool <- behavior mapEditor tabTraversal
{-
    let
        eStyle :: Event t Bool = bStyle <@ eDiffGo
        eStringStyle :: Event t String = show <$> eStyle
        setStyleText :: String -> IO () =(styledTextCtrlAddText sourceEditorCtrlOfPage)

    reactimate $ setStyleText <$> eStringStyle
         reactimate $ (styledTextCtrlAddText sourceEditorCtrlOfPage) ("what the fuck\n") <$ (unions [eDoItMenuButton,eAutoContextItem] )
-}


  -- return ()

    -- status bar
     --  let bstatus :: Behavior t String
    --   bstatus = (\r -> "total Blocks: unknown") <$> bBlockMap
    --   bstatus = (\r -> "total Blocks: " ++ show (length r)) <$> bBlockMap



