{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Reactive.Banana
import Reactive.Banana.WX
import Dialogs
import SourceEditor
import Controls.Mud.MapEditor
import RBWX.RBWX
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

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
      , bgcolor    := white
      , resizeable := False]


    status <- statusField [text := "Loading MUD Editor"]
    set frame1 [statusBar := [status]]

    -- t  <- timer ff [ interval   := 50 ]

    fileMenu  <- menuPane      [ text := "File" ]
    doMenu  <- menuPane      [ text := "Do" ]
    doItMenuItem   <- menuItem doMenu [ text := "Do It", help := "DoIt" ]
    new   <- menuItem fileMenu [ text := "&New\tCtrl+N", help := "New file" ]
    save   <- menuItem fileMenu [ text := "&Save\tCtrl+S", help := "Save file" ]



    openMenuItem <- menuItem fileMenu [ text      := "&Open\tCtrl+O"
                           , help      := "Open file"
               --            , checkable := True
                           ]
    menuSub fileMenu doMenu []
    menuLine fileMenu
    quit  <- menuQuit fileMenu [help := "Quit the ide"]
    (mapEditor,eAutoMenuItem) <- mapEditorIO frame1
    diffGo <- button frame1 [text := "Go"]
    sourceEditorCtrl <- liftIO $ sourceEditor frame1 []
    let row1Layout = minsize (sz gridWidth gridHeight) $ widget mapEditor
        columnLayout = column 10 [row1Layout,widget diffGo, fill $  minsize (sz 400 400) $ widget sourceEditorCtrl]
    --   minsize (sz 600 400) $ wxhaskell setwidget diffV,widget diffGo]
    set frame1 [ layout  :=  margin 10 columnLayout]
    set new   [on command := mainNetwork]
    set quit  [on command := close frame1]
    set frame1 [menuBar := [fileMenu]]
    -- Events

    -- save event
    eSaveButton :: Event t ()  <- event0 save command


    -- open file event, load file into SourceEditor

    -- NOTE: DO NOT USE "command" event for menuItems. It WILL cause duplicate event firings on menus attached to the menubar, but not sub-menus or context menus.
    eOpenButton :: Event t ()  <- event0 frame1 $ menu openMenuItem
    eDoItMenuButton :: Event t ()  <- event0 frame1 $ menu doItMenuItem


    reactimate $ (styledTextCtrlAddText sourceEditorCtrl) ("what the fuck\n") <$ (unions [eDoItMenuButton,eAutoMenuItem] )

    wireupSourceEditorOpenFileDialog frame1 sourceEditorCtrl eOpenButton

    -- diffButton event
    eDiffGo :: Event t ()  <- event0 diffGo command
    bStyle :: Behavior t Bool <- behavior mapEditor tabTraversal

    let
        eStyle :: Event t Bool = bStyle <@ eDiffGo
        eStringStyle :: Event t String = show <$> eStyle
        setStyleText :: String -> IO () =(styledTextCtrlAddText sourceEditorCtrl)

    reactimate $ setStyleText <$> eStringStyle


    -- status bar
    --   let bstatus :: Behavior t String
     --   bstatus = (\r -> "total Blocks: unknown") <$> bBlockMap
       -- bstatus = (\r -> "total Blocks: " ++ show (length r)) <$> bBlockMap
    --     set status [text :== "total Blocks: unknown"]

