{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Graphics.UI.WX as WX
import Graphics.UI.WXCore as WXCore hiding (Event)
import Reactive.Banana as RB
import Reactive.Banana.WX   hiding (newEvent)
import System.Random
--import Graphics.UI.WXContrib.WXDiffCtrl
import Graphics.UI.WX.Classes
import Paths (getDataFile)
import WxAdditions
import Dialogs
import SourceEditor
import Controls.Mud.MapEditor
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
-- constants



main :: IO ()
main = start mudEditor


{-----------------------------------------------------------------------------
    Game Logic
------------------------------------------------------------------------------}
-- main game function
mudEditor :: IO ()
mudEditor = do
    frame1 <- frame [ text       := "Editor for the Functional Interactive Fiction Engine (E-FIFE)"
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

    contextMenu  <- menuPane      [ text := "Context" ]
    autoMenuItem  <- menuItem contextMenu      [ text := "Auto" ]

    openMenuItem <- menuItem fileMenu [ text      := "&Open\tCtrl+O"
                           , help      := "Open file"
               --            , checkable := True
                           ]
    menuSub fileMenu doMenu []
    menuLine fileMenu
    quit  <- menuQuit fileMenu [help := "Quit the ide"]


    set new   [on command := mudEditor]
    set quit  [on command := close frame1]


    set frame1 [menuBar := [fileMenu]]

    (mapEditor,mapEditorNetwork) <- mapEditorIO frame1
 --   cmb <- comboBox frame1 [items := ["item1","item2"]]
    sourceEditorCtrl <- sourceEditor frame1 []
  --  set open  [on command :=  (sourceEditorOpenFileDialog frame1 (\fd r -> handlerDialogFinish (fd,r)) sourceEditorCtrl)]

   -- set save  [on command :=  (sourceEditorFileSave sourceEditorCtrl  >> return ())]

    diffGo <- button frame1 [text := "Go"]

    let
      row1Layout = minsize (sz gridWidth gridHeight) $ widget mapEditor
      columnLayout = column 10 [row1Layout,widget diffGo, fill $  minsize (sz 400 400) $ widget sourceEditorCtrl]
     --   minsize (sz 600 400) $ wxhaskell setwidget diffV,widget diffGo]


    set frame1 [ layout  :=  margin 10 columnLayout]

    let openFileDialog :: ShowDialog
        openFileDialog = fileOpenDialog1 frame1 True True "Open File" [("Haskell file",["*.hs"])] "" ""


    -- event network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            -- save event
            eSaveButton :: RB.Event t ()  <- event0 save command

            -- open file event, load file into SourceEditor

            -- NOTE: DO NOT USE "command" event for menuItems. It WILL cause duplicate event firings on menus attached to the menubar, but not sub-menus or context menus.
            eOpenButton :: RB.Event t ()  <- event0 frame1 $ menu openMenuItem
            eDoItMenuButton :: RB.Event t ()  <- event0 frame1 $ menu doItMenuItem
            eAutoMenuItem <- event0 frame1 $ menu autoMenuItem

            reactimate $ (styledTextCtrlAddText sourceEditorCtrl) ("what the fuck\n") <$ (unions [eDoItMenuButton,eAutoMenuItem] )

            eGetDialogFinish <- eventDialogResult openFileDialog eOpenButton
            eGetDialogFile :: RB.Event t FilePath  <- eventDialogOkFilePath eGetDialogFinish
            reactimate $ (\fp -> sourceEditorLoadFile sourceEditorCtrl fp >> return ()) <$> eGetDialogFile

            -- diffButton event
            eDiffGo :: RB.Event t ()  <- event0 diffGo command
            bStyle :: Behavior t Bool <- behavior mapEditor tabTraversal

            let
                eStyle :: RB.Event t Bool = bStyle <@ eDiffGo
                eStringStyle :: RB.Event t String = show <$> eStyle
                setStyleText :: String -> IO () =(styledTextCtrlAddText sourceEditorCtrl)

            reactimate $ setStyleText <$> eStringStyle

            reactimate $ menuPopup contextMenu (Point 0 0) frame1 <$ eDiffGo

            -- keyboard events
            ekey   <- event1 mapEditor keyOnDownEvent
            let eKeyLeft  = filterE ((== KeyLeft ) . keyKey) ekey
                eKeyRight = filterE ((== KeyRight) . keyKey) ekey
                eKeyUp = filterE (\evK ->  ((keyKey evK)== KeyUp) ) ekey
                eKeyDown = filterE ((== KeyDown) . keyKey) ekey

            reactimate ((panelSetFocus mapEditor) <$ eKeyDown)

            -- status bar
         --   let bstatus :: Behavior t String
             --   bstatus = (\r -> "total Blocks: unknown") <$> bBlockMap
               -- bstatus = (\r -> "total Blocks: " ++ show (length r)) <$> bBlockMap
       --     set status [text :== "total Blocks: unknown"]

    network <- compile networkDescription
    actuate mapEditorNetwork
    actuate network
