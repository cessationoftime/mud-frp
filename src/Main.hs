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
    new   <- menuItem fileMenu [ text := "&New\tCtrl+N", help := "New file" ]
    save   <- menuItem fileMenu [ text := "&Save\tCtrl+S", help := "Save file" ]
    openMenuItem <- menuItem fileMenu [ text      := "&Open\tCtrl+O"
                           , help      := "Open file"
               --            , checkable := True
                           ]
    menuLine fileMenu
    quit  <- menuQuit fileMenu [help := "Quit the ide"]


   -- (addDialogFinish,handlerDialogFinish) :: EventSource (FileDialog (), Int) <- newAddHandler
    (addGetDialogFile,handlerGetDialogFile) :: EventSource (FileDialog ()) <- newAddHandler
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

    -- event network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            -- save event
            eSaveButton :: RB.Event t ()  <- event0 save command

            eDialogFinish :: RB.Event t (FileDialog (), Int)  <- eventDialogFinish openMenuItem frame1
            eGetDialogFile :: RB.Event t FilePath  <- fromAddHandler (fileDialogGetPath `mapIO` addGetDialogFile)
            let eDialogOk  = eventDialogOk eDialogFinish
              --  what :: RB.Event t (IO ()) = (\(fd,r) -> handlerGetDialogFile fd) <$> eDialogOk

            reactimate $ handlerGetDialogFile <$> eDialogOk

            reactimate $ (\fp -> sourceEditorLoadFile sourceEditorCtrl fp >> return ()) <$> eGetDialogFile


   --loaded <- fromMaybe (return False) $ sourceEditorLoadFile sourceEditorCtrl <$> mbFilePath
  --let mb =  (\fp -> (loaded,fp) ) `fmap` mbFilePath
  --return mb


          --  let openFile =
               -- bActiveFile :: Behavior t (Maybe (Bool,FilePath)) = accumB Nothing $ (openFile <@ eOpenButton)
            --reactimate $ (sourceEditorOpenFileDialog ff sourceEditorCtrl) <$> eOpenButton
         --   doActive Nothing = error "user must pick a file to load"
         --   doActive Just (True, x) = x
         --   doActive Just (False, x) = error "file failed to load"


           -- bOpenFileDialog :: Behavior t (Maybe (Bool,FilePath)) <- fromPoll $  sourceEditorOpenFileDialog ff sourceEditorCtrl

            -- diffButton event
            eDiffGo :: RB.Event t ()  <- event0 diffGo command
            bStyle :: Behavior t Bool <- behavior mapEditor tabTraversal

            let
                eStyle :: RB.Event t Bool = bStyle <@ eDiffGo
                eStringStyle :: RB.Event t String = show <$> eStyle
                setStyleText :: String -> IO () =(styledTextCtrlAddText sourceEditorCtrl)

            reactimate $ setStyleText <$> eStringStyle

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


