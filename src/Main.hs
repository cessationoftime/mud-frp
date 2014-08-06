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
import SourceEditor
import Controls.Mud.MapEditor
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
-- constants

type EventSource a = (AddHandler a, a -> IO ())

main :: IO ()
main = start mudEditor


{-----------------------------------------------------------------------------
    Game Logic
------------------------------------------------------------------------------}
-- main game function
mudEditor :: IO ()
mudEditor = do
    ff <- frame [ text       := "Editor for the Functional Interactive Fiction Engine (E-FIFE)"
                , bgcolor    := white
                , resizeable := False]


    status <- statusField [text := "Loading MUD Editor"]
    set ff [statusBar := [status]]

   -- t  <- timer ff [ interval   := 50 ]

    fileMenu  <- menuPane      [ text := "File" ]
    new   <- menuItem fileMenu [ text := "&New\tCtrl+N", help := "New file" ]
    save   <- menuItem fileMenu [ text := "&Save\tCtrl+S", help := "Save file" ]
    open <- menuItem fileMenu [ text      := "&Open\tCtrl+O"
                           , help      := "Open file"
               --            , checkable := True
                           ]
    menuLine fileMenu
    quit  <- menuQuit fileMenu [help := "Quit the ide"]


    (addDialogFinish,handlerDialogFinish) :: EventSource (FileDialog (), Int) <- newAddHandler
    (addGetDialogFile,handlerGetDialogFile) :: EventSource (FileDialog ()) <- newAddHandler
    set new   [on command := mudEditor]
    set quit  [on command := close ff]


    set ff [menuBar := [fileMenu]]

    (mapEditor,mapEditorNetwork) <- mapEditorIO ff
 --   cmb <- comboBox ff [items := ["item1","item2"]]
    sourceEditorCtrl <- sourceEditor ff []
    set open  [on command :=  (sourceEditorOpenFileDialog ff (\fd r -> handlerDialogFinish (fd,r)) sourceEditorCtrl)]

   -- set save  [on command :=  (sourceEditorFileSave sourceEditorCtrl  >> return ())]

    diffGo <- button ff [text := "Go"]

    let
      row1Layout = minsize (sz gridWidth gridHeight) $ widget mapEditor
      columnLayout = column 10 [row1Layout,widget diffGo, fill $  minsize (sz 400 400) $ widget sourceEditorCtrl]
     --   minsize (sz 600 400) $ wxhaskell setwidget diffV,widget diffGo]


    set ff [ layout  :=  margin 10 columnLayout]

    -- event network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            -- save event
            eSaveButton :: RB.Event t ()  <- event0 save command
            eOpenButton :: RB.Event t ()  <- event0 open command

            eDialogFinish :: RB.Event t (FileDialog (), Int)  <- fromAddHandler addDialogFinish
            eGetDialogFile :: RB.Event t FilePath  <- fromAddHandler (fileDialogGetPath `mapIO` addGetDialogFile)
            let eDialogOk  = filterE (\(_,r) -> r == wxID_OK) eDialogFinish
                eDialogCancel = filterE (\(_,r) -> r /= wxID_OK) eDialogFinish
              --  what :: RB.Event t (IO ()) = (\(fd,r) -> handlerGetDialogFile fd) <$> eDialogOk

            reactimate $ (\(fd,r) -> handlerGetDialogFile fd) <$> eDialogOk

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


