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

    game  <- menuPane      [ text := "Game" ]
    new   <- menuItem game [ text := "&New\tCtrl+N", help := "New game" ]
    open <- menuItem game [ text      := "&Open\tCtrl+O"
                           , help      := "Open file"
               --            , checkable := True
                           ]
    menuLine game
    quit  <- menuQuit game [help := "Quit the game"]

    set new   [on command := mudEditor]
    set quit  [on command := close ff]


    set ff [menuBar := [game]]

    (mapEditor,mapEditorNetwork) <- mapEditorIO ff
 --   cmb <- comboBox ff [items := ["item1","item2"]]
    sourceEditorCtrl <- sourceEditor ff []
    set open  [on command :=  (sourceEditorOpenFileDialog ff sourceEditorCtrl  >> return ())]

    diffGo <- button ff [text := "Go"]

    let
      row1Layout = minsize (sz gridWidth gridHeight) $ widget mapEditor
      columnLayout = column 10 [row1Layout,widget diffGo, fill $  minsize (sz 400 400) $ widget sourceEditorCtrl]
     --   minsize (sz 600 400) $ wxhaskell setwidget diffV,widget diffGo]


    set ff [ layout  :=  margin 10 columnLayout]

    -- event network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
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
