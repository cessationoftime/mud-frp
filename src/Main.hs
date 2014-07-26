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
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
-- constants
gridHeight, gridWidth, blockSize :: Int
blockSize = 40 -- blockSize is 40x40
gridHeight   = 430 -- window height
gridWidth    = 430 -- window width
blocksHorizontal = gridWidth `div` blockSize
blocksVertical = gridHeight `div` blockSize

square, selectedSquare :: Bitmap ()
square    = bitmap $ getDataFile "square.png"
selectedSquare    = bitmap $ getDataFile "selectedSquare.png"

main :: IO ()
main = start mudEditor

whatDiffer :: FilePath -> FilePath -> IO [String]
whatDiffer fp1 fp2 = return ["testing123","test456"]


colorscheme = [ ( wxSTC_HA_DEFAULT, rgb 0 0 0 )
                  , ( wxSTC_HA_IDENTIFIER, rgb 0 0 0 )
                  , ( wxSTC_HA_KEYWORD, rgb 0 0 255 )
                  , ( wxSTC_HA_NUMBER, rgb 100 100 100 )
                  , ( wxSTC_HA_STRING, rgb 100 100 200 )
                  , ( wxSTC_HA_CHARACTER, rgb 0 100 200 )
                  , ( wxSTC_HA_CLASS, rgb 255 0 255 )
                  , ( wxSTC_HA_MODULE, rgb 255 0 0 )
                  , ( wxSTC_HA_CAPITAL, rgb 0 255 0 )
                  , ( wxSTC_HA_DATA, rgb 255 0 0 )
                  , ( wxSTC_HA_IMPORT, rgb 150 0 200 )
                  , ( wxSTC_HA_OPERATOR, rgb 256 0 0 )
                  , ( wxSTC_HA_INSTANCE, rgb 150 61 90 )
                  , ( wxSTC_HA_COMMENTLINE, rgb 10 80 100 )
                  , ( wxSTC_HA_COMMENTBLOCK, rgb 0 60 0 )
                  , ( wxSTC_HA_COMMENTBLOCK2, rgb 0 30 0 )
                  , ( wxSTC_HA_COMMENTBLOCK3, rgb 0 10 0 )
                  ]

keywords = "as case class data default deriving do else hiding if import " ++
           "in infix infixl infixr instance let module newtype of qualified" ++
           "then type where"

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

    t  <- timer ff [ interval   := 50 ]

    game  <- menuPane      [ text := "Game" ]
    new   <- menuItem game [ text := "&New\tCtrl+N", help := "New game" ]
    pause <- menuItem game [ text      := "&Pause\tCtrl+P"
                           , help      := "Pause game"
                           , checkable := True
                           ]
    menuLine game
    quit  <- menuQuit game [help := "Quit the game"]

    set new   [on command := mudEditor]
    set pause [on command := set t [enabled :~ not]]
    set quit  [on command := close ff]

    set ff [menuBar := [game]]

    pp <- panel ff []
 --   cmb <- comboBox ff [items := ["item1","item2"]]





    styledTxt <- styledTextCtrl ff []
    styledTextCtrlLoadFile styledTxt "/home/cvanvranken/Desktop/STCLexer.hs"
    styledTextCtrlStyleClearAll styledTxt
    styledTextCtrlSetLexer styledTxt wxSTC_LEX_HASKELL
    styledTextCtrlSetKeyWords styledTxt 0 keywords
    let fontstyle = fontFixed { _fontFace = "Monospace" }
    (font, _) <- fontCreateFromStyle fontstyle
    mapM_ (\style -> styledTextCtrlStyleSetFont styledTxt style font) [0..wxSTC_STYLE_LASTPREDEFINED]
    sequence_ [styledTextCtrlStyleSetForeground styledTxt k c | (k, c) <- colorscheme]

    --diffV <- diffViewer ff [diffFiles := ("/home/cvanvranken/Desktop/unt1","/home/cvanvranken/Desktop/unt2")]
    diffGo <- button ff [text := "Diff Files"]

    let
      row1Layout = minsize (sz gridWidth gridHeight) $ widget pp
      columnLayout = column 10 [row1Layout,widget diffGo, fill $  minsize (sz 400 400) $ widget styledTxt]
     --   minsize (sz 600 400) $ wxhaskell setwidget diffV,widget diffGo]


    set ff [ layout  :=  margin 10 columnLayout]
    set pp [on (charKey '-') := set t [interval :~ \i -> i * 2] -- slow down timer interval
           , on (charKey '+') := set t [interval :~ \i -> max 10 (div i 2)] -- speed up timer interval
           ]
-- reactimate :: Frameworks t => Event t (IO ()) -> Moment t ()
-- styledTextCtrlSetText :: forall a. StyledTextCtrl a -> String -> IO ()
    -- event network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            -- timer
            etick  <- event0 t command

            -- diffButton event
            eDiffGo :: RB.Event t ()  <- event0 diffGo command
       --     eInt :: Reactive.Banana.Event t Int <- 1 <$ eDiffGo
            bStyle :: Behavior t Bool <- behavior pp tabTraversal



            let
                eStyle :: RB.Event t Bool = bStyle <@ eDiffGo
                eStringStyle :: RB.Event t String = show <$> eStyle
                setStyleText :: String -> IO () =(styledTextCtrlAddText styledTxt)


          --       executeDiff = Just whatDiffer
          --  sink diffV [diffFn :== stepper executeDiff (executeDiff <$ eDiffGo)]
         --   reactimate $ setStyleText <$> eStringStyle

            reactimate $ setStyleText <$> eStringStyle

            -- navi events
          --  eNavi <- event1 pp keyOnDownEvent
         --   reactimate $ (styledTextCtrlAddText styledTxt "Navi!\n") <$ eNavi

            -- keyboard events
            ekey   <- event1 pp keyOnDownEvent
            let eKeyLeft  = filterE ((== KeyLeft ) . keyKey) ekey
                eKeyRight = filterE ((== KeyRight) . keyKey) ekey
                eKeyUp = filterE (\evK ->  ((keyKey evK)== KeyUp) ) ekey
                eKeyDown = filterE ((== KeyDown) . keyKey) ekey

            reactimate ((panelSetFocus pp) <$ eKeyDown)

            -- mouse events

            emouse <- event1 pp mouse
            let checkMouseLeftDown :: EventMouse -> Bool
                checkMouseLeftDown (MouseLeftDown _ _)  = True
                checkMouseLeftDown _ = False

                eClickLeft  = filterE checkMouseLeftDown emouse


            -- active block position
            let
                bActiveBlock :: Behavior t BlockPosition
                bActiveBlock = accumB (0,0) $
                    unions [moveLeft <$ eKeyLeft, moveRight <$ eKeyRight,
                    moveUp <$ eKeyUp,moveDown <$ eKeyDown]

                moveLeft  (x,y) = (0 `max` (x - 1),y)
                moveRight (x,y) = ( blocksHorizontal `min` (x + 1),y)
                moveUp  (x,y) = (x,0 `max` (y - 1))
                moveDown (x,y) = (x,blocksVertical `min` (y + 1))

            -- toggle block draw
            let
                bBlockMap :: Behavior t BlockMap
                bBlockMap = accumB [((x,y),True) | x <- [0..blocksHorizontal], y <- [0..blocksVertical]] $
                    (toggleBlockMap <$> eClickLeft)

                toggleBlockMap :: EventMouse -> BlockMap -> BlockMap
                toggleBlockMap evMouse blockMap = (toggleBlockDrawing $ mousePos evMouse) `map` blockMap

                nearClickPosition :: Position -> Position -> Bool
                nearClickPosition (Point xBlock yBlock) (Point xClick yClick) =
                  let xMax = xBlock + 30
                      yMax = yBlock + 30
                  in (xClick > xBlock && xClick < xMax) && (yClick > yBlock && yClick < yMax)

                toggleBlockDrawing :: Position ->(BlockPosition,Bool)-> (BlockPosition,Bool)
                toggleBlockDrawing clickPos (pos,d) | nearClickPosition (getBlockLoc pos) clickPos  = (pos,not d)
                toggleBlockDrawing _ x = x

                --

            -- draw the game state
            sink pp [on paint :== stepper (\_dc _ -> return ()) $
                     (drawGameState <$> bActiveBlock <*> bBlockMap) <@ etick]
            reactimate $ repaint pp <$ etick

            -- status bar
            let bstatus :: Behavior t String
                bstatus = (\r -> "total Blocks: " ++ show (length r)) <$> bBlockMap
            sink status [text :== bstatus]

    network <- compile networkDescription
    actuate network


-- rock logic
type BlockPosition = (Int,Int)
type BlockMap = [(BlockPosition,Bool)]
type Position = Point2 Int

drawnBlockLocations  :: BlockMap -> [BlockPosition]
drawnBlockLocations bm = fst `map` (snd `filter` bm)

validBlockLocations :: [[Position]]
validBlockLocations = [[ (point i j) | i <- [1,blockSize..gridHeight]] | j <- [1,blockSize..gridWidth]]


getBlockLoc :: BlockPosition -> Position
getBlockLoc x = ((validBlockLocations !! (snd x)) !! (fst x) )

--blockLocations :: [Position]
--blockLocations =   getBlockLoc `map` drawnBlockLocations

-- draw game state
drawGameState :: BlockPosition -> BlockMap -> DC a -> b -> IO ()
drawGameState activeBlock blockMap dc _view = do
    let
    mapM_  (drawBlock dc activeBlock) (drawnBlockLocations blockMap)

drawBlock :: DC a -> BlockPosition -> BlockPosition -> IO ()
drawBlock dc activePos drawPos  =
  drawBitmap dc squareImage (getBlockLoc drawPos) True []
  where squareImage = if activePos == drawPos then selectedSquare else square



