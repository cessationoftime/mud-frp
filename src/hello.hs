{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore as WXCore
import Reactive.Banana
import Reactive.Banana.WX
import System.Random

import Paths (getDataFile)

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
-- constants
height, width, blockSize :: Int
blockSize = 40 -- blockSize is 40x40
height   = 640 -- window height
width    = 640 -- window width

chance   :: Double
chance   = 0.1

square, selectedSquare :: Bitmap ()
square    = bitmap $ getDataFile "square.png"
selectedSquare    = bitmap $ getDataFile "selectedSquare.png"

main :: IO ()
main = start mudEditor

{-----------------------------------------------------------------------------
    Game Logic
------------------------------------------------------------------------------}
-- main game function
mudEditor :: IO ()
mudEditor = do
    ff <- frame [ text       := "MUD Editor"
                , bgcolor    := white
                , resizeable := False ]

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
    set ff [ layout  := minsize (sz width height) $ widget pp ]
    set pp [ on (charKey '-') := set t [interval :~ \i -> i * 2] -- slow down timer interval
           , on (charKey '+') := set t [interval :~ \i -> max 10 (div i 2)] -- speed up timer interval
           ]

    -- event network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            -- timer
            etick  <- event0 t command

            -- keyboard events
            ekey   <- event1 pp keyboard
            let eKeyLeft  = filterE ((== KeyLeft ) . keyKey) ekey
                eKeyRight = filterE ((== KeyRight) . keyKey) ekey
                eKeyUp = filterE (\evK ->  ((keyKey evK)== KeyUp) ) ekey
                eKeyDown = filterE ((== KeyDown) . keyKey) ekey

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
                    (moveLeft <$ eKeyLeft) `union` (moveRight <$ eKeyRight)
                    `union` (moveUp <$ eKeyUp) `union` (moveDown <$ eKeyDown)

                moveLeft  (x,y) = (0 `max` (x - 1),y)
                moveRight (x,y) = ( (width `div` blockSize) `min` (x + 1),y)
                moveUp  (x,y) = (x,0 `max` (y - 1))
                moveDown (x,y) = (x,(height `div` blockSize) `min` (y + 1))

            -- toggle block draw
            let
                bBlockMap :: Behavior t BlockMap
                bBlockMap = accumB [((x,y),True) | x <- [0..16], y <- [0..16]] $
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
validBlockLocations = [[ (point i j) | i <- [1,blockSize..height]] | j <- [1,blockSize..width]]


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
