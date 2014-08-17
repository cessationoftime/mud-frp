{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"
-----------------------------------------------------------------------------
--
-- Module      :  Controls.Mud.MapEditor
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Controls.Mud.MapEditor (mapEditorIO,gridHeight,gridWidth) where

import Reactive.Banana
import Reactive.Banana.WX
import System.Random
import Paths (getDataFile)
import RBWX.RBWX
import WxAdditions
type BlockPosition = (Int,Int)
type BlockMap = [(BlockPosition,Bool)]
type Position = Point2 Int

--mapEditor ::  Window a -> [Prop (Panel ())] -> IO (Panel ())
--mapEditor framef prop =  panel framef prop

gridHeight, gridWidth, blockSize :: Int
blockSize = 40 -- blockSize is 40x40
gridHeight   = 430 -- window height
gridWidth    = 430 -- window width
blocksHorizontal = gridWidth `div` blockSize
blocksVertical = gridHeight `div` blockSize

square, selectedSquare :: Bitmap ()
square    = bitmap $ getDataFile "square.png"
selectedSquare    = bitmap $ getDataFile "selectedSquare.png"

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


mapEditorIO :: forall t a. Frameworks t => Window a -> Moment t (Panel (), Event t ())
mapEditorIO window = do
    -- Layout
    t <- timer window [ interval   := 50 ]
    mapEditorPanel <- panel window []
    contextMenu  <-  menuPane      [ text := "Context" ]
    autoMenuItem  <- menuItem contextMenu      [ text := "Auto" ]
    wireupContextMenu mapEditorPanel contextMenu
    eAutoMenuItem <- event0 window $ menu autoMenuItem
    -- Events
    ekey   <- event1 mapEditorPanel keyOnDownEvent
    emouse <- event1 mapEditorPanel mouse
    -- timer
    etick  <- event0 t command

    let eKeyLeft  = filterE ((== KeyLeft ) . keyKey) ekey
        eKeyRight = filterE ((== KeyRight) . keyKey) ekey
        eKeyUp = filterE (\evK ->  ((keyKey evK)== KeyUp) ) ekey
        eKeyDown = filterE ((== KeyDown) . keyKey) ekey

        moveLeft  (x,y) = (0 `max` (x - 1),y)
        moveRight (x,y) = ( blocksHorizontal `min` (x + 1),y)
        moveUp  (x,y) = (x,0 `max` (y - 1))
        moveDown (x,y) = (x,blocksVertical `min` (y + 1))



        bActiveBlock :: Behavior t BlockPosition
        bActiveBlock = accumB (0,0) $
                     unions [moveLeft <$ eKeyLeft, moveRight <$ eKeyRight, moveUp <$ eKeyUp,moveDown <$ eKeyDown]

       -- mouse events
        checkMouseLeftDown :: EventMouse -> Bool
        checkMouseLeftDown (MouseLeftDown _ _)  = True
        checkMouseLeftDown _ = False

        eClickLeft  = filterE checkMouseLeftDown emouse

     -- toggle block draw
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

    reactimate ((panelSetFocus mapEditorPanel) <$ eKeyDown)

    -- draw the game state
    sink mapEditorPanel [on paint :== stepper (\_dc _ -> return ()) $ (drawGameState <$> bActiveBlock <*> bBlockMap) <@ etick]
    reactimate $ repaint mapEditorPanel <$ etick
    return (mapEditorPanel,eAutoMenuItem)
