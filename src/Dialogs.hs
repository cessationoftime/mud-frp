{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Dialogs
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

module Dialogs where

import Reactive.Banana
import Reactive.Banana.WX
import Reactive.Banana.Frameworks

import Graphics.UI.WX.Classes
import qualified Graphics.UI.WX as WX
import Graphics.UI.WX hiding (Event, Attr)
import qualified Graphics.UI.WXCore as WXCore

-- observe "key up" events (many thanks to Abu Alam)
-- this should probably be in the wxHaskell library
-- keyboardUp :: WX.Event (Window a) (EventKey -> IO ())
-- keyboardUp = WX.newEvent "keyboardUp" WXCore.windowGetOnKeyUp WXCore.windowOnKeyUp

--eDialogFinish :: RB.Event t (FileDialog (), Int)  <- fromAddHandler addDialogFinish
--set open  [on command :=  (sourceEditorOpenFileDialog ff (\fd r -> handlerDialogFinish (fd,r)) sourceEditorCtrl)]

-- (addDialogFinish,handlerDialogFinish) :: EventSource (FileDialog (), Int) <- newAddHandler
-- (addGetDialogFile,handlerGetDialogFile) :: EventSource (FileDialog ()) <- newAddHandler

--   open <- menuItem fileMenu [ text := "&Open\tCtrl+O", help      := "Open file"  ]

--   eGetDialogFile :: RB.Event t FilePath  <- fromAddHandler (fileDialogGetPath `mapIO` addGetDialogFile)


--         reactimate $ (\(fd,r) -> handlerGetDialogFile fd) <$> eDialogOk
--         reactimate $ (\fp -> sourceEditorLoadFile sourceEditorCtrl fp >> return ()) <$> eGetDialogFile

type EventSource a = (AddHandler a, a -> IO ())

type OpenDialogResulter a = WXCore.FileDialog () -> Int -> IO a

fileOpenDialog1 :: Window a -> OpenDialogResulter () -> Bool -> Bool -> String -> [(String,[String])] -> FilePath -> FilePath -> IO ()
fileOpenDialog1 parent result rememberCurrentDir allowReadOnly message wildcards directory filename
  = WXCore.fileDialog parent result flags message wildcards directory filename
  where
    flags
      = WXCore.wxOPEN .+. (if rememberCurrentDir then WXCore.wxCHANGE_DIR else 0) .+. (if allowReadOnly then 0 else WXCore.wxHIDE_READONLY)


-- | Show an openFileDialog and get a FilePath. Load the file pointed to by
-- the path into the sourceEditor
openFileDialog :: Window a -> OpenDialogResulter () -> IO ()
openFileDialog window resulter =
  fileOpenDialog1 window resulter True True "Open File" [("Haskell file",["*.hs"])] "" ""


-- | Event that occurs when the /user/ changed
-- the selection marker in a list box widget.
eventDialogFinish :: (Frameworks t, Commanding c) => c -> Window a -> Moment t (Event t (WXCore.FileDialog (), Int))
eventDialogFinish commander w = do
    addDialogFinish <- liftIO $ commandOpenFileDialog w commander
    --addHandler <- liftIO $ event1ToAddHandler dialog (event0ToEvent1 select)
    fromAddHandler addDialogFinish

eventDialogButtonClick :: Int ->  Event t (WXCore.FileDialog (), Int) ->  Event t (WXCore.FileDialog ())
eventDialogButtonClick wxId eDialogFinish  = fst `fmap` eClick
   where eClick = filterE (\(_,r) -> r == wxId) eDialogFinish

eventDialogOk :: Event t (WXCore.FileDialog (), Int) ->  Event t (WXCore.FileDialog ())
eventDialogOk = eventDialogButtonClick WXCore.wxID_OK

eventDialogCancel :: Event t (WXCore.FileDialog (), Int) ->  Event t (WXCore.FileDialog ())
eventDialogCancel = eventDialogButtonClick WXCore.wxID_CANCEL

  --  fromAddHandler $ mapIO (const $ get dialog selection) addHandler

-- eDialogFinish :: RB.Event t (FileDialog (), Int)  <- fromAddHandler addDialogFinish

commandOpenFileDialog :: Commanding c => Window a -> c -> IO (AddHandler (WXCore.FileDialog (), Int))
commandOpenFileDialog w commander = do
  (addDialogFinish,handlerDialogFinish) <- newAddHandler
  set commander [ on command := openFileDialog w (\fd r -> handlerDialogFinish (fd,r)) ]
  return addDialogFinish

{-
-- Fix @select@ event not being fired when items are *un*selected.
setupDialog :: WXCore.FileDialog () -> IO ()
setupDialog dialog =
    set dialog [ on unclick := handler ]
    where
    handler _ = do
        propagateEvent
        s <- get dialog selection
        when (s == -1) $ get dialog (on select) >>= id


-- | Behavior corresponding to user input the text field.
behaviorText :: Frameworks t =>
    TextCtrl w -> String -> Moment t (Behavior t String)
behaviorText w s = stepper s <$> eventText w

-- | Event that occurs when the /user/ changed
-- the selection marker in a list box widget.
eventSelection :: Frameworks t =>
    SingleListBox b -> Moment t (Event t Int)
eventSelection w = do
    liftIO $ fixSelectionEvent w
    addHandler <- liftIO $ event1ToAddHandler w (event0ToEvent1 select)
    fromAddHandler $ mapIO (const $ get w selection) addHandler

-- Fix @select@ event not being fired when items are *un*selected.
fixSelectionEvent :: (Selecting w, Reactive w, Selection w) => w -> IO ()
fixSelectionEvent listbox =
    set listbox [ on unclick := handler ]
    where
    handler _ = do
        propagateEvent
        s <- get listbox selection
        when (s == -1) $ get listbox (on select) >>= id
   -}
