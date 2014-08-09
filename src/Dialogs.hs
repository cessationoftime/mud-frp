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
import RBWX.Lift

type ShowDialog = OpenDialogResulter () -> IO ()
type OpenDialogResulter a = FileDialog () -> Int -> IO a

fileOpenDialog1 :: Window a -> Bool -> Bool -> String -> [(String,[String])] -> FilePath -> FilePath -> ShowDialog
fileOpenDialog1 parent rememberCurrentDir allowReadOnly message wildcards directory filename result
  = fileDialog parent result flags message wildcards directory filename
  where
    flags
      = wxOPEN .+. (if rememberCurrentDir then wxCHANGE_DIR else 0) .+. (if allowReadOnly then 0 else wxHIDE_READONLY)

-- |type representing the FileDialog and it's result
type DialogResult = (FileDialog (), Int)

-- |trigger opening the fileDialog on the given event and create a new DialogResult event.
eventDialogResult :: Frameworks t => ShowDialog -> Event t b -> Moment t (Event t DialogResult)
eventDialogResult showDialog ev = do
    (addDialogFinish,handlerDialogFinish) <- liftIO newAddHandler
    reactimate $ (\_ -> showDialog (\fd r -> handlerDialogFinish (fd,r))) <$> ev
    fromAddHandler addDialogFinish

-- |used to create events representing specific button presses in dialogs using the wxID constants. Ex: eventDialogOk, eventDialogCancel
eventDialogButtonClick :: Int ->  Event t DialogResult ->  Event t (FileDialog ())
eventDialogButtonClick wxId eDialogFinish  = fst `fmap` eClick
   where eClick = filterE (\(_,r) -> r == wxId) eDialogFinish

-- |filter the eventDialogResult into an OK-button-only result
eventDialogOk :: Event t DialogResult ->  Event t (FileDialog ())
eventDialogOk = eventDialogButtonClick wxID_OK

-- |filter the eventDialogResult into a cancel-button-only result
eventDialogCancel :: Event t DialogResult ->  Event t (FileDialog ())
eventDialogCancel = eventDialogButtonClick wxID_CANCEL

-- |filter the eventDialogResult into an OK-button-only result, and get the resulting FilePath
eventDialogOkFilePath :: Frameworks t => Event t DialogResult ->  Moment t (Event t FilePath)
eventDialogOkFilePath eDialogFinish = do
    (addGetDialogFile,handlerGetDialogFile) <- liftIO newAddHandler
    reactimate $ handlerGetDialogFile <$> (eventDialogOk eDialogFinish)
    fromAddHandler (fileDialogGetPath `mapIO` addGetDialogFile)


