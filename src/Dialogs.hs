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

module Dialogs (loadFileWithDialog, DialogDescriptor(..), DialogOpenMode(..)) where
import RBWX.RBWX


fileOpenDialog1 :: Window a -> Bool -> Bool -> String -> [(String,[String])] -> FilePath -> FilePath -> ChainIO DialogResult
fileOpenDialog1 parent rememberCurrentDir allowReadOnly message wildcards directory filename result
  = fileDialog parent (curry result) flags message wildcards directory filename
  where
    flags
      = wxOPEN .+. (if rememberCurrentDir then wxCHANGE_DIR else 0) .+. (if allowReadOnly then 0 else wxHIDE_READONLY)

-- |type representing the FileDialog and it's result
type DialogResult = (FileDialog (), Int)

-- |trigger opening the fileDialog on the given event and create a new DialogResult event.
eventDialogResult :: Frameworks t => ChainIO DialogResult -> Event t b -> Moment t (Event t DialogResult)
eventDialogResult showDialog ev = showDialog `mapIOchainreaction` ev


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
eventDialogOkFilePath eDialogFinish = fileDialogGetPath `mapIOreaction` (eventDialogOk eDialogFinish)

-- | setup the eventNetwork to show an openFileDialog in the given Window when the given event is received. And load the file into the sourceControl
fileDialogOkEvent :: Frameworks t => String ->  [DialogDescriptor] -> Window a -> Event t b -> Moment t (Event t FilePath)
fileDialogOkEvent title fileDescs frame1 event = do
  let openFileDialog = fileOpenDialog1 frame1 True True title (descriptor <$> fileDescs) "" ""
  eGetDialogFinish <- eventDialogResult openFileDialog event
  eGetDialogOkFilePath  <- eventDialogOkFilePath eGetDialogFinish
  return eGetDialogOkFilePath

data DialogOpenMode = New | Open

instance Show DialogOpenMode where
  show New = "New File"
  show Open = "Open File"

data DialogDescriptor = Haskell | Workspace | Project | Nix | Cabal

instance Show DialogDescriptor where
  show Haskell = "Haskell Source"
  show Workspace = "N6 Workspace"
  show Project = "N6 Project"
  show Nix = "Nix"
  show Cabal = "Cabal"

fileExtensions :: DialogDescriptor -> [String]
fileExtensions Haskell = ["*.hs", "*.lhs"]
fileExtensions Workspace = ["*.n6"]
fileExtensions Project = ["*.n6proj"]
fileExtensions Nix = ["*.nix"]
fileExtensions Cabal = ["*.cabal"]

descriptor :: DialogDescriptor -> (String,[String])
descriptor fd = (show fd,fileExtensions fd)

loadFileWithDialog fileMode = fileDialogOkEvent (show (fileMode :: DialogOpenMode))

