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

module Dialogs (fileDialogOkEvent, DialogDescriptor(..), DialogOpenMode(..)) where
import RBWX.RBWX


fileDialog1 :: DialogOpenMode -> String -> [(String,[String])] -> FilePath -> FilePath -> Window a -> ChainIO DialogResult
fileDialog1 dialogOpenMode message wildcards directory filename parent result
  = fileDialog parent (curry result) flags message wildcards directory filename
  where flags = dialogModeFlags dialogOpenMode

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
fileDialogOkEventEx :: Frameworks t => String -> DialogOpenMode -> String -> [DialogDescriptor] -> Window a -> Event t b -> Moment t (Event t FilePath)
fileDialogOkEventEx title fileMode fileName fileDescs frame1 event = do
  let openFileDialog = fileDialog1 fileMode title (descriptor <$> fileDescs) "" fileName frame1
  eGetDialogFinish <- eventDialogResult openFileDialog event
  eGetDialogOkFilePath  <- eventDialogOkFilePath eGetDialogFinish
  return eGetDialogOkFilePath

data DialogOpenMode = New | Open

instance Show DialogOpenMode where
  show New = "New File"
  show Open = "Open File"

dialogModeFlags :: DialogOpenMode -> Int
dialogModeFlags mode =
  case mode of
    -- use save dialog, remember current directory, Show overwrite prompt
    New -> wxSAVE .+. wxCHANGE_DIR .+. wxOVERWRITE_PROMPT
    -- use open dialog, remember current directory, hide readonly
    Open -> wxOPEN .+. wxCHANGE_DIR  .+. wxHIDE_READONLY


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

fileDialogOkEvent :: Frameworks t => DialogOpenMode -> String -> [DialogDescriptor] -> Window a -> Event t b -> Moment t (Event t FilePath)
fileDialogOkEvent fileMode fileName = fileDialogOkEventEx (show (fileMode :: DialogOpenMode)) fileMode fileName

fileDialogOkEvent' :: Frameworks t => DialogOpenMode -> String -> [DialogDescriptor] -> Window a -> Event t b -> Moment t (Event t FilePath)
fileDialogOkEvent' fileMode fileName = fileDialogOkEventEx (show (fileMode :: DialogOpenMode)) fileMode fileName

