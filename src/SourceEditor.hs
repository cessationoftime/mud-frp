-----------------------------------------------------------------------------
--
-- Module      :  SourceEditor
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- Code for the SourceEditor control
--
-----------------------------------------------------------------------------
module SourceEditor (sourceEditor,sourceEditorLoadFile,wireupSourceEditorOpenFileDialog) where

import Reactive.Banana
import Reactive.Banana.WX
import Data.Maybe (fromMaybe)
import Data.IORef
import Dialogs
import RBWX.RBWX

-- | setup the eventNetwork to show an openFileDialog in the given Window when the given event is received. And load the file into the sourceControl
wireupSourceEditorOpenFileDialog :: Frameworks t => Window a -> SourceEditorCtrl -> Event t () -> Moment t (Behavior t (Maybe FilePath))
wireupSourceEditorOpenFileDialog frame1 sourceEditorCtrl event = do
  let openFileDialog = fileOpenDialog1 frame1 True True "Open File" [("Haskell file",["*.hs"])] "" ""
  eGetDialogFinish <- eventDialogResult openFileDialog event
  eGetDialogFile  <- eventDialogOkFilePath eGetDialogFinish
  reactimate $ (\fp -> sourceEditorLoadFile sourceEditorCtrl fp >> return ()) <$> eGetDialogFile
  return $ stepper Nothing (Just <$> eGetDialogFile)
-- |
-- the color scheme to use to highlight Haskell code
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

type SourceEditorCtrl = StyledTextCtrl ()

-- | load file into the sourceEditor
-- AKA wxHaskell's styledTextCtrlLoadFile
sourceEditorLoadFile :: SourceEditorCtrl -> FilePath -> IO Bool
sourceEditorLoadFile = styledTextCtrlLoadFile

sourceEditorSaveFile :: SourceEditorCtrl -> FilePath -> IO Bool
sourceEditorSaveFile = styledTextCtrlSaveFile


-- | Show an saveFileDialog and get a FilePath. Save the file pointed to by
-- the path into the sourceEditor
sourceEditorSaveFileDialog :: Window a -> SourceEditorCtrl -> IO Bool
sourceEditorSaveFileDialog window sourceEditorCtrl = do
  filePath <- fileSaveDialog window True True "Save File" [("Haskell file",["*.hs"])] "" ""
  let mb = (sourceEditorLoadFile sourceEditorCtrl) <$> filePath
  fromMaybe (return False) mb


-- | Create the SourceEditor control
-- AKA wxHaskell's StyledTextCtrl
sourceEditor :: Window a -> [Prop (SourceEditorCtrl)] -> IO (SourceEditorCtrl)
sourceEditor window props = do
    styledTxt <- styledTextCtrl window props
    styledTextCtrlStyleClearAll styledTxt
    styledTextCtrlSetLexer styledTxt wxSTC_LEX_HASKELL
    styledTextCtrlSetKeyWords styledTxt 0 keywords
    let fontstyle = fontFixed { _fontFace = "Monospace" }
    (font, _) <- fontCreateFromStyle fontstyle
    mapM_ (\style -> styledTextCtrlStyleSetFont styledTxt style font) [0..wxSTC_STYLE_LASTPREDEFINED]
    sequence_ [styledTextCtrlStyleSetForeground styledTxt k c | (k, c) <- colorscheme]

--styledTextCtrlLoadFile styledTxt "/home/cvanvranken/Desktop/STCLexer.hs"

    return styledTxt
