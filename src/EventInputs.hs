{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :  EventController
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

module EventInputs where
import RBWX.RBWX


data NotebookInput t = NotebookInput {
  eiNewPage :: Event t FilePath
 ,eiOpenPage :: Event t FilePath
 ,eiSave :: Event t ()
 ,eiSaveAll :: Event t ()
}

class UniteInputs a where
  unite :: [a] -> a

class NotebookInputs a where
  toNotebookInput :: a -> NotebookInput t

instance UniteInputs (NotebookInput t) where
  unite inputs = NotebookInput (unions $ eiNewPage <$> inputs) (unions $ eiOpenPage <$> inputs) (unions $ eiSave <$> inputs) (unions $ eiSaveAll <$> inputs)

class WorkspaceBrowserInputs a where
   toWorkspaceBrowserInput :: a -> WorkspaceBrowserInput t

data WorkspaceBrowserInput t = WorkspaceBrowserInput {
   eiCreateWorkspace :: Event t FilePath
  ,eiOpenWorkspace :: Event t FilePath
}

instance UniteInputs (WorkspaceBrowserInput t) where
  unite inputs = WorkspaceBrowserInput (uni eiCreateWorkspace) (uni eiOpenWorkspace)
    where  uni x =(unions $ x <$> inputs)

data AuiManagerInput t = AuiManagerInput {
  eiAddPane :: Event t [(Window (), Int, String)]
}

class AuiManagerInputs a where
   toAuiManagerInput :: a -> AuiManagerInput t

instance UniteInputs (AuiManagerInput t) where
  unite inputs = AuiManagerInput (uni eiAddPane)
    where  uni x =(unions $ x <$> inputs)

