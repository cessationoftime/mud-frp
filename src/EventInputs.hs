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

module EventInputs (
NotebookInputs(NotebookInputs)
,unite
,WorkspaceBrowserInputs(WorkspaceBrowserInputs)
,AuiManagerInputs(..)
) where
import RBWX.RBWX

class Inputs a where
  unite :: [a] -> a

--------------
{-
class NotebookInputs a where
  newPage :: Event t FilePath = never
  openPage :: Event t FilePath = never
  save :: Event t () = never
  saveAll :: Event t () = never
-}

data NotebookInputs t = NotebookInputs {
     newPage :: Event t FilePath
    ,openPage :: Event t FilePath
    ,save :: Event t ()
    ,saveAll :: Event t ()
  }

instance Inputs (NotebookInputs t) where
    unite inputs = NotebookInputs (uni newPage) (uni openPage) (uni save) (uni saveAll)
      where  uni x =(unions $ x <$> inputs)

-----------

data WorkspaceBrowserInputs t = WorkspaceBrowserInputs { createWS :: Event t FilePath, openWS :: Event t FilePath }


instance Inputs (WorkspaceBrowserInputs t) where
    unite inputs = WorkspaceBrowserInputs (uni createWS) (uni openWS)
      where  uni x =(unions $ x <$> inputs)



class AuiManagerInputs a where
  addPane :: a -> Event t [(Window (), Int, String)]
  addPane _ = never
