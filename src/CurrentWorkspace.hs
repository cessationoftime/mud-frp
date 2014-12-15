-----------------------------------------------------------------------------
--
-- Module      :  CurrentWorkspace
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

module CurrentWorkspace where

currentWorkspaceSetup :: Frameworks t => Moment t ()
currentWorkspaceSetup = do
  accumD (WorkspaceState "" [])
