-----------------------------------------------------------------------------
--
-- Module      :  CurrentWorkspaceData
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

module CurrentWorkspaceData where


type Project = FilePath
data WorkspaceState = WorkspaceState { workspaceFile :: FilePath, projects :: [Project] }

data ChangeType = OpenWorkspace | CloseWorkspace | OpenProject | CloseProject

-- | the change that has taken place.  This data should be sent to downstream events.
data WorkspaceStateChange = WorkspaceStateChange {change :: ChangeType, old :: WorkspaceState, new :: WorkspaceState}


