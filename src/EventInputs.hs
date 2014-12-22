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

-------------- Notebook

type NotebookInput t = Event t NotebookChange
data NotebookChange = NewPage FilePath | OpenPage FilePath | Save | SaveAll

justNewPage :: NotebookChange ->  Maybe FilePath
justNewPage (NewPage fp) = Just fp
justNewPage _ = Nothing

justOpenPage :: NotebookChange ->  Maybe FilePath
justOpenPage (OpenPage fp) = Just fp
justOpenPage _ = Nothing

-------------- WorkspaceBrowser

type WorkspaceBrowserInput t = Event t WorkspaceBrowserChange
data WorkspaceBrowserChange = WorkspaceStateInit | StateChange WorkspaceState | CreateWorkspace FilePath | CreateProject FilePath

-------------- CurrentWorkspace

type Project = FilePath
data WorkspaceState = WorkspaceState { workspaceFile :: FilePath, projects :: [Project] } deriving (Eq,Show)

data WorkspaceChangeType = WorkspaceChangeInit | OpenWorkspace FilePath | CloseWorkspace | OpenProject FilePath | CloseProject FilePath

-- | the change that has taken place.  This data should be sent to downstream events.
data WorkspaceStateChange = WorkspaceStateChange {lastchange :: WorkspaceChangeType, current :: WorkspaceState}
