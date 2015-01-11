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
import CabalParsing
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

data ProjectState = CreateProjectState FilePath String| ImportProjectState FilePath String | ProjectState FilePath String (OpResult [CabalBuildInfo]) deriving (Show)
data WorkspaceState = WorkspaceState { workspaceFile :: FilePath, projects :: [ProjectState] } deriving (Show)

data WorkspaceChangeType = WorkspaceChangeInit | OpenWorkspace FilePath | CloseWorkspace | OpenProject ProjectState | CloseProject FilePath

-- | the change that has taken place.  This data should be sent to downstream events.
data WorkspaceStateChange = WorkspaceStateChange {lastchange :: WorkspaceChangeType, current :: WorkspaceState}

projectStateBuildInfos :: ProjectState -> [CabalBuildInfo]
projectStateBuildInfos (ProjectState _ _ (buildInfos,_)) = buildInfos
projectStateBuildInfos _ = []

projectStateModuleFiles :: ProjectState -> [FilePath]
projectStateModuleFiles = moduleFiles . projectStateBuildInfos

projectStateCabalFile :: ProjectState -> FilePath
projectStateCabalFile (ProjectState _ cabalFp _) = cabalFp
projectStateCabalFile (CreateProjectState _ cabalFp) = cabalFp
projectStateCabalFile (ImportProjectState _ cabalFp) = cabalFp

-- | get the project filepath and the project file's content
projectStateProjectFile :: ProjectState -> (FilePath,String)
projectStateProjectFile (ProjectState fp cabalFp _) = (fp,cabalFp)
projectStateProjectFile (CreateProjectState fp cabalFp) = (fp,cabalFp)
projectStateProjectFile (ImportProjectState fp cabalFp) = (fp,cabalFp)

projectStateFilePath :: ProjectState -> FilePath
projectStateFilePath (ProjectState fp _ _) = fp
projectStateFilePath (CreateProjectState fp _) = fp
projectStateFilePath (ImportProjectState fp _) = fp
