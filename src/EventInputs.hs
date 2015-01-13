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
import Utility
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

--newtype ProjectFilePath = ProjectFilePath { unProjectFilePath :: FilePath }
--newtype WorkspaceFilePath = WorkspaceFilePath { unWorkspaceFilePath :: FilePath }

data ProjectState = CreateProjectState FilePath String |
                    ImportProjectState FilePath String (OpResult [CabalBuildInfo]) |
                    ProjectState FilePath String (OpResult [CabalBuildInfo]) deriving (Show)
                    
data WorkspaceState = WorkspaceState { workspaceFile :: FilePath, projects :: [ProjectState] } deriving (Show)

data WorkspaceChangeType = WorkspaceChangeInit | UpdateBuildInfo (Maybe ProjectState) | OpenWorkspace FilePath | CloseWorkspace | OpenProject ProjectState | CloseProject FilePath

-- | the change that has taken place.  This data should be sent to downstream events.
data WorkspaceStateChange = WorkspaceStateChange {lastchange :: WorkspaceChangeType, current :: WorkspaceState}

isOpenProject ::  WorkspaceStateChange -> Bool
isOpenProject (WorkspaceStateChange (OpenProject _) _) = True
isOpenProject (WorkspaceStateChange _ _) = False

isProject p0 p1 = projectStateFilePath p0 ==  projectStateFilePath p1

isProject' p0 p1 = projectStateCabalFile p0 ==  projectStateCabalFile p1

-- | use cab file to identify project
isProject'' :: FilePath -> ProjectState -> Bool
isProject'' p0 p1 = p0 ==  projectStateCabalFile p1

projectStateFilter' :: FilePath -> [ProjectState] -> [ProjectState]
projectStateFilter' cabfp = filter (isProject'' cabfp)

projectStateFilter :: ProjectState -> [ProjectState] -> [ProjectState]
projectStateFilter p = filter (isProject p)

projectUpdate :: ProjectState -> [ProjectState] ->[ProjectState]
projectUpdate p ps = listUpdate isProject p ps

projectStateBuildInfos :: ProjectState -> [CabalBuildInfo]
projectStateBuildInfos (ProjectState _ _ (buildInfos,_)) = buildInfos
projectStateBuildInfos (CreateProjectState _ _) = []
projectStateBuildInfos (ImportProjectState _ _ (buildInfos,_)) = buildInfos

projectStateModuleFiles :: ProjectState -> [FilePath]
projectStateModuleFiles = moduleFiles . projectStateBuildInfos

projectStateCabalFile :: ProjectState -> FilePath
projectStateCabalFile (ProjectState _ cabalFp _) = cabalFp
projectStateCabalFile (CreateProjectState _ cabalFp) = cabalFp
projectStateCabalFile (ImportProjectState _ cabalFp _) = cabalFp

-- | get the project filepath and the project file's content
projectStateProjectFile :: ProjectState -> (FilePath,String)
projectStateProjectFile (ProjectState fp cabalFp _) = (fp,cabalFp)
projectStateProjectFile (CreateProjectState fp cabalFp) = (fp,cabalFp)
projectStateProjectFile (ImportProjectState fp cabalFp _) = (fp,cabalFp)

projectStateFilePath :: ProjectState -> FilePath
projectStateFilePath (ProjectState fp _ _) = fp
projectStateFilePath (CreateProjectState fp _) = fp
projectStateFilePath (ImportProjectState fp _ _) = fp

projectUpdateBuildInfo :: (OpResult [CabalBuildInfo]) -> ProjectState -> ProjectState
projectUpdateBuildInfo b ps = let (fp,s) = projectStateProjectFile ps in ProjectState fp s b
