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
--
type NotebookInput t = Event t NotebookChange
data NotebookChange = NewPage FilePath | OpenPage FilePath | Save | SaveAll

justNewPage :: NotebookChange ->  Maybe FilePath
justNewPage (NewPage fp) = Just fp
justNewPage _ = Nothing

justOpenPage :: NotebookChange ->  Maybe FilePath
justOpenPage (OpenPage fp) = Just fp
justOpenPage _ = Nothing

filterSave :: NotebookChange ->  Bool
filterSave Save = True
filterSave _ = False

filterSaveAll :: NotebookChange ->  Bool
filterSaveAll SaveAll = True
filterSaveAll _ = False

-------------- WorkspaceBrowser

type WorkspaceBrowserInput t = Event t WorkspaceBrowserChange
data WorkspaceBrowserChange = WorkspaceStateInit | StateChange WorkspaceState | CreateWorkspace FilePath | CreateProject FilePath

-------------- CurrentWorkspace

--newtype ProjectFilePath = ProjectFilePath { unProjectFilePath :: FilePath }
--newtype WorkspaceFilePath = WorkspaceFilePath { unWorkspaceFilePath :: FilePath }

data ProjectState = CreateProjectState FilePath CabalPath |
                    ImportProjectState FilePath CabalPath (OpResult [CabalBuildInfo]) |
                    ProjectState FilePath CabalPath (OpResult [CabalBuildInfo]) deriving (Show)

data WorkspaceState = WorkspaceStateLoading {workspaceFile :: FilePath} | WorkspaceState { workspaceFile :: FilePath, projects :: [ProjectState] } deriving (Show)

data WorkspaceChangeType = WorkspaceChangeInit | OpenWorkspace FilePath | CloseWorkspace | OpenProject FilePath | CloseProject FilePath

-- | the change that has taken place.  This data should be sent to downstream events.
data WorkspaceStateChange = WorkspaceStateChange {lastchange :: WorkspaceChangeType, current :: WorkspaceState}

isOpenProject ::  WorkspaceStateChange -> Bool
isOpenProject (WorkspaceStateChange (OpenProject _) _) = True
isOpenProject (WorkspaceStateChange _ _) = False

isProject p0 p1 = projectStateFilePath p0 ==  projectStateFilePath p1

isProject' p0 p1 = projectStateCabalFile p0 ==  projectStateCabalFile p1

-- | use cab file to identify project
isProject'' :: CabalPath -> ProjectState -> Bool
isProject'' p0 p1 = p0 ==  projectStateCabalFile p1

-- | use proj file to identify project
isProject''' :: FilePath -> ProjectState -> Bool
isProject''' p0 p1 = p0 ==  projectStateProjFile p1

projectStateFilter' :: CabalPath -> [ProjectState] -> [ProjectState]
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

projectStateCabalFile :: ProjectState -> CabalPath
projectStateCabalFile (ProjectState _ cabalFp _) = cabalFp
projectStateCabalFile (CreateProjectState _ cabalFp) = cabalFp
projectStateCabalFile (ImportProjectState _ cabalFp _) = cabalFp

-- | get the project filepath and the project file's content
projectStateProjFile :: ProjectState -> FilePath
projectStateProjFile (ProjectState fp _ _) = fp
projectStateProjFile (CreateProjectState fp _) = fp
projectStateProjFile (ImportProjectState fp _ _) = fp

projectStateBothFile :: ProjectState -> (FilePath,CabalPath)
projectStateBothFile (ProjectState fp cabalFp _) = (fp,cabalFp)
projectStateBothFile (CreateProjectState fp cabalFp) = (fp,cabalFp)
projectStateBothFile (ImportProjectState fp cabalFp _) = (fp,cabalFp)

projectStateFilePath :: ProjectState -> FilePath
projectStateFilePath (ProjectState fp _ _) = fp
projectStateFilePath (CreateProjectState fp _) = fp
projectStateFilePath (ImportProjectState fp _ _) = fp

projectUpdateBuildInfo :: (OpResult [CabalBuildInfo]) -> ProjectState -> ProjectState
projectUpdateBuildInfo b ps = let (fp,s) = projectStateBothFile ps in ProjectState fp s b
