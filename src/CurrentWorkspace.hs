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

module CurrentWorkspace (currentWorkspaceSetup) where
import EventInputs
import Reactive.Banana
import Reactive.Banana.Frameworks
import RBWX.RBWX
import System.Directory
import System.FilePath
import Dialogs
import qualified Data.Tuple as Tup
import System.Environment (getArgs)
import Data.Maybe (fromMaybe,maybeToList, listToMaybe)
import Utility (readCatch)
import CabalParsing
import Control.Exception.Base (SomeException)
import Data.Either
import Data.List (find)
import Control.Concurrent (ThreadId)
--get the first argument if it exists, which is the workspace filePath
getWorkspaceArg :: IO String
getWorkspaceArg = do
  args <- liftIO $ getArgs
  return $ fromMaybe "" $ listToMaybe args

readInitialWorkspaceState :: TriggerOpenProject -> IO WorkspaceState
readInitialWorkspaceState triggerOpenProject = getWorkspaceArg >>= (readWorkspaceState triggerOpenProject)

--BuildWrapperState -> BuildWrapper a -> (Either SomeException a -> IO ()) -> IO ThreadId

type TriggerFinalizeOpenProject = RunCmdTrigger FilePath
type OutputFinalizeOpenProject = RunCmdOutput FilePath (OpResult [CabalBuildInfo])
type TriggerOpenProject = FilePath -> IO ()

-- | read project file (.n6proj) file contents
readProjectState :: TriggerFinalizeOpenProject -> FilePath -> IO ProjectState
readProjectState triggerInfos projfp = do
  cabalFp <- readCatch projfp
  let cbFp = newCabalPath cabalFp
  _ <- triggerInfos cbFp projfp
  return $ ProjectState projfp cbFp ([],[])

readWorkspaceState :: TriggerOpenProject -> FilePath -> IO WorkspaceState
readWorkspaceState triggerOpenProject workspaceFilePath = do
  putStrLn $ "readWorkspaceState: " ++ workspaceFilePath
  do contents <- readCatch workspaceFilePath
     let projectList = read contents :: [String]
     putStrLn $ "readWorkspaceState: " ++ contents ++ " : " ++ workspaceFilePath ++ " : " ++ show projectList
     sequence $ (triggerOpenProject) <$> projectList
     return $ if null projectList
                 then WorkspaceState workspaceFilePath []
                 else WorkspaceStateLoading workspaceFilePath


-- | Left import project (projFp,cabalFP), Right open project
data PendingOpen = ImportPending (FilePath,CabalPath) | OpenPending (FilePath,CabalPath)

pendingOpenCabal :: PendingOpen -> CabalPath
pendingOpenCabal (ImportPending (_,cabalFP)) = cabalFP
pendingOpenCabal (OpenPending (_,cabalFP)) = cabalFP

pendingOpenTuple :: PendingOpen -> (FilePath,CabalPath)
pendingOpenTuple (ImportPending (fp,cabalFP)) = (fp,cabalFP)
pendingOpenTuple (OpenPending (fp,cabalFP)) = (fp,cabalFP)

currentWorkspaceSetup :: (?cl :: CabalLock, Frameworks t) =>
   Frame () -> Event t () -> Event t () -> Event t () -> Event t () -> Moment t (Behavior t WorkspaceStateChange)
currentWorkspaceSetup frame1 eCreateWorkspace eOpenWorkspace eCreateProject eImportProject = do
  eCreateProjectFP <- fileDialogOkEvent New "NewProject.cabal" [Cabal] frame1  eCreateProject
  eCreateProjectFP2 <- fileDialogOkEventEx New "NewProject.n6proj" [Project] frame1 (newCabalPath <$> eCreateProjectFP)

  eImportProjectFP <- fileDialogOkEvent Open "ImportedCabal.cabal" [Cabal] frame1 eImportProject
  eImportProjectFP2 <- fileDialogOkEventEx New "ImportedProject.n6proj" [Project] frame1 (newCabalPath <$> eImportProjectFP)

    --3
  (eFinalizeOpenProject,triggerFinalizeOpenProject) <- newCabalEvent cabalBuildInfos
  (eOpenProject,triggerOpenProject) <- newEvent

  reactimate $ (\e -> logWarningMsg ("eOpenProject: " ++ (show e))) <$> eOpenProject

  eOpenProjectRead <- readCatch `ioOnEvent2` eOpenProject  -- get the cabalFp from the n6proj
  let eOpenProjectRead' = (\x -> (fst x,newCabalPath $ snd x)) <$> eOpenProjectRead
  --1
  let ePendingOpen = ( (ImportPending . Tup.swap) <$> eImportProjectFP2) `union` (OpenPending <$> eOpenProjectRead')
  processPendingOpenProject triggerFinalizeOpenProject ePendingOpen
  --2
  --eImportProjectState <- processImportProjectFP triggerFinalizeOpenProject
  --eCreateProjectState <- processCreateProjectFP

  --4
  eFinalOpenProject <- processFinalizeOpenProject eFinalizeOpenProject

  --TODO: pass triggerOpenProject to processOpenWorkspace

  eCreateWorkspaceFP <- fileDialogOkEvent New "NewWorkspace.n6" [Workspace] frame1 eCreateWorkspace
  eOpenWorkspaceFP <- fileDialogOkEvent Open "" [Workspace] frame1 eOpenWorkspace
  eOpenWorkspaceState <- processOpenWorkspaceFP triggerOpenProject eOpenWorkspaceFP

  eCreateWorkspaceState <- processCreateWorkspaceFP eCreateWorkspaceFP

  --initialWorkspaceState <- liftIO (readInitialWorkspaceState triggerOpenProject)

  let bWorkspaceStateChange = accumB (WorkspaceStateChange WorkspaceChangeInit $ WorkspaceState "" [] ) (unions [eOpenWorkspaceState, eCreateWorkspaceState,eFinalOpenProject])

  writeOnChanges `ioOnChanges` bWorkspaceStateChange
  
  return bWorkspaceStateChange

  where
  processFinalizeOpenProject :: Frameworks t =>
    Event t (OutputFinalizeOpenProject) ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processFinalizeOpenProject eCabalBuildInfos = do
    let (eLeft,eRight) = split eCabalBuildInfos
    --reactimate $ (\e -> logWarningMsg ("processCabalBuildInfos success: " ++ (show e))) <$> eRight
    reactimate $ (\e -> logWarningMsg ("processCabalBuildInfos exception: " ++ (show e))) <$> eLeft
    return $ procRight <$> eRight
    where
    procRight (cabfp,projFp,buildInfo) (WorkspaceStateChange _ (WorkspaceState fp prjs)) =
      let p = ProjectState projFp cabfp buildInfo
      in WorkspaceStateChange (OpenProject projFp) $ WorkspaceState fp (p:prjs)
    procRight (cabfp,projFp,buildInfo) (WorkspaceStateChange _ (WorkspaceStateLoading fp)) =
      let p = ProjectState projFp cabfp buildInfo
      in WorkspaceStateChange (OpenProject projFp) $ WorkspaceState fp [p]
        
  processCreateWorkspaceFP :: Frameworks t =>
    Event t FilePath ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processCreateWorkspaceFP eCreateWorkspaceOk = do
    return $ (\fp _ -> WorkspaceStateChange (OpenWorkspace fp) $ WorkspaceState fp []) <$> eCreateWorkspaceOk

  processOpenWorkspaceFP ::  Frameworks t =>
    TriggerOpenProject -> Event t FilePath -> Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processOpenWorkspaceFP triggerOpenProject eOpenWorkspaceFP = do
    workspaceState :: Event t WorkspaceState <- (readWorkspaceState triggerOpenProject) `mapIOreaction` eOpenWorkspaceFP
    return $ changeState <$> workspaceState
    where
    changeState wss@(WorkspaceState fp _) _ = WorkspaceStateChange (OpenWorkspace fp) wss
    changeState wss@(WorkspaceStateLoading fp) _ = WorkspaceStateChange (OpenWorkspace fp) wss
    -- | if workspace or project file does not exist, continue as though the contents were empty.

    
  -- create project using NewProjectState because we should delay reading/creation of the cabal file until writeOnChanges
 -- processCreateProjectFP :: Frameworks t =>
 --   Event t (FilePath,FilePath) ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
 -- processCreateProjectFP eCreateProjectOk = do
  --  return $ (\(c,fp) (WorkspaceStateChange _ (WorkspaceState wfp prjs)) ->
  --     let cps = (CreateProjectState fp c) in
  --     WorkspaceStateChange (OpenProject cps) (WorkspaceState wfp (cps:prjs))) <$> eCreateProjectOk

  -- | trigger cabalEvent:cabalBuildInfos, which then finalizes opening the Project
  processPendingOpenProject :: Frameworks t =>
    TriggerFinalizeOpenProject -> Event t PendingOpen ->  Moment t ()
  processPendingOpenProject triggerFinalizeOpenProject ePendingOpen = do
    let ePendingCabalFP = pendingOpenTuple <$> ePendingOpen
    _ <- (\(fp,cabFp) -> triggerFinalizeOpenProject cabFp fp >> return ()) `ioOnEvent` ePendingCabalFP
    return ()

  -- | write changes to files after (Behavior t WorkspaceStateChange) has changed
  writeOnChanges :: WorkspaceStateChange -> IO ()
  writeOnChanges (WorkspaceStateChange _ ws@(WorkspaceState _ prjs)) = writeWorkspaceFile ws >> writeProjects prjs
  writeOnChanges (WorkspaceStateChange _ ws@(WorkspaceStateLoading _)) = return ()

createIfNotExist :: ((String, FilePath) -> IO ()) -> (String, FilePath) -> IO ()
createIfNotExist newFileFunc (c,fp) = do
            fileExists <- doesFileExist fp -- if file doesn't exist then assume we are trying to create it
            if fileExists
              then return ()
	      else newFileFunc (c,fp)

writeProjects :: [ProjectState] -> IO ()
writeProjects prjs = sequence_ $ writeProjectFile <$> prjs

writeProjectFile :: ProjectState -> IO ()
writeProjectFile (CreateProjectState fp cabalFp) = do
  writeFile fp (unCabalPath cabalFp)
  writeFile (unCabalPath cabalFp) "" -- write a new cabal file
writeProjectFile (ImportProjectState fp cabalFp _) = do
  writeFile fp (unCabalPath cabalFp)
writeProjectFile (ProjectState fp cabalFp _) = do
  writeFile fp (unCabalPath cabalFp)

writeWorkspaceFile :: WorkspaceState -> IO ()
writeWorkspaceFile (WorkspaceState fp projects) = do
  writeFile fp $ show $ projectStateProjFile <$> projects
