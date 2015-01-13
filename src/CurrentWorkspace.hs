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

readInitialWorkspaceState :: AsyncInfos -> IO WorkspaceState
readInitialWorkspaceState triggerInfos = getWorkspaceArg >>= (readWorkspaceState triggerInfos)

--BuildWrapperState -> BuildWrapper a -> (Either SomeException a -> IO ()) -> IO ThreadId


type AsyncInfos = FilePath -> IO ThreadId

-- | if workspace or project file does not exist, continue as though the contents were empty.
readWorkspaceState :: AsyncInfos -> FilePath -> IO WorkspaceState
readWorkspaceState triggerInfos workspaceFilePath = do
  putStrLn $ "readWorkspaceState: " ++ workspaceFilePath
  if workspaceFilePath == ""
    then do return $ WorkspaceState "" []
    else do contents <- readCatch workspaceFilePath
            putStrLn $ "readWorkspaceState: " ++ contents ++ " " ++ workspaceFilePath
            let projectList = read contents :: [String]
            projects <- sequence $ (readProjectState triggerInfos) <$> projectList
            return $ WorkspaceState workspaceFilePath projects

-- | read project file (.n6proj) file contents
readProjectState :: AsyncInfos -> FilePath -> IO ProjectState
readProjectState triggerInfos fp = do
  cabalFp <- readCatch fp
  --opBuildInfo@(cabalBuildInfos,_) <- getCabalBuildInfos cabalFp
  _ <- triggerInfos cabalFp
  --return $ ProjectState fp cabalFp opBuildInfo
  return $ ProjectState fp cabalFp ([],[])

currentWorkspaceSetup :: (?cl :: CabalLock, Frameworks t) =>
   Frame () -> Event t () -> Event t () -> Event t () -> Event t () -> Moment t (Behavior t WorkspaceStateChange)
currentWorkspaceSetup frame1 eCreateWorkspace eOpenWorkspace eCreateProject eImportProject = do
  eCreateProjectFP <- fileDialogOkEvent New "NewProject.cabal" [Cabal] frame1 eCreateProject
  eCreateProjectFP2 <- fileDialogOkEventEx New "NewProject.n6proj" [Project] frame1 eCreateProjectFP

  eImportProjectFP <- fileDialogOkEvent Open "ImportedCabal.cabal" [Cabal] frame1 eImportProject
  eImportProjectFP2 <- fileDialogOkEventEx New "ImportedProject.n6proj" [Project] frame1 eImportProjectFP
  (eCabalBuildInfos,triggerCabalBuildInfos) <- newCabalEvent cabalBuildInfos

  eUpdateBuildInfos <- processCabalBuildInfos eCabalBuildInfos

  eCreateWorkspaceFP <- fileDialogOkEvent New "NewWorkspace.n6" [Workspace] frame1 eCreateWorkspace
  eOpenWorkspaceFP <- fileDialogOkEvent Open "" [Workspace] frame1 eOpenWorkspace
  eCreateProjectState <- processCreateProjectFP eCreateProjectFP2
  eCreateWorkspaceState <- processCreateWorkspaceFP eCreateWorkspaceFP
  eImportProjectState <- processImportProjectFP triggerCabalBuildInfos eImportProjectFP2
  eOpenWorkspaceState <- processOpenWorkspaceFP triggerCabalBuildInfos eOpenWorkspaceFP
  initialWorkspaceState <- liftIO (readInitialWorkspaceState triggerCabalBuildInfos)

  let bWorkspaceStateChange = accumB (WorkspaceStateChange WorkspaceChangeInit initialWorkspaceState) (unions [eCreateProjectState,eImportProjectState, eCreateWorkspaceState,eOpenWorkspaceState, eUpdateBuildInfos])
  writeOnChanges `ioOnChanges` bWorkspaceStateChange
  return bWorkspaceStateChange

  where
  processCabalBuildInfos :: Frameworks t =>
    Event t (RunCmdOutput (OpResult [CabalBuildInfo])) ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processCabalBuildInfos eCabalBuildInfos = do
    let (eLeft,eRight) = split eCabalBuildInfos
    reactimate $ (\e -> logWarningMsg ("processCabalBuildInfos exception: " ++ (show e))) <$> eLeft
    return $ procRight <$> eRight
    where
    procRight (cabfp,buildInfo) (WorkspaceStateChange _ (WorkspaceState fp prjs)) =
      let pMaybe = find (isProject'' cabfp) prjs
          pMaybe' = (projectUpdateBuildInfo buildInfo) <$> pMaybe
          prjsMaybe = (flip projectUpdate prjs) <$> pMaybe'
          prjs' = fromMaybe prjs prjsMaybe
      in WorkspaceStateChange (UpdateBuildInfo pMaybe') $ WorkspaceState fp prjs'
      
  processCreateWorkspaceFP :: Frameworks t =>
    Event t FilePath ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processCreateWorkspaceFP eCreateWorkspaceOk = do
   -- let eCreateWorkspaceContentOk = ("",) <$> eCreateWorkspaceOk
    --eCreated <- (createIfNotExist newWorkspaceFile) `ioOnEvent` eCreateWorkspaceContentOk
   -- let eCreated2 = snd <$> eCreateWorkspaceOk
    return $ (\fp _ -> WorkspaceStateChange (OpenWorkspace fp) $ WorkspaceState fp []) <$> eCreateWorkspaceOk

  processOpenWorkspaceFP ::  Frameworks t =>
    AsyncInfos -> Event t FilePath -> Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processOpenWorkspaceFP triggerInfos eOpenWorkspaceFP = do
    workspaceState :: Event t WorkspaceState <- (readWorkspaceState triggerInfos) `mapIOreaction` eOpenWorkspaceFP
    return $ (\wss@(WorkspaceState fp _) _ -> WorkspaceStateChange (OpenWorkspace fp) wss) <$> workspaceState
  -- create project using NewProjectState because we should delay reading/creation of the cabal file until writeOnChanges
  processCreateProjectFP :: Frameworks t =>
    Event t (FilePath,FilePath) ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processCreateProjectFP eCreateProjectOk = do
    return $ (\(c,fp) (WorkspaceStateChange _ (WorkspaceState wfp prjs)) ->
       let cps = (CreateProjectState fp c) in
       WorkspaceStateChange (OpenProject cps) (WorkspaceState wfp (cps:prjs))) <$> eCreateProjectOk

  processImportProjectFP :: Frameworks t =>
    AsyncInfos -> Event t (FilePath,FilePath) ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processImportProjectFP triggerInfos eImportProjectOk = do
    eProjectState :: Event t ProjectState <- (readCabalFile triggerInfos) `mapIOreaction` eImportProjectOk
    return $ (\ips@(ImportProjectState fp c _)  (WorkspaceStateChange _ (WorkspaceState wfp prjs)) ->
       WorkspaceStateChange (OpenProject ips) (WorkspaceState wfp (ips:prjs))) <$> eProjectState
    where
    readCabalFile :: AsyncInfos -> (FilePath,FilePath) -> IO ProjectState
    readCabalFile triggerInfos (cfp,fp) = do
     -- opResult <- liftIO $ getCabalBuildInfos cfp
      _ <- triggerInfos cfp
      return $ ImportProjectState fp cfp ([],[])

  -- | write changes to files after (Behavior t WorkspaceStateChange) has changed, unless buildInfo is only being updated
  writeOnChanges :: WorkspaceStateChange -> IO ()
  writeOnChanges (WorkspaceStateChange (UpdateBuildInfo _) _) = do
    putStrLn "writeOnChanges: UpdateBuildInfo"
    return ()
  writeOnChanges (WorkspaceStateChange _ ws@(WorkspaceState _ prjs)) = writeWorkspaceFile ws >> writeProjects prjs

{-
  processImportProjectFP :: Frameworks t =>
    Event t (FilePath,FilePath) ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processImportProjectFP eImportProjectOk = do
    eCreated <- (createIfNotExist newProjectFile) `ioOnEvent` eImportProjectOk
    let eCreated2 = snd <$> eCreated
    return $ (\fp (WorkspaceStateChange _ (WorkspaceState wfp prjs)) -> WorkspaceStateChange (OpenProject fp) $ WorkspaceState wfp (fp:prjs)) <$> eCreated2
-}

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
  writeFile fp cabalFp
  writeFile cabalFp "" -- write a new cabal file
writeProjectFile (ImportProjectState fp cabalFp _) = do
  writeFile fp cabalFp
writeProjectFile (ProjectState fp cabalFp _) = do
  writeFile fp cabalFp

writeWorkspaceFile :: WorkspaceState -> IO ()
writeWorkspaceFile (WorkspaceState fp projects) = do
  putStrLn $ "writeWorkspaceFile: " ++ show (fst .  projectStateProjectFile <$> projects)
  writeFile fp $ show $ fst .  projectStateProjectFile <$> projects
