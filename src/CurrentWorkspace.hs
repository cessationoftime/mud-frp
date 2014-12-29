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

currentWorkspaceSetup :: Frameworks t => Frame () -> Event t () -> Event t () -> Event t () -> Event t () -> Moment t (Behavior t WorkspaceStateChange)
currentWorkspaceSetup frame1 eCreateWorkspace eOpenWorkspace eCreateProject eImportProject = do
  eCreateProjectFP <- fileDialogOkEvent New "NewProject.cabal" [Cabal] frame1 eCreateProject
  eCreateProjectFP2 <- fileDialogOkEventEx New "NewProject.n6proj" [Project] frame1 eCreateProjectFP

  eImportProjectFP <- fileDialogOkEvent Open "ImportedCabal.cabal" [Cabal] frame1 eImportProject
  eImportProjectFP2 <- fileDialogOkEventEx New "ImportedProject.n6proj" [Project] frame1 eImportProjectFP

  
  eCreateWorkspaceFP <- fileDialogOkEvent New "NewWorkspace.n6" [Workspace] frame1 eCreateWorkspace
  eOpenWorkspaceFP <- fileDialogOkEvent Open "" [Workspace] frame1 eOpenWorkspace
  eCreateProjectState <- processCreateProjectFP eCreateProjectFP2
  eCreateWorkspaceState <- processCreateWorkspaceFP eCreateWorkspaceFP
  eImportProjectState <- processCreateProjectFP eImportProjectFP2
  let eOpenWorkspaceState  = processOpenWorkspaceFP eOpenWorkspaceFP
      bWorkspaceStateChange = accumB (WorkspaceStateChange WorkspaceChangeInit $ WorkspaceState "" []) (unions [eCreateProjectState,eImportProjectState, eCreateWorkspaceState,eOpenWorkspaceState])
  writeOnChanges `ioOnChanges` bWorkspaceStateChange 
  return bWorkspaceStateChange

  where
  processCreateWorkspaceFP :: Frameworks t =>
    Event t FilePath ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processCreateWorkspaceFP eCreateWorkspaceOk = do
   -- let eCreateWorkspaceContentOk = ("",) <$> eCreateWorkspaceOk
    --eCreated <- (createIfNotExist newWorkspaceFile) `ioOnEvent` eCreateWorkspaceContentOk
   -- let eCreated2 = snd <$> eCreateWorkspaceOk
    return $ (\fp (WorkspaceStateChange _ (WorkspaceState _ prjs)) -> WorkspaceStateChange (OpenWorkspace fp) $ WorkspaceState fp prjs) <$> eCreateWorkspaceOk

  processOpenWorkspaceFP :: Event t FilePath -> Event t (WorkspaceStateChange -> WorkspaceStateChange)
  processOpenWorkspaceFP eOpenWorkspaceFP =
    (\fp (WorkspaceStateChange _ (WorkspaceState _ prjs)) -> WorkspaceStateChange (OpenWorkspace fp) $ WorkspaceState fp prjs) <$> eOpenWorkspaceFP

  processCreateProjectFP :: Frameworks t =>
    Event t (FilePath,FilePath) ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processCreateProjectFP eCreateProjectOk = do
   -- eCreated <- (createIfNotExist newProjectFile) `ioOnEvent`  eCreateProjectOk
    return $ (\(c,fp) (WorkspaceStateChange _ (WorkspaceState wfp prjs)) -> WorkspaceStateChange (OpenProject fp) $ WorkspaceState wfp ((fp,c):prjs)) <$> eCreateProjectOk

  writeOnChanges :: WorkspaceStateChange -> IO ()
  writeOnChanges (WorkspaceStateChange _ ws@(WorkspaceState _ prjs)) = newWorkspaceFile ws >> writeProjects prjs

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

writeProjects :: [Project] -> IO ()
writeProjects prjs = sequence_ $ newProjectFile <$> prjs

newProjectFile :: Project -> IO ()
newProjectFile (fp,contents) = do
  writeFile fp contents

newWorkspaceFile :: WorkspaceState -> IO ()
newWorkspaceFile (WorkspaceState fp projects) = do
  writeFile fp (show $ fst <$> projects)
