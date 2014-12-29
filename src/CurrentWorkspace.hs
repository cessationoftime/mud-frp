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
  eCreateProjectFP <- fileDialogOkEvent New "NewProject.n6proj" [Project] frame1 eCreateProject
  
  eImportProjectFP <- fileDialogOkEvent New "ImportedProject.n6proj" [Project] frame1 eImportProject
  eImportProjectFP <- fileDialogOkEventEx Open "ImportedCabal.cabal" [Cabal] frame1 eImportProjectFP
  
  eCreateWorkspaceFP <- fileDialogOkEvent New "NewWorkspace.n6" [Workspace] frame1 eCreateWorkspace
  eOpenWorkspaceFP <- fileDialogOkEvent Open "" [Workspace] frame1 eOpenWorkspace
  eCreateProjectState <- processCreateProjectFP eCreateProjectFP
  eCreateWorkspaceState <- processCreateWorkspaceFP eCreateWorkspaceFP
  eImportProjectState <- processImportProjectFP eImportProjectFP
  let eOpenWorkspaceState  = processOpenWorkspaceFP eOpenWorkspaceFP
  return $ accumB (WorkspaceStateChange WorkspaceChangeInit $ WorkspaceState "" []) (unions [eCreateProjectState,eImportProjectState, eCreateWorkspaceState,eOpenWorkspaceState])

  where
  processCreateWorkspaceFP :: Frameworks t =>
    Event t FilePath ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processCreateWorkspaceFP eCreateWorkspaceOk = do
    let eCreateWorkspaceContentOk = ("",) <$> eCreateWorkspaceOk
    eCreated <- (createIfNotExist newWorkspaceFile) `ioOnEvent` eCreateWorkspaceContentOk
    let eCreated2 = snd <$> eCreated
    return $ (\fp (WorkspaceStateChange _ (WorkspaceState _ prjs)) -> WorkspaceStateChange (OpenWorkspace fp) $ WorkspaceState fp prjs) <$> eCreated2

  processOpenWorkspaceFP :: Event t FilePath -> Event t (WorkspaceStateChange -> WorkspaceStateChange)
  processOpenWorkspaceFP eOpenWorkspaceFP =
    (\fp (WorkspaceStateChange _ (WorkspaceState _ prjs)) -> WorkspaceStateChange (OpenWorkspace fp) $ WorkspaceState fp prjs) <$> eOpenWorkspaceFP

  processCreateProjectFP :: Frameworks t =>
    Event t FilePath ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processCreateProjectFP eCreateProjectOk = do
    let eCreateProjectContentOk = ("",) <$> eCreateProjectOk
    eCreated <- (createIfNotExist newProjectFile) `ioOnEvent` eCreateProjectContentOk
    let eCreated2 = snd <$> eCreated
    return $ (\fp (WorkspaceStateChange _ (WorkspaceState wfp prjs)) -> WorkspaceStateChange (OpenProject fp) $ WorkspaceState wfp (fp:prjs)) <$> eCreated2

  processImportProjectFP :: Frameworks t =>
    Event t (FilePath,FilePath) ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processImportProjectFP eImportProjectOk = do
    eCreated <- (createIfNotExist newProjectFile) `ioOnEvent` eImportProjectOk
    let eCreated2 = snd <$> eCreated
    return $ (\fp (WorkspaceStateChange _ (WorkspaceState wfp prjs)) -> WorkspaceStateChange (OpenProject fp) $ WorkspaceState wfp (fp:prjs)) <$> eCreated2



createIfNotExist :: ((String, FilePath) -> IO ()) -> (String, FilePath) -> IO ()
createIfNotExist newFileFunc (c,fp) = do
            fileExists <- doesFileExist fp -- if file doesn't exist then assume we are trying to create it
            if fileExists
              then return ()
	      else newFileFunc (c,fp)

newProjectFile :: (String, FilePath) -> IO ()
newProjectFile (contents,fp) = do
  writeFile fp contents

newWorkspaceFile :: (String, FilePath) -> IO ()
newWorkspaceFile (_,fp) = do
  writeFile fp "[]"
