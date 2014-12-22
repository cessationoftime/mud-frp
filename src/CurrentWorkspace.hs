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

currentWorkspaceSetup :: Frameworks t => Event t FilePath -> Event t FilePath -> Event t FilePath  -> Moment t (Behavior t WorkspaceStateChange)
currentWorkspaceSetup eCreateWorkspaceFP eOpenWorkspaceFP eCreateProjectFP = do

  eCreateProjectState <- processCreateProjectFP eCreateProjectFP
  eCreateWorkspaceState <- processCreateWorkspaceFP eCreateWorkspaceFP
  let eOpenWorkspaceState  = processOpenWorkspaceFP eOpenWorkspaceFP
  return $ accumB (WorkspaceStateChange WorkspaceChangeInit $ WorkspaceState "" []) (unions [eCreateProjectState,eCreateWorkspaceState,eOpenWorkspaceState])

  where
  processCreateWorkspaceFP :: Frameworks t =>
    Event t FilePath ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processCreateWorkspaceFP eCreateWorkspaceOk = do
    eCreated <- performCreate `ioOnEvent` eCreateWorkspaceOk
    return $ (\fp (WorkspaceStateChange _ (WorkspaceState _ prjs)) -> WorkspaceStateChange (OpenWorkspace fp) $ WorkspaceState fp prjs) <$> eCreated
    where performCreate = createIfNotExist newWorkspaceFile

  processOpenWorkspaceFP :: Event t FilePath -> Event t (WorkspaceStateChange -> WorkspaceStateChange)
  processOpenWorkspaceFP eOpenWorkspaceFP =
    (\fp (WorkspaceStateChange _ (WorkspaceState _ prjs)) -> WorkspaceStateChange (OpenWorkspace fp) $ WorkspaceState fp prjs) <$> eOpenWorkspaceFP

  processCreateProjectFP :: Frameworks t =>
    Event t FilePath ->  Moment t (Event t (WorkspaceStateChange -> WorkspaceStateChange))
  processCreateProjectFP eCreateProjectOk = do
    eCreated <- performCreate `ioOnEvent` eCreateProjectOk
    return $ (\fp (WorkspaceStateChange _ (WorkspaceState wfp prjs)) -> WorkspaceStateChange (OpenProject fp) $ WorkspaceState wfp (fp:prjs)) <$> eCreated
    where performCreate = createIfNotExist newProjectFile

createIfNotExist :: (FilePath -> IO ()) -> FilePath -> IO ()
createIfNotExist newFileFunc fp = do
            fileExists <- doesFileExist fp -- if file doesn't exist then assume we are trying to create it
            if fileExists
              then return ()
              else newFileFunc fp

newProjectFile :: FilePath -> IO ()
newProjectFile fp = do
  writeFile fp ""

newWorkspaceFile :: FilePath -> IO ()
newWorkspaceFile fp = do
  writeFile fp "[]"
