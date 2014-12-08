{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
--
-- Module      :  Controls.WorkspaceBrowser
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

module Controls.WorkspaceBrowser (createWorkspaceBrowser,WorkspaceBrowserOutputs, WorkspaceBrowser(WorkspaceBrowser)) where

import System.Directory
import System.FilePath
import Control.Exception
import Data.List( zip3 )
import Paths (getDataFile)
import RBWX.RBWX
import EventInputs
import Dialogs
import qualified Reactive.Banana.Frameworks as Frame


newtype WorkspaceBrowser = WorkspaceBrowser (Panel ())

data WorkspaceBrowserData =
  Nodeless (Frame ()) (Panel ()) (TreeCtrl ())  (Button ()) (Button ()) (Button ()) |
  Noded (Frame ()) (Panel ()) (TreeCtrl ())  (Button ()) (Button ()) (Button ()) (TreeItem) [TreeItem]

data LayoutMode = None | HasWorkspace | HasProject

layoutWhen :: WorkspaceBrowserData -> IO ()
-- when nothing is open
layoutWhen (Nodeless frame1 workspacePanel workspaceTree buttonCreateWS buttonOpenWS buttonCreateProject) = do
      _ <- windowHide workspaceTree
      _ <- windowHide buttonCreateProject
      _ <- windowShow buttonCreateWS
      _ <- windowShow buttonOpenWS
      set workspacePanel [ layout := fill $ column 2 [ widget buttonOpenWS, widget buttonCreateWS ] ]
-- when only workspace is open, no projects
layoutWhen (Noded frame1 workspacePanel workspaceTree buttonCreateWS buttonOpenWS buttonCreateProject wsNode []) = do
      _ <- windowShow workspaceTree
      _ <- windowShow buttonCreateProject
      _ <- windowHide buttonCreateWS
      _ <- windowHide buttonOpenWS
      set workspacePanel [ layout := fill $ row 2 [widget workspaceTree, widget buttonCreateProject ] ]
-- when one or more projects are open
layoutWhen (Noded frame1 workspacePanel workspaceTree buttonCreateWS buttonOpenWS buttonCreateProject wsNode projNodes) = do
      _ <- windowShow workspaceTree
      _ <- windowHide buttonCreateProject
      _ <- windowHide buttonCreateWS
      _ <- windowHide buttonOpenWS
      set workspacePanel [ layout := fill $ widget workspaceTree ]

--workspaceBrowser :: Frameworks t => Window a -> Event t () -> Event t () -> Moment t (WorkspaceBrowser t)
--workspaceBrowser notebook frame1 eOpen eCreate = do

data WorkspaceBrowserOutputs t = WorkspaceBrowserOutputs Int

createWorkspaceBrowser :: (Frameworks t) => Frame () -> [WorkspaceBrowserInput t] -> (WorkspaceBrowser -> IO ())  -> Moment t (WorkspaceBrowserOutputs t)
createWorkspaceBrowser frame1 inp setupIO = do
    wbData@(Nodeless _ panel tree buttonCreateWS buttonOpenWS buttonCreateProject) <- liftIO $ setup frame1
    eButtonCreateProject  <- event0 buttonCreateProject command
    eButtonCreateWS  <- event0 buttonCreateWS command
    eButtonOpenWS <- event0 buttonOpenWS command
    eCreateWorkspaceOk <- fileDialogOkEvent New "NewWorkspace.n6" [Workspace] frame1 eButtonCreateWS
    eOpenWorkspaceOk <- fileDialogOkEvent Open "" [Workspace] frame1 eButtonOpenWS
    eCreateProjectOk <- fileDialogOkEvent New "NewProject.n6proj" [Project] frame1 eButtonCreateProject
    liftIO $ setupIO $ WorkspaceBrowser panel

    -- track workspaceData as a behavior, run IO and use output to update the behavior.
    let wbInput = WorkspaceBrowserInput eCreateWorkspaceOk eOpenWorkspaceOk eCreateProjectOk
        inputs =  unite $ wbInput:inp
        loadW  =  loadWorkspace <$> (eCreateWorkspaceOk `union` eOpenWorkspaceOk)
        loadP = loadProject <$> eCreateProjectOk
        loader = loadW `union` loadP
    workspaceDataBehavior <- ioAccumB wbData loader

    return (WorkspaceBrowserOutputs 0)


--outputs :: (Frameworks t) => WorkspaceBrowserData -> Frame () -> WorkspaceBrowserInput t -> Moment t (WorkspaceBrowserOutputs t)
--outputs wbData frame1 (WorkspaceBrowserInput eCreateWS eOpenWS eCreateProject)  =
 -- let
 -- do


loadProject :: FilePath -> WorkspaceBrowserData -> IO WorkspaceBrowserData
loadProject fp wbData@(Noded frame1 workspacePanel workspaceTree buttonCreateWS buttonOpenWS buttonCreateProject wsNode projNodes) = do
    windowFreeze workspacePanel
    fileExists <- doesFileExist fp -- if file doesn't exist then assume we are trying to create it
    let baseName = takeBaseName fp
    let directory = takeDirectory fp

     -- add root directory
    --(rootPath,rootName) <- getRootDir
    newProjNode <- treeCtrlAppendItem workspaceTree wsNode baseName (imageIndex imgDisk) imageNone objectNull

    treeCtrlSetItemPath workspaceTree wsNode directory
    treeCtrlAddSubDirs workspaceTree wsNode
    --treeCtrlExpandAllChildren workspaceTree root

    let newWbData = Noded frame1 workspacePanel workspaceTree buttonCreateWS buttonOpenWS buttonCreateProject wsNode (newProjNode:projNodes)
    layoutWhen newWbData
    _ <- windowLayout frame1 -- need to run this or the workspaceTree can appear tiny and in the wrong location
    windowThaw workspacePanel
    return newWbData

loadWorkspace :: FilePath -> WorkspaceBrowserData -> IO WorkspaceBrowserData
loadWorkspace fp wbData@(Nodeless frame1 workspacePanel workspaceTree buttonCreateWS buttonOpenWS buttonCreateProject) = do
    windowFreeze workspacePanel
    fileExists <- doesFileExist fp -- if file doesn't exist then assume we are trying to create it
    let baseName = takeBaseName fp
    let directory = takeDirectory fp
  -- set top node
    workspaceNode <- treeCtrlAddRoot workspaceTree ("Workspace: " ++ baseName) (imageIndex imgComputer) imageNone objectNull

    treeCtrlSetItemPath workspaceTree workspaceNode ""

    let newWbData = Noded frame1 workspacePanel workspaceTree buttonCreateWS buttonOpenWS buttonCreateProject workspaceNode []
    layoutWhen newWbData
    _ <- windowLayout frame1 -- need to run this or the workspaceTree can appear tiny and in the wrong location
    windowThaw workspacePanel
    return newWbData

createWsFile :: IO ()
createWsFile = return ()

openWsFile :: IO ()
openWsFile = return ()

setup ::  Frame () -> IO WorkspaceBrowserData
setup window1 = do

    workspacePanel <- panel window1 []
    windowFreeze workspacePanel

    openWorkspaceButton <- button workspacePanel [text := "Open Workspace"]
    createWorkspaceButton <- button workspacePanel [text := "Create Workspace"]
    createProjectButton <- button workspacePanel [text := "Create Project"]


    --TODO: embed the buttons in the workspace browser.
    --TODO: Hide them when a workspace opens. Show them when a workspace closes.
    --TODO: need a context menu option to close the workspace.

    -- TreeCtrl setup
    workspaceTree <- treeCtrl workspacePanel []

    -- image list
    images     <- imageListFromFiles (sz 16 16) imageFiles

    treeCtrlAssignImageList workspaceTree images  {- 'assign' deletes the imagelist on delete -}

    let wbData = Nodeless window1 workspacePanel workspaceTree createWorkspaceButton openWorkspaceButton createProjectButton
    layoutWhen wbData

    windowThaw workspacePanel

    return wbData

{--------------------------------------------------------------------------------
   Images
--------------------------------------------------------------------------------}
imgComputer   = "computer"
imgDisk       = "disk"
imgFile       = "file"
imgHFile      = "hsicon"
imgFolder     = "f_closed"
imgFolderOpen = "f_open"

-- plain names of images
imageNames
  = [imgComputer,imgDisk,imgFile,imgHFile,imgFolder,imgFolderOpen]

-- file names of the images
imageFiles
  = map (\name -> getDataFile $ name ++ ".ico") imageNames

-- get the index of an image
imageIndex :: String -> Int
imageIndex name
  = case lookup name (zip imageNames [0..]) of
      Just idx  -> idx
      Nothing   -> imageNone

-- (-1) means no image present
imageNone :: Int
imageNone     = (-1)

{--------------------------------------------------------------------------------
   The client data of the directory tree is the full path of the
   tree node. Here we wrap the "unsafe" basic calls into safe wrappers.
--------------------------------------------------------------------------------}
treeCtrlSetItemPath :: TreeCtrl a -> TreeItem -> FilePath -> IO ()
treeCtrlSetItemPath t item path
  = treeCtrlSetItemClientData t item (return ()) path

treeCtrlGetItemPath :: TreeCtrl a -> TreeItem -> IO FilePath
treeCtrlGetItemPath t item
  = do mbpath <- unsafeTreeCtrlGetItemClientData t item
       case mbpath of
         Just path -> return path
         Nothing   -> return ""

{--------------------------------------------------------------------------------
   On tree event
--------------------------------------------------------------------------------}
onTreeEvent :: TreeCtrl a -> ListCtrl b -> StatusField -> EventTree -> IO ()
onTreeEvent t l status event
  = case event of
      TreeItemExpanding item veto  | treeItemIsOk item
        -> do wxcBeginBusyCursor
              treeCtrlChildrenAddSubDirs t item
              wxcEndBusyCursor
              propagateEvent
      TreeSelChanged item olditem  | treeItemIsOk item
        -> do wxcBeginBusyCursor
              path <- treeCtrlGetItemPath t item
              set status [text := path]
              listCtrlShowDir l path
              wxcEndBusyCursor
              propagateEvent
      other
        -> propagateEvent

onListEvent :: ListCtrl a -> StatusField -> EventList -> IO ()
onListEvent l status event
  = case event of
      ListItemSelected item
        -> do count <- listCtrlGetSelectedItemCount l
              set status [text := (show count ++ " item" ++ (if count /= 1 then "s" else "") ++ " selected") ]
              propagateEvent
      other
        -> propagateEvent

ioExceptionHandler :: a -> IOException -> IO a
ioExceptionHandler res _ = return res

swallowIOExceptions :: a -> IO a -> IO a
swallowIOExceptions def act =
  act `catch` ioExceptionHandler def

{--------------------------------------------------------------------------------
   View directory files
--------------------------------------------------------------------------------}
listCtrlShowDir :: ListCtrl a -> FilePath -> IO ()
listCtrlShowDir listCtrl path
  = swallowIOExceptions () $
    do itemsDelete listCtrl
       contents <- getDirectoryContents path
       let paths = map (\cont -> path ++ cont) contents
       mapM_ (listCtrlAddFile listCtrl) (zip3 [0..] contents paths)

listCtrlAddFile l (idx,fname,fpath)
  = do isdir <- swallowIOExceptions False $ doesDirectoryExist fpath
       perm  <- getPermissions fpath
       time  <- getModificationTime fpath
       let image = imageIndex (if isdir
                                then imgFolder
                                else if (extension fname == "hs")
                                      then imgHFile
                                      else imgFile)
       listCtrlInsertItemWithLabel l idx fpath image        -- use this instead of 'items' so we can set the image.
       set l [item idx := [fname,showPerm perm,show time]]

extension fname
  | elem '.' fname  = reverse (takeWhile (/='.') (reverse fname))
  | otherwise       = ""

showPerm perm
  = [if readable perm then 'r' else '-'
    ,if writable perm then 'w' else '-'
    ,if executable perm then 'x' else '-'
    ,if searchable perm then 's' else '-'
    ]



{--------------------------------------------------------------------------------
   Directory tree helpers
--------------------------------------------------------------------------------}
treeCtrlChildrenAddSubDirs :: TreeCtrl a -> TreeItem -> IO ()
treeCtrlChildrenAddSubDirs t parent
  = do children <- treeCtrlGetChildren t parent
       mapM_ (treeCtrlAddSubDirs t) children

treeCtrlAddSubDirs :: TreeCtrl a -> TreeItem -> IO ()
treeCtrlAddSubDirs t parent
  = do fpath <- treeCtrlGetItemPath t parent
       dirs  <- getSubdirs fpath
       treeCtrlDeleteChildren t parent
       mapM_ addChild dirs
       treeCtrlSetItemHasChildren t parent (not (null dirs))
  where
    addChild (path,name)
      = do item <- treeCtrlAppendItem t parent name (imageIndex imgFolder) (imageIndex imgFolderOpen) objectNull
           treeCtrlSetItemPath t item path


{----------------------Directory----------------------------------------------------------
   General directory operations
--------------------------------------------------------------------------------}

-- Return the sub directories of a certain directory as a tuple: the full path and the directory name.
getSubdirs :: FilePath -> IO [(FilePath,FilePath)]
getSubdirs fpath
  = do contents  <- swallowIOExceptions [] $ getDirectoryContents fpath
       let names = filter (\dir -> head dir /= '.') $ contents
           paths = map (\dir -> fpath ++ dir ++ "/") names
       isdirs    <- mapM (\dir -> swallowIOExceptions False $ doesDirectoryExist dir) paths
       let dirs  = [(path,name) | (isdir,(path,name)) <- zip isdirs (zip paths names), isdir]
       return dirs



-- Return the root directory as a tuple: the full path and name.
getRootDir :: IO (FilePath,FilePath)
getRootDir
  = do current <- getCurrentDirectory
       let rootName  = takeWhile (not . isPathSeparator) current
           rootPath  = rootName ++ "/"
       exist <- swallowIOExceptions False $ doesDirectoryExist rootPath
       if exist
        then return (rootPath,rootName)
        else return (current ++ "/", reverse (takeWhile (not . isPathSeparator) (reverse current)))
