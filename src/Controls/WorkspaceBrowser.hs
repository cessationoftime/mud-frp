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

newtype WorkspaceBrowser = WorkspaceBrowser (Panel ())

data WorkspaceBrowserData = WorkspaceBrowserData (Frame ()) (Panel ()) (TreeCtrl ())  (Button ()) (Button ())

layoutWhenWorkspaceUnloaded :: WorkspaceBrowserData -> IO ()
layoutWhenWorkspaceUnloaded (WorkspaceBrowserData frame1 workspacePanel workspaceTree buttonCreateWS buttonOpenWS) = do
  _ <- windowHide workspaceTree
  _ <- windowShow buttonCreateWS
  _ <- windowShow buttonOpenWS
  set workspacePanel [ layout := fill $ column 2 [ widget buttonOpenWS, widget buttonCreateWS ] ]
  return ()

layoutWhenWorkspaceLoaded :: WorkspaceBrowserData -> IO ()
layoutWhenWorkspaceLoaded (WorkspaceBrowserData frame1 workspacePanel workspaceTree buttonCreateWS buttonOpenWS) = do
  _ <- windowShow workspaceTree
  _ <- windowHide buttonCreateWS
  _ <- windowHide buttonOpenWS
  set workspacePanel [ layout := fill $ widget workspaceTree ]
  return ()

--workspaceBrowser :: Frameworks t => Window a -> Event t () -> Event t () -> Moment t (WorkspaceBrowser t)
--workspaceBrowser notebook frame1 eOpen eCreate = do

data WorkspaceBrowserOutputs t = WorkspaceBrowserOutputs Int

createWorkspaceBrowser :: (Frameworks t) => Frame () -> [WorkspaceBrowserInput t] -> (WorkspaceBrowser -> IO ())  -> Moment t (WorkspaceBrowserOutputs t)
createWorkspaceBrowser frame1 inputs setupIO = do
    wbData@(WorkspaceBrowserData _ panel tree buttonCreateWS buttonOpenWS) <- liftIO $ setup frame1
    eButtonCreateWS  <- event0 buttonCreateWS command
    eButtonOpenWS <- event0 buttonOpenWS command
    eCreateDialogOk <- fileDialogOkEvent New "NewWorkspace.n6" [Workspace] frame1 eButtonCreateWS
    eOpenDialogOk <- fileDialogOkEvent Open "" [Workspace] frame1 eButtonOpenWS
    let wbInput = WorkspaceBrowserInput eCreateDialogOk eOpenDialogOk
    liftIO $ setupIO $ WorkspaceBrowser panel
    outputs wbData frame1 $ unite (wbInput:inputs)


outputs :: (Frameworks t) => WorkspaceBrowserData -> Frame () -> WorkspaceBrowserInput t -> Moment t (WorkspaceBrowserOutputs t)
outputs wbData frame1 (WorkspaceBrowserInput eCreateWS eOpenWS)  =
  let e = eCreateWS `union` eOpenWS in do
  reactimate $ (loadWS wbData) <$> e
  return (WorkspaceBrowserOutputs 0)

loadWS :: WorkspaceBrowserData -> FilePath -> IO ()
loadWS wbData@(WorkspaceBrowserData frame1 workspacePanel workspaceTree buttonCreateWS buttonOpenWS) fp = do
    windowFreeze workspacePanel
    layoutWhenWorkspaceLoaded wbData

    -- putStrLn fp
    -- putStrLn fp
   --  putStrLn fp
  -- set top node


    top <- treeCtrlAddRoot workspaceTree "Workspace" (imageIndex imgComputer) imageNone objectNull


    treeCtrlSetItemPath workspaceTree top ""

     -- add root directory
    (rootPath,rootName) <- getRootDir
    root <- treeCtrlAppendItem workspaceTree top rootName (imageIndex imgDisk) imageNone objectNull


    treeCtrlSetItemPath workspaceTree root rootPath
    treeCtrlAddSubDirs workspaceTree root
    treeCtrlExpand workspaceTree root

    _ <- windowLayout frame1 -- need to run this or the workspaceTree can appear tiny and in the wrong location
    windowThaw workspacePanel
    return ()

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


    --TODO: embed the buttons in the workspace browser.
    --TODO: Hide them when a workspace opens. Show them when a workspace closes.
    --TODO: need a context menu option to close the workspace.

    -- TreeCtrl setup
    workspaceTree <- treeCtrl workspacePanel []

    -- image list
    images     <- imageListFromFiles (sz 16 16) imageFiles

    treeCtrlAssignImageList workspaceTree images  {- 'assign' deletes the imagelist on delete -}

   -- set workspacePanel [ layout := fill $ column 2 [ widget openWorkspaceButton, widget createWorkspaceButton, widget workspaceTree ] ]

    let wbData = WorkspaceBrowserData window1 workspacePanel workspaceTree createWorkspaceButton openWorkspaceButton
    layoutWhenWorkspaceUnloaded wbData

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


{--------------------------------------------------------------------------------
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
