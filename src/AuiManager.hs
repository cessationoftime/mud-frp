{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  AuiManager
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

module AuiManager (
   createAuiManager
  ,withUnderlying
  ,withParent
  ,withWindow
) where
import RBWX.RBWX
import EventInputs (AuiManagerInputs(..), unite, eiAddPane)
import GHC.Exts

class Fusion a where
  type FusionInput t a
  type FusionOutput t a
 -- fusionInput :: a -> FusionInput t a
  --fusionOutput :: a -> FusionOutput t a



-- We need a way to setup both input and output prior to the events providing the input
-- can we make an output implement an input?

data AuiManagerOutputs t = AuiManagerOutputs

data AuiManagerWidget = AuiManagerWidget (AuiManager ()) (Frame ())

{-
class HasEvents a where
  type Input a b :: Constraint
  type Output a t
  outputs :: (Frameworks t,Input a b) => a -> [b] -> Moment t (Output a t)

instance HasEvents AuiManagerWidget where
  type Input AuiManagerWidget b = AuiManagerInputs b
  type Output AuiManagerWidget t = AuiManagerOutputs t
  outputs (AuiManagerWidget aui frame1) inputs = let eAddPane = unions $ addPane `map` inputs in do
     reactimate $  (\panes -> (sequence_ [auiManagerAddPane aui p loc title | (p,loc,title) <- panes]) >> (auiManagerUpdate aui)) <$> eAddPane
     return AuiManagerOutputs
-}

class WXWidget a where
  type Underlying a
  type WidgetParent a
  withUnderlying :: a -> (Underlying a -> IO()) -> IO ()
  withParent :: a -> (WidgetParent a -> IO()) -> IO ()





class (WXWidget a) => WindowWidget a where
  withWindow :: a -> (Window () -> IO()) -> IO ()


instance WXWidget AuiManagerWidget where
  type Underlying AuiManagerWidget = AuiManager ()
  type WidgetParent AuiManagerWidget = Frame ()
  withUnderlying (AuiManagerWidget aui _) f = f aui
  withParent (AuiManagerWidget _ p) f = f p

instance WindowWidget AuiManagerWidget where
  withWindow (AuiManagerWidget aui _) f = f $ objectCast aui



createAuiManager :: Frameworks t => Frame () -> Moment t AuiManagerWidget
createAuiManager frame1 = do
    aui <- liftIO $ setup frame1
    return $ AuiManagerWidget aui frame1

setup :: Frame () -> IO (AuiManager ())
setup frame1 = do
  aui <- liftIO  $ auiManagerCreate frame1 wxAUI_MGR_DEFAULT
  let addPane b c w =  auiManagerAddPane aui w b c >> return ()

  -- If the frame itself is destroyed, then unInit the auiManager. Ignore other window destroy events
  liftIO $ windowOnDestroy frame1 $ (withCurrentEvent (\ev ->
    do b <- eventObjectIsWindow ev frame1
       if b then auiManagerUnInit aui else return ()) )

  return aui
