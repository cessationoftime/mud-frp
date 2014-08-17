-----------------------------------------------------------------------------
--
-- Module      :  RBWX.RBWX
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

module RBWX.RBWX (
  module RBWX.Lift,
  module RBWX.ContextMenu,
  mapIOevent,
  mapIOchainevent,
  ChainIO
) where

import RBWX.Lift
import RBWX.ContextMenu
import Reactive.Banana
import Reactive.Banana.Frameworks

-- | perform the IO on the given event, use the output of the IO to create a new event
mapIOevent :: Frameworks t => (a -> IO b) -> Event t a ->  Moment t (Event t b)
mapIOevent func ev = do
    (adder,handler) <- liftIO newAddHandler
    reactimate $ handler <$> ev
    fromAddHandler (func `mapIO` adder)

type ChainIO a = (a -> IO ()) -> IO ()
-- | perform IO on the given event, allow the IO to trigger a new event
mapIOchainevent :: Frameworks t => ChainIO a -> Event t b -> Moment t (Event t a)
mapIOchainevent func ev = do
    (adder,handler) <- liftIO newAddHandler
    reactimate $ (func  handler) <$ ev
    fromAddHandler adder
