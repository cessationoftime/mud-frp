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

module RBWX.Banana.WX.Core.Core (
  module RBWX.Banana.WX.Core.Lift,
  module RBWX.Banana.WX.Core.ContextMenu,
  mapIOreaction,
  mapIOchainreaction,
  ChainIO
  ,wxID_ANY
) where

import RBWX.Banana.WX.Core.Lift
import RBWX.Banana.WX.Core.ContextMenu
import Reactive.Banana
import Reactive.Banana.Frameworks

-- | perform the IO on the given event, use the output of the IO to create a new event
mapIOreaction :: Frameworks t => (a -> IO b) -> Event t a ->  Moment t (Event t b)
mapIOreaction func ev = do
    (adder,handler) <- liftIO newAddHandler
    reactimate $ handler <$> ev
    fromAddHandler (func `mapIO` adder)

type ChainIO a = (a -> IO ()) -> IO ()
-- | perform IO on the given event, allow the IO to trigger a new event
mapIOchainreaction :: Frameworks t => ChainIO a -> Event t b -> Moment t (Event t a)
mapIOchainreaction func ev = do
    (adder,handler) <- liftIO newAddHandler
    reactimate $ (func  handler) <$ ev
    fromAddHandler adder


-- from defs.h
wxID_ANY :: Int
wxID_ANY = -1
