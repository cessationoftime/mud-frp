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
  mapIOreaction2,
  mapIOchainreaction,
  ioReaction,
  ChainIO
  ,wxID_ANY
) where

import RBWX.Banana.WX.Core.Lift
import RBWX.Banana.WX.Core.ContextMenu
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Reactive.Banana.Frameworks as Frame

import Control.Applicative
import Control.Monad
import Data.Maybe (isJust, catMaybes)
import Data.Monoid (Monoid(..))

-- | perform the IO on the given event, create a new event
ioReaction :: Frameworks t =>
   (a -> IO ()) -> Event t a ->  Moment t (Event t a)
ioReaction func ev = do
     (adder,handler) <- liftIO newAddHandler
     reactimate $ (\aa -> func aa >> handler aa) <$> ev
     fromAddHandler adder
{-
ioAccumB :: Frameworks t => (a -> f -> IO a) -> a -> Event t f ->  Moment t (Behavior t a)
ioAccumB f acc ev = do
   let ff = f acc
   ev1 <- mapIOreaction  ff ev
   return $ stepper acc ev1


ioAccumB2 :: Frameworks t => (a -> IO a) -> a -> Event t a ->  Moment t (Behavior t a)
ioAccumB2 f acc ev = do
   ev1 <- mapIOreaction  ff ev
   return $ stepper acc ev1

-}


{-
ioAccumB :: Frameworks t => a -> Event t (a -> IO a) ->  Moment t (Behavior t a)
ioAccumB acc ev = do
  (adder,handler) <- liftIO newAddHandler
  reactimate $ handler <$> ev
  fromAddHandler (func `mapIO` adder)
  stepper acc
-}

-- | internal shared function for mapIOReaction and mapIOreaction2
procIO :: (b  -> IO ()) -> (a -> IO b) -> a -> IO ()
procIO eventBInput eventFunc  inp= eventFunc inp >>= eventBInput

-- | perform the IO on the given event, use the output of the IO to create a new event
mapIOreaction :: Frameworks t =>
   (a -> IO b) -> Event t a ->  Moment t (Event t b)
mapIOreaction func ev = do
    -- create new handler
    (eventB,eventBInput) <- Frame.newEvent
    let procIO1 = procIO eventBInput
    -- reactimate on the given event (an output), use this output as input to the system on the newly created addhandler.
    reactimate $ (procIO1 func) <$> ev
    return eventB

-- | perform the IO on the given event, use the output of the IO to create a new event
mapIOreaction2 :: Frameworks t =>
   a -> Event t (a -> IO b) ->  Moment t (Event t b)
mapIOreaction2 inp evFunc = do
    (eventB,eventBInput) <- Frame.newEvent
    let procIO1 = procIO eventBInput
    -- run the function contained in the event. use it's output to trigger a new event with reactimate
    reactimate $ (flip procIO1 inp) <$> evFunc
    return eventB

-- | perform the IO on the given event, use the output of the IO to create a new event
mapIOreaction3 :: Frameworks t => Event t (IO b) ->  Moment t (Event t b)
mapIOreaction3 evFunc = do
    (eventB,eventBInput) <- Frame.newEvent
    let procIO1 = procIO eventBInput
    -- run the function contained in the event. use it's output to trigger a new event with reactimate
    reactimate $ (\iob -> iob >>= eventBInput) <$> evFunc
    return eventB

{-

accumMapIO :: Frameworks t => a -> Event t (a -> IO a) ->  Moment t (Event t a)
accumMapIO inp evFunc = do
  let mapper = flip mapIOreaction2
      dd = mapper evFunc
      beh :: Behavior t a
      beh = accumB $ inp (procer <$> evFunc)
  where
  procer :: (a -> IO a) -> a -> IO a
  procer func stateA =

-}

{-
-- | perform the IO on the given event, use the output of the IO to create a new event
mapIOreactionAB :: Frameworks t => Event t (a -> IO b) ->  Moment t (Event t (a -> b))
mapIOreactionAB evFunc = do
    (eventB,eventBInput) <- Frame.newEvent
    let procAB1 = procAB eventBInput
    -- run the function contained in the event. use it's output to trigger a new event with reactimate
    reactimate $ procAB1 <$> evFunc
    return eventB

    where
    procAB :: (b  -> IO ()) -> (a -> IO b) -> a -> IO ()
    procAB eventBInput eventFunc  inp= do
      va <- eventFunc inp
      eventBInput va
-}
type ChainIO a = (a -> IO ()) -> IO ()
-- | perform IO on the given event, allow the IO to trigger a new event
mapIOchainreaction :: Frameworks t =>
   ChainIO a -> Event t b -> Moment t (Event t a)
mapIOchainreaction func ev = do
    (adder,handler) <- liftIO newAddHandler
    reactimate $ (func  handler) <$ ev
    fromAddHandler adder


-- from defs.h
wxID_ANY :: Int
wxID_ANY = -1
