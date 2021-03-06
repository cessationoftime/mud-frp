
-----------------------------------------------------------------------------
--
-- Module      :  OutputIsInput
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
--  This module handles side effects, and provides functions to use in the situation where
--  output (reactimate) needs to trigger IO and that IO generates a value to use as a new input to the network.
--
-----------------------------------------------------------------------------

--TODO, rename module to InputIsOutput.
module RBWX.Banana.WX.Core.Core (
   module RBWX.Banana.WX.Core.Lift
  ,module RBWX.Banana.WX.Core.ContextMenu
  ,mapIOreaction
  ,mapIOreaction2
  ,mapIOchainreaction
  ,mapIOchainreaction2
  ,ioOnEvent
  ,ioOnEvent2
  ,ioAccumB
  ,ioAccumChanges
  ,ioOnChanges
  ,ioFilterE
  ,ChainIO
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

ioFilterE :: Frameworks t =>
  (a -> IO Bool) -> Event t a ->  Moment t (Event t a)
ioFilterE func ev = do
  (adder,handler) <- liftIO newAddHandler

  let doIO aa = (func aa) >>= (\x -> if x then handler aa else return ())

  reactimate $ doIO <$> ev
  fromAddHandler adder




-- | perform the IO on the given event, create a new event
ioOnEvent :: Frameworks t =>
  (a -> IO ()) -> Event t a ->  Moment t (Event t a)
ioOnEvent func ev = do
     (adder,handler) <- liftIO newAddHandler
     reactimate $ (\aa -> func aa >> handler aa) <$> ev
     fromAddHandler adder

ioOnEvent2 :: Frameworks t =>
  (a -> IO b) -> Event t a ->  Moment t (Event t (a,b))
ioOnEvent2 func ev = do
     (event,handler) :: (Event t (a,b), Handler (a, b))  <- Frame.newEvent
     reactimate $ (\aa -> func aa >>= (\bb -> handler (aa,bb))) <$> ev
     return event

     {-
     (event,handler) :: (Event t (a,b), Handler (a, b))  <- Frame.newEvent
    -- the chainfunction will call the handler to trigger the new event
    reactimate $ (\evb ->
        let handler2 = (\aaa -> handler (evb,aaa))
        in chainfunc handler2
      ) <$> ev
    return event
-}
ioOnChanges :: Frameworks t =>
  (a -> IO ()) -> Behavior t a ->  Moment t (Event t a)
ioOnChanges func beh = do
     (event,handler) :: (Event t a, Handler a)  <- Frame.newEvent
     --(adder,handler) <- liftIO newAddHandler
     changeEvent <- changes beh
     reactimate' $ (\aa ->  (\bb -> func bb >> handler bb) <$> aa) <$> changeEvent
     --fromAddHandler adder
     return event

-- | start with an initial value and combine with incoming events, perform the IO action upon receiving the event. Use the IO output to upade the Behavior.
ioAccumB :: Frameworks t =>
  a -> Event t (a -> IO a) ->  Moment t (Behavior t a)
ioAccumB acc ev = do
    (eventZ,eventZIn) <- Frame.newEvent
    -- track state as a behavior, run IO function from event and use output to update the behavior.
    let steppingBehavior = stepper acc eventZ
        b2 = (\bb ee -> ee bb >>= eventZIn) <$> steppingBehavior
    -- update the behavior with the new state using reactimate and eventZInput
    reactimate $ b2 <@> ev

    return steppingBehavior

-- | start with an initial value and combine with incoming changes, perform the IO action upon receiving the change event. Use the IO output to update the output Behavior.
ioAccumChanges :: Frameworks t =>
  a -> Behavior t e -> (e -> a -> IO a) -> Moment t (Behavior t a)
ioAccumChanges acc beh ioFunc = do
    (eventZ,eventZIn) <- Frame.newEvent
    changeEvent <- changes beh
    -- track state as a behavior, run IO function from event and use output to update the behavior.
    let steppingBehavior = stepper acc eventZ
        b2 = (\bb ff -> (\ee ->ioFunc ee bb >>= eventZIn) <$> ff ) <$> steppingBehavior
    -- update the behavior with the new state using reactimate and eventZInput
    reactimate' $ b2 <@> changeEvent

    return steppingBehavior

-- | internal shared function for mapIOReaction and mapIOreaction2
procIO :: Handler b -> (a -> IO b) -> a -> IO ()
procIO eventBInput eventFunc inp= eventFunc inp >>= eventBInput

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
mapIOreaction3 :: Frameworks t =>
  Event t (IO b) ->  Moment t (Event t b)
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
-- | perform IO on the given event, allow the IO to trigger a new event.
mapIOchainreaction :: Frameworks t =>
   ChainIO a -> Event t b -> Moment t (Event t a)
mapIOchainreaction func ev = do
    (adder,handler) <- Frame.newEvent
    reactimate $ (func  handler) <$ ev
    return adder

-- | perform IO on the given event, allow the IO to trigger a new event. Pass the value of the triggering event to the output event
mapIOchainreaction2 :: Frameworks t => forall a b.
   ChainIO b -> Event t a -> Moment t (Event t (a, b))
mapIOchainreaction2 chainfunc ev = do
    (event,handler) :: (Event t (a,b), Handler (a, b))  <- Frame.newEvent
    -- the chainfunction will call the handler to trigger the new event
    reactimate $ (\evb ->
        let handler2 = (\aaa -> handler (evb,aaa))
        in chainfunc handler2
      ) <$> ev
    return event

-- from defs.h
wxID_ANY :: Int
wxID_ANY = -1
