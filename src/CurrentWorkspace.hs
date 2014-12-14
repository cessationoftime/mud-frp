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

module CurrentWorkspace (

) where



ioReaction :: Frameworks t =>
   (a -> IO ()) -> Event t a ->  Moment t (Event t a)
ioReaction func ev = do
     (adder,handler) <- liftIO newAddHandler
     reactimate $ (\aa -> func aa >> handler aa) <$> ev
     fromAddHandler adder
