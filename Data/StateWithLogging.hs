{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad

-- My State monad with state logging
data StateWithLogging s a = Show s => StateWithLogging {effectAndLog :: s -> IO(s,a)}

instance Show s => Monad (StateWithLogging s) where
  return x = StateWithLogging $ \s -> return (s,x)
  sa >>= f = StateWithLogging $
    \s -> do (s1,a) <- effectAndLog sa s
             (s2,b) <- effectAndLog (f a) s1
             print $ show s2
             return (s2,b)

instance Show s => Applicative (StateWithLogging s) where
  pure = return
  (<*>) = Control.Monad.ap

instance Show s => Functor (StateWithLogging s) where
  fmap = Control.Monad.liftM

returnLogState :: Show s => StateWithLogging s a-> s -> IO s
returnLogState sa initalState = do (s,a) <- effectAndLog sa initalState
                                   return s
                                
returnLogValue :: Show s => StateWithLogging s a -> s -> IO a
returnLogValue sa initalState = do (s,a) <- effectAndLog sa initalState
                                   return a
--------------------
   -- EXAMPLES --
--------------------

--------------------
-- List with length
--------------------
type Counter = Int

emptyList :: StateWithLogging Counter [Int]
emptyList = StateWithLogging $ \s -> return (0,[])

prependList :: Int -> [Int] -> StateWithLogging Counter [Int]
prependList n ns = StateWithLogging $ \s -> return (s+1,n:ns)
