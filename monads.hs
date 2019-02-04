{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad

-- my List monad
infixr 5 :-:
data List a = Nil
            | a :-: (List a)
  deriving (Show, Eq)

concatenate :: List (List a) -> List a
concatenate Nil = Nil
concatenate (Nil :-: xss) = concatenate xss
concatenate ((x:-:xs) :-: xss) = x :-: concatenate (xs:-:xss) 

instance Functor List where
  fmap f Nil = Nil
  fmap f (x:-:xs) = (f x) :-: (fmap f xs)

instance Applicative List where
  pure x = x :-: Nil
  (f :-: fs) <*> (x :-: xs) = (f x) :-: (fs <*> xs)
  Nil <*> _ = Nil
  _ <*> Nil = Nil

instance Monad List where
  return x = x :-: Nil
  (>>=) xs f = concatenate (fmap f xs)

-- My State monad
newtype State s a = State {effect :: s -> (s,a)}

instance Monad (State s) where
  return x = State $ \s -> (s,x)
  sa >>= f = State $ \s -> let (s1,a) = effect sa s
                               (s2,b) = effect (f a) s1
                           in  (s2,b)

returnState :: s -> State s a -> s
returnState initalState sa = fst (effect sa initalState)

returnValue :: s -> State s a -> a
returnValue initalState sa = snd (effect sa initalState)

instance Applicative (State s) where
  pure = return
  sab <*> sa  = State $ \s -> let (s1,ab) = effect sab s
                                  (s2,a)  = effect sa s1 
                              in  (s2,ab a)

instance Functor (State s) where
  fmap ab sa = State $ \s -> let (s1,a) = effect sa s
                             in  (s1,ab a)

type Counter = Int

factorial :: Int -> Int
factorial 0 = 1 
factorial n = n * (factorial $ n - 1)

factorialS :: Int -> State Counter Int
factorialS 0 = State $ \s -> (0, 1)
factorialS n = State $ \s -> let (previousState, previousValue) = effect (factorialS $ n-1) 0
                             in  (previousState + 1, previousValue * n)

class Recursive r where
  isBase :: r -> Bool
  recurse :: r -> r
  backtrack :: r -> r
  backtrack n = if isBase n then n else backtrack $ recurse n

type Nat = Int

instance Recursive Nat where
  isBase m = m == 0 
  recurse m = if m == 0 then 0 else m - 1

factBase :: Nat -> Int
factBase 0 = 1

factStep :: Nat -> Int -> Int
factStep = \n -> (*n)

assembleSteps :: Recursive r => (r -> a -> a) -> r -> (a -> a)
assembleSteps step argument =
  if isBase argument
     then id
     else step argument . (assembleSteps step $ recurse argument)

runRecursive :: Recursive r => (r -> a) -> (r -> a -> a) -> r -> a
runRecursive baseCase step argument = let f = assembleSteps step argument
                                      in f (baseCase (backtrack argument))

liftRecursive:: Recursive a => (a -> a) -> (a -> a) -> a -> State Counter a
liftRecursive base step x = if isBase x
                               then State $ \s -> (0, base x)
                               else State $ \s -> (s + 1, f $ previous x)

-- factorial 2 
-- State $ \s -> (s+1, returnValue 0 $ factorial 1)
-- State $ \s -> (s+1, returnValue 0 $ State $ \s -> (s+1, returnValue 0 $ factorial 0))
-- State $ \s -> (s+1, returnValue 0 $ State $ \s -> (s+1, returnValue 0 $ State $ \s -> (0, 1)))
-- State $ \s -> (s+1, returnValue 0 $ State $ \s -> (s+1, returnValue 0 $ State $ \s -> (0, 1)))
-- State $ \s -> (s+1, returnValue 0 $ State $ \s -> (s+1, 1))
-- State $ \s -> (s+1, returnValue 0 $ State $ \s -> (s+1, 1))
-- State $ \s -> (s+1, returnValue 0 $ State $ \s -> (s+1, 1))
-- State $ \s -> (s+1, 1)

-- My State monad with state logging
data LoggingState s a = Show s => LoggingState {effectAndLog :: s -> IO((s,a))}

instance Show s => Monad (LoggingState s) where
  -- if a Record (newtype Record = Record {entry :: a}) has just a single entry 
  -- then Record x ~>  Record {entry = x}
  -- so on the following line LoggingState $ f ~> LoggingState {effectAndLog = f}
  return x = LoggingState $ \s -> return (s,x)
  sa >>= f = LoggingState $ \s -> do (s1,a) <- effectAndLog sa s
                                     (s2,b) <- effectAndLog (f a) s1
                                     print $ show s2
                                     return (s2,b)

instance Show s => Applicative (LoggingState s) where
  pure = return
  (<*>) = Control.Monad.ap

instance Show s => Functor (LoggingState s) where
  fmap = Control.Monad.liftM

-- returnState :: Show s => s -> LoggingState s a -> IO(s)
-- returnState initalState sa = do (s,a) <- effectAndLog sa initalState
--                                 return s
                                
-- returnValue :: Show s => s -> LoggingState s a -> IO(a)
-- returnValue initalState sa = do (s,a) <- effectAndLog sa initalState
--                                 return a

emptyListM :: LoggingState Counter [Int]
emptyListM = LoggingState $ \s -> return (0,[])

prependListM :: Int -> [Int] -> LoggingState Counter [Int]
prependListM n ns = LoggingState $ \s -> return (s+1,n:ns)

prependList :: Int -> [Int] -> [Int]
prependList n ns = n:ns
