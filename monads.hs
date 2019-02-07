{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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
  (f :-: fs) <*> (x :-: xs) = (f x) :-: (fs <*> xs) -- this is not what [] does
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

-- factorial with counter
type Counter = Int

factorial :: Int -> Int
factorial 0 = 1 
factorial n = n * (factorial $ n - 1)

factorialS :: Int -> State Counter Int
factorialS 0 = State $ \s -> (0, 1)
factorialS n = State $ \s -> let (previousState, previousValue) = effect (factorialS $ n-1) 0
                             in  (previousState + 1, previousValue * n)

-- recursive function definition
class SimplyRecursive r where
  isBase :: r -> Bool
  recurse :: r -> r
  backtrack :: r -> r
  backtrack n = if isBase n then n else backtrack $ recurse n

assembleSteps :: SimplyRecursive r => (r -> a -> a) -> r -> (a -> a)
assembleSteps step argument =
  if isBase argument
     then id
     else step argument . (assembleSteps step $ recurse argument)

runRecursive :: SimplyRecursive r => (r -> a) -> (r -> a -> a) -> r -> a
runRecursive baseCase step argument =
  let f = assembleSteps step argument
      in f $ baseCase $ backtrack argument

-- example: factorial
type Nat = Int

instance SimplyRecursive Nat where
  isBase m = m == 0 
  recurse m = if m == 0 then 0 else m - 1

factBase :: Nat -> Int
factBase 0 = 1

factStep :: Nat -> Int -> Int
factStep = \n -> (*n)

-- 0 1 2 3 4 5 6  7  8  9
-- 0 1 1 2 3 5 8 13 21 34
-- fib 9 <~ +[fib 8, fib 7] <~ +[+[fib 7, fib 6], +[fib 6, fib 5]]
-- class Recursive r m where
--   recurseAll :: (Monad m, Recursive (m r) m) => r -> m r

-- instance SimplyRecursive a => Recursive [a] [] where
--   recurseAll [] = []
--   recurseAll [x] = [[]]
--   recurseAll xs = (map drop [0..recurse $ length xs]) <*> [xs]

class Recursive r s where
  recurse :: r -> s
-- instance Recursive Int [] where
--   recurseAll 0 = []
--   recurseAll m  = [0..m-1]

-- fibBase 0 = 0
-- fibBase 1 = 1

-- fibStep :: Nat -> [Int] -> Int -- will be replaced by :: [Int]
-- fibStep _ xs = fibStep _ (recurse n !! 0)

-- -- the type will by replaced by more general :: Recursive r, Recursive (R a) => (r -> R)
-- assembleSteps :: Recursive r => (r -> [a] -> a) -> r -> ([a] -> a) 
-- assembleSteps step argument =
--   if isBase argument
--      then id
--      else step argument . (assembleSteps step $ recurse argument)

-- example: tower or hanoi

data Position = Left | Middle | Right
not :: Position -> Position -> Position
not Main.Left Main.Middle = Main.Right
not Main.Left Main.Right = Main.Middle
not Main.Middle Main.Right = Main.Left
not p r = Main.not r p

type Height = Int
data HanoiTask = HanoiTask
  {from   :: Position
  ,to     :: Position
  ,height :: Height
  }

instance SimplyRecursive HanoiTask where
  isBase HanoiTask{height = h} = h == 1
  recurse HanoiTask{from = f, to = t, height = h} = HanoiTask {from = f, to = Main.not f t, height = h - 1}

type Path = [(Position, Position)]

-- hanoiBase :: HanoiTask -> Path
-- hanoiBase HanoiTask{height = 1} = [(from,to)]

-- hanoiStep :: HanoiTask -> Int -> Int
-- hanoiStep HanoiTask{} = 


-- assembleStepsWithCounter :: Recursive r => (r -> a -> a) -> r -> (a -> a)
-- assembleStepsWithCounter step argument

-- runRecursiveWithCounter :: Recursive r => (r -> a) -> (r -> a -> a) -> r -> State Counter a
-- runRecursiveWithCounter baseCase step argument =
--   let f = assembleStepsWithCounter step argument
--       in baseCase >>= (f $ baseCase $ backtrack argument)

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
