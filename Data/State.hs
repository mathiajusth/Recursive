{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.State where

import Control.Monad

-- State monad
newtype State s a = State {effect :: s -> (s,a)}

instance Monad (State s) where
  return x = State $ \s -> (s,x)
  sa >>= f = State $ \s -> let (s1,a) = effect sa s
                               (s2,b) = effect (f a) s1
                           in  (s2,b)

returnState :: State s a -> s -> s
returnState sa initalState = fst (effect sa initalState)

returnValue :: State s a -> s -> a
returnValue sa initalState = snd (effect sa initalState)

instance Applicative (State s) where
  pure = return
  sab <*> sa  = State $ \s -> let (s1,ab) = effect sab s
                                  (s2,a)  = effect sa s1 
                              in  (s2,ab a)

instance Functor (State s) where
  fmap ab sa = State $ \s -> let (s1,a) = effect sa s
                             in  (s1,ab a)

--------------------
   -- EXAMPLES --
--------------------

--------------------
-- Factorial with counter
--------------------
type Counter = Int

-- pure factorial
factorial :: Int -> Int
factorial 0 = 1 
factorial n = n * factorial (n - 1)

-- factorial with recursion stack counter
factorialS :: Int -> State Counter Int
factorialS 0 = State $ \s -> (0, 1)
factorialS n = State $ \s -> let (previousState, previousValue) = effect (factorialS $ n-1) 0
                             in  (previousState + 1, previousValue * n)

