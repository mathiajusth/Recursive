{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.List where

import Control.Monad()

-- List monad
infixr 5 :-:
data List a = Nil
            | a :-: (List a)
  deriving (Show, Eq)

concatenate :: List (List a) -> List a
concatenate Nil = Nil
concatenate (Nil :-: xss) = concatenate xss
concatenate ((x:-:xs) :-: xss) = x :-: concatenate (xs:-:xss) 

instance Functor List where
  fmap _ Nil = Nil
  fmap f (x:-:xs) = f x :-: fmap f xs

instance Applicative List where
  pure x = x :-: Nil
  (f :-: fs) <*> (x :-: xs) = f x :-: (fs <*> xs) -- this is not what Applicative [] does
  Nil <*> _ = Nil
  _ <*> Nil = Nil

instance Monad List where
  return x = x :-: Nil
  (>>=) xs f = concatenate (fmap f xs)

