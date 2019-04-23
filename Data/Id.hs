module Data.Id where

newtype Id a = Id a

instance Functor Id where
  fmap f  (Id a) = Id $ f a

