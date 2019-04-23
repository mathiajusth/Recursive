{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Recursive where

import Data.State

class BaseOf r where
  isBasal :: r -> Bool

class (Functor m, BaseOf r) => Recursive m r where
  recurse :: Functor m => r -> m r

assemble :: (Recursive m a, BaseOf a) => (a -> m b -> b) -> (a -> b) -> a -> b
assemble step base arg =
  if isBasal arg
     then base arg
     else step arg $ fmap (assemble step base) (recurse arg)


type Count = Int

instance Initializable Count where
  initial = 0

-- assemble and count the number of recursive calls
assembleWithCount :: (Recursive m a, BaseOf a, Foldable m) => (a -> m b -> b) -> (a -> b) -> a -> State Count b
assembleWithCount step base arg =
  let stepS arg msb = State $ \s -> (combine . fmap (flip effect s)) msb
        where combine mcb = ((+1).sum $ fmap fst mcb
                            ,step arg $ fmap snd mcb
                            )
      baseS arg = State $ \s -> (s, base arg)
      in assemble stepS baseS arg
