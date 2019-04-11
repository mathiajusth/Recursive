module Utils.Miscellaneous where

transposeValues :: (Eq a) => (a,a) -> a -> a
transposeValues (v1,v2) val
  | val == v1 = v2
  | val == v2 = v1
  | otherwise = val
