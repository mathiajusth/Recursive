module Utils.List where

isHetero :: (Eq a) => [a] -> Bool
isHetero [] = True
isHetero (x:xs) = notElem x xs && isHetero xs

isHomo :: (Eq a) => [a] -> Bool
isHomo [] = True
isHomo (x:xs) = filter (==x) xs == xs

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x:removeDuplicates (filter (/= x) xs)

isInRange :: Int -> Int -> Int -> Bool
isInRange p q x = x `elem` [p..q]
