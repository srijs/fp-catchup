module Quicksort where

qs :: [Int] -> [Int]
qs []     = []
qs (x:xs) = qs left ++ [x] ++ qs right
  where
    left  = [y | y <- xs, y <= x]
    right = [y | y <- xs, y >  x]
