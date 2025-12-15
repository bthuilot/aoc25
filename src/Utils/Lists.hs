{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

module Utils.Lists (groupsOf, allEqual, uniquePairs) where

import Data.List (tails)


-- | 'groupsOf' splits a list of items
-- into groups of a given size. If the list
-- can not be divided into the given amount evenly,
-- the last element of the list will be the remainder
groupsOf :: Int -> [a] -> [[a]]
groupsOf amt xs 
  | length xs <= amt = [xs]
  | otherwise = take amt xs : groupsOf amt (drop amt xs)

-- | 'allEqual' will check if all elements in
-- a list are equal, lazily 
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (a : recur@(b : _)) = (a == b) && allEqual recur


-- | 'uniquePairs' will create unique pairings for each
-- item in the given list with every other item in the list
uniquePairs :: [a] -> [(a, a)]
uniquePairs list = [ (x, y) | (x:rest) <- tails list, y <- rest ]
