{-# OPTIONS_GHC -Wall #-}
module Golf where

-- Returns list of sublists of xs, one for each index i of xs by:
--   * dropping elements before index,
--   * create a list of sublists, each whose head is i positions away from the previous sublist
--   * keep only those with non-empty contents,
--   * from those, build up a string by concatenating the head of each.
skips :: [a] -> [[a]]
skips xs = map (\n ->
                    foldr ((++) . take 1)
                              []
                              $ takeWhile ((0 <) . length)
                              $ iterate (drop (n + 1))
                              $ drop n
                              $ xs)
           [0..length xs -1]

-- Returns a list of local maxima in a list of integers.
-- A local maxima is an element which is strictly larger than each of its neighbours.
-- Neither the first nor last element of a non-empty list can be a local maxima.
-- This is computed by:
--  * repeatedly dropping an element, then, for each sublist
--  * take the first 3 elements, then, for each sublist
--  * take the ones with precisely three elements, and
--  * filter those containing a local maxima in the middle, then
--  * extract the local maxima out of the remaining sublists.
localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_:y:_:_) -> y) $
                 filter (\(x:y:z:_) -> x < y && y > z) $
                 takeWhile ((== 3) . length) $
                 map (take 3) $
                 iterate (drop 1) xs
