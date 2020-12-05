module Solutions.Day3 where

import qualified AoC.Lib as L

day3 = L.Solution 3 parse part1 part2

parse :: String -> [String]
parse = lines

part1 :: [String] -> Int
part1 xs = treeTraverse 1 3 xs

part2 :: [String] -> Int
part2 xs = p1*p2*p3*p4*p5
  where p1 = treeTraverse 1 1 xs
        p2 = treeTraverse 1 3 xs
        p3 = treeTraverse 1 5 xs
        p4 = treeTraverse 1 7 xs
        p5 = treeTraverse 2 1 xs

treeTraverse :: Int -> Int -> [String] -> Int
treeTraverse sd sr xs = length trees
  where trees = filter (== '#') spots
        spots = [ (xs !! (x*sd)) !! (y `mod` 31) |
                  x <- [0.. ((length xs)-1)],
                  (x*sd) < (length xs),
                  let y = x*sr]


-- thoughts..
-- invariant
-- start 0, 0
-- then 1, 3
-- then 2, 6
-- then 3, 9
-- ...
-- then 10, 30
-- then 11, 0
-- ...
-- t/m length input (324 (0-323)
-- so 323, (323*3 % 31)					  

-- invariant = x, x*3 % 31 | x < 323
