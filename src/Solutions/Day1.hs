module Solutions.Day1 where

import qualified AoC.Lib as L

day1 = L.Solution 1 parse part1 part2

parse :: (Integral a, Read  a) => String -> [a]
parse = map read . lines

part1 :: (Integral a) => [a] -> a
part1 xs = head [ x*y | x <- xs, y <-xs, x+y == 2020]  

part2 :: (Integral a) => [a] -> a
part2 xs = head [ x*y*z | x <- xs, y <-xs, z <- xs, x+y+z == 2020]  
