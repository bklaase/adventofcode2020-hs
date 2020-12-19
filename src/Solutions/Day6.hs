{-# LANGUAGE OverloadedStrings #-}
module Solutions.Day6 where

import Data.List
import qualified Data.Text as T
import qualified AoC.Lib as L

day6 = L.Solution 6 parse part1 part2

parse = map T.unpack . T.splitOn "\n\n" . T.pack

part1 :: [String] -> Int
part1 = sum . map (length . nub . concat . lines)

part2 :: [String] -> Int
part2 = sum . map (length . (foldr1 intersect) . lines)
