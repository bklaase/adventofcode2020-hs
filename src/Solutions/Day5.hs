module Solutions.Day5 where

import Data.List
import qualified AoC.Lib as L

day5 = L.Solution 5 parse (maximum . part1) (part2)

parse :: String -> [(String, String)]
parse =  map (span (< 'L')) . lines

part1 :: [(String, String)] -> [Int]
part1 = map ticketToSeatId

part2 :: [(String, String)] -> [Int]
part2 xs = [54..930] \\ (sort $ part1 xs)

upperchars = "BR" :: String
lowerchars = "FL" :: String

ticketToSeatId :: (String, String) -> Int
ticketToSeatId (rs,cs) = 8*row + col
  where row = binseqToNumber rs
        col = binseqToNumber cs

binseqToNumber :: String -> Int
binseqToNumber s =
  proc s 0
    where proc "" r = r
          proc (h:t) r = proc t $ r + (charToBin h) * 2^(length t)

charToBin :: Char -> Int
charToBin c = if elem c upperchars then 1 else 0
