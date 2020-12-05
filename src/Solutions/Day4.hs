{-# LANGUAGE OverloadedStrings #-}
module Solutions.Day4 where

import Text.ParserCombinators.ReadP
import qualified Data.Text as T
import Data.List
import qualified AoC.Lib as L

day4 = L.Solution 4 parse part1 part2

parse :: String -> [String]
parse = map T.unpack . T.splitOn "\n\n" . T.pack

part1 :: [String] -> Int
part1 = length . filter validId . map ((map fst) .  processId)
  where reqFields = filter (/= "cid") fields
        validId = null . (\\) reqFields


part2 :: [String] -> Int
part2 xs = length xs

processId :: String -> [(String,String)]
processId = proc []
  where proc rs "" = rs
        proc rs inp = proc (nr:rs) ninp
          where step = fieldS inp
                nr = (fst . head) step
                ninp = (snd . head) step


fields = ["byr","iyr","eyr","hgt","hcl","ecl","pid","cid"]

-- parser
fieldP :: ReadP (String,String)
fieldP = do
 field <- choice $ map string fields
 char ':'
 value <- munch1 $ not . sep
 munch sep
 return (field,value)
 where sep = (\c -> c == ' ' || c == '\n')

fieldS = readP_to_S fieldP
