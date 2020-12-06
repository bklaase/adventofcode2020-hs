{-# LANGUAGE OverloadedStrings #-}
module Solutions.Day4 where

import Text.ParserCombinators.ReadP
import qualified Data.Text as T
import Data.List
import qualified AoC.Lib as L

day4 = L.Solution 4 parse (length . part1) (length . part2)

parse :: String -> [String]
parse = map T.unpack . T.splitOn "\n\n" . T.pack

part1 :: [String] -> [[(String, String)]]
part1 = filter validId . map processId
  where reqFields = filter (/= "cid") fields
        validId = null . (\\) reqFields . map fst


part2 :: [String] -> [[(String, String)]]
part2  = filter (all validateField) . part1

-- valid field names
fields = ["byr","iyr","eyr","hgt","hcl","ecl","pid","cid"]

-- field validators
validateField :: (String,String) -> Bool

validateField ("byr",v) = readBetween v 1920 2002
validateField ("iyr",v) = readBetween v 2010 2020
validateField ("eyr",v) = readBetween v 2020 2030
validateField ("hgt",v) = let (l,u) = span (<= '9') v in
                            case u of "cm" -> readBetween l 150 193
                                      "in" -> readBetween l 59 76
                                      _ -> False
validateField ("hcl",v) = let (pound,color) = span (== '#') v;
                              hex = concat[['a'..'f'],['0'..'9']] in
                            pound == "#" && length color == 6 && all (`elem` hex) color
validateField ("ecl",v) = v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validateField ("pid",v) = length v == 9 && all (`elem` ['0'..'9']) v
validateField ("cid",_) = True -- always checks out
validateField  (_,_) = False

readBetween :: String -> Int -> Int -> Bool
readBetween s lb ub = lb <= val && val <= ub
  where val = read s :: Int

-- run parser over single id string
processId :: String -> [(String,String)]
processId = proc []
  where proc rs "" = rs
        proc rs inp = proc (nr:rs) ninp
          where step = fieldS inp
                nr = (fst . head) step
                ninp = (snd . head) step

-- parser for id to fields
fieldP :: ReadP (String,String)
fieldP = do
 field <- choice $ map string fields
 char ':'
 value <- munch1 $ not . sep
 munch sep
 return (field,value)
 where sep = (\c -> c == ' ' || c == '\n')

fieldS = readP_to_S fieldP
