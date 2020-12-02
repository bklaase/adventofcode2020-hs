module Solutions.Day2 where

-- import System.IO
import Text.ParserCombinators.ReadP
import qualified AoC.Lib as L

day2 = L.Solution 2 parse part1 part2

parse :: String -> [PassRow]
parse = map (fst . head . readP_to_S passRowP) . lines

part1 :: [PassRow] -> Int
part1 = length . (filter valid)
  where valid x = (prmin x) <= l && l <= (prmax x)
          where l = length $ filter (== prchar x) (prpass x)

part2 :: [PassRow] -> Int
part2  = length . (filter valid)
  where valid x = c1 /= c2
          where c1 = (prpass x) !! ((prmin x) - 1) == (prchar x)
                c2 = (prpass x) !! ((prmax x) - 1) == (prchar x)

data PassRow = PassRow {prmin::Int, prmax::Int, prchar::Char, prpass::String} deriving Show

passRowP :: ReadP PassRow
passRowP = do
  from <- munch1 (/= '-')
  satisfy (== '-')
  till <- munch1 (/= ' ')
  skipSpaces
  character <- get
  satisfy (== ':')
  skipSpaces
  pass <- look
  return $ PassRow (read from)  (read till) character pass
                             
