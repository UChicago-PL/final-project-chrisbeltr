module Brainfuck where

import Data.List.Split (chunksOf)
import GHC.Float (int2Float)
import Logic

main = do
  print "we did it!!!!"

-- just to filter out non valid brainfuck characters
minify :: String -> String
minify = filter (`elem` [',', '.', '<', '>', '[', ']', '+', '-'])

-- turns a brainfuck character into a list of binary
charToBinary :: Char -> [Int]
charToBinary c = case c of
  '.' -> [0, 0, 0, 0]
  ',' -> [0, 0, 0, 1]
  '-' -> [0, 0, 1, 0]
  '+' -> [0, 0, 1, 1]
  '[' -> [0, 1, 0, 0]
  ']' -> [0, 1, 0, 1]
  '<' -> [0, 1, 1, 0]
  '>' -> [0, 1, 1, 1]
  _ -> [1, 1, 1, 1]

-- turns brainfuck characters into a grid
fromBF :: String -> Grid
fromBF input =
  let mini = minify input
      total = length mini
      chars = concatMap charToBinary mini
   in make2D chars

-- turns a list of binary into a brainfuck character
binaryToChar :: [Int] -> Char
binaryToChar list
  | length list /= 4 = ' '
  | otherwise = case list of
      [0, 0, 0, 0] -> '.'
      [0, 0, 0, 1] -> ','
      [0, 0, 1, 0] -> '-'
      [0, 0, 1, 1] -> '+'
      [0, 1, 0, 0] -> '['
      [0, 1, 0, 1] -> ']'
      [0, 1, 1, 0] -> '<'
      [0, 1, 1, 1] -> '>'
      _ -> ' '

-- turns a grid into brainfuck characters
toBF :: Grid -> String
toBF grid = do
  let flat = concat grid
      grouped = chunksOf 4 flat
   in (filter (/= ' ') . map binaryToChar) grouped