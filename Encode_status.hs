-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module Encode_status where

import System.IO
import Data.Array.IArray

-- These functions were originally written for a previous unpublished game project called Maze Game.  They are used to convert numbers from hexadecimal to binary form.
decimal_binary :: Integer -> Integer -> [Int]
decimal_binary d_num factor = if (d_num - factor) < 0 then 0 : decimal_binary d_num (div factor 2)
                              else if (d_num - factor) == 0 then [1]
                              else 1 : decimal_binary (d_num - factor) (div factor 2)

pad :: [Int] -> [Int] -> Int -> Int -> [Int]
pad num acc pad_size c =
  if c == pad_size then take (pad_size + 1) (num ++ acc)
  else pad num (0 : acc) pad_size (c + 1)

hex_decimal :: [Char] -> Integer -> Integer
hex_decimal [] c = 0
hex_decimal (x:xs) c = (count2 x) ^ c + hex_decimal xs (c + 1)

count2 :: Char -> Integer
count2 '0' = 0
count2 '1' = 1
count2 '2' = 2
count2 '3' = 3
count2 '4' = 4
count2 '5' = 5
count2 '6' = 6
count2 '7' = 7
count2 '8' = 8
count2 '9' = 9
count2 'A' = 10
count2 'B' = 11
count2 'C' = 12
count2 'D' = 13
count2 'E' = 14
count2 'F' = 15
count2 x = error ("\nInvalid character in level unlock code: " ++ [x])
