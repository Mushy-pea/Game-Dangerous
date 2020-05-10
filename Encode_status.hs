-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module Encode_status where

import System.IO
import Data.Array.IArray

bit_reorder_pattern = [122,31,71,38,13,126,12,27,115,118,106,96,0,10,26,78,108,17,11,14,7,42,15,77,113,100,1,92,87,104,105,6,58,43,8,53,3,2,9,4,18,68,86,102,22,30,74,91,57,85,20,107,61,95,72,34,125,50,65,114,88,35,94,21,54,59,63,80,112,41,76,99,39,121,117,73,48,66,28,46,69,70,120,97,51,23,111,82,81,98,127,56,33,60,37,32,49,64,90,123,45,79,25,52,109,16,103,83,47,124,19,5,29,62,119,44,67,110,84,36,101,55,24,89,116,75,93,40] :: [Int]

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
hex_decimal (x:xs) c = (count2 x) * (16 ^ c) + hex_decimal xs (c + 1)

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

reorder_bits :: [Int] -> Array Int Int -> Array Int Int -> Int -> Array Int Int
reorder_bits [] bit_arr_in  bit_arr_out i = bit_arr_out
reorder_bits (x:xs) bit_arr_in  bit_arr_out i = reorder_bits xs bit_arr_in (bit_arr_out // [(i, bit_arr_in ! x)]) (i + 1)

