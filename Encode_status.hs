-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- The functions in this module are part of the system that handles progression between maps.  Upon reaching the transition point between two maps
-- the user is presented with a 32 hex digit level unlock code, within which is encoded certain game state values.  This code will be required to unlock
-- the map being entered.  A high level of redundancy is added to these codes (only 61 of 128 bits are used) and a bit reordering step is performed during encoding
-- and decoding.  This means a naive attempt to guess a valid code has a 1 in 2 ^ 67 chance of success, as codes that would result in game state values outside a
-- valid range are rejected.  This approach has been taken to avoid adding complexity to the per map game state saving system.

module Main where

import System.IO
import Data.Array.IArray
import Data.Maybe
import Build_model

bit_reorder_scheme = [122,31,71,38,13,126,12,27,115,118,106,96,0,10,26,78,108,17,11,14,7,42,15,77,113,100,1,92,87,104,105,6,58,43,8,53,3,2,9,4,18,68,86,102,22,30,74,91,57,85,20,107,61,95,72,34,125,50,65,114,88,35,94,21,54,59,63,80,112,41,76,99,39,121,117,73,48,66,28,46,69,70,120,97,51,23,111,82,81,98,127,56,33,60,37,32,49,64,90,123,45,79,25,52,109,16,103,83,47,124,19,5,29,62,119,44,67,110,84,36,101,55,24,89,116,75,93,40] :: [Int]

main = do
  putStr "\nEnter unlock code: "
  code <- getLine
  test_decode code

test_decode :: [Char] -> IO ()
test_decode code =
  let bit_list = pad (decimal_binary (hex_decimal code 0) (2 ^ 127)) [] 127 0
      reordered_bit_list = reorder_bits bit_reorder_scheme (listArray (0, 127) bit_list) (array (0, 127) [(i, 0) | i <- [0 .. 127]]) 0
      game_state = extract_state_values reordered_bit_list ps0_init ps1_init 0
  in do
  if isNothing game_state == False then putStr ("\n\n" ++ show (fst (fromJust game_state)) ++ "\n\n" ++ show (snd (fromJust game_state)))
  else putStr ("\n\nInvalid map unlock code entered.")

-- These five functions were originally written for a previous unpublished game project called Maze Game.  They are used to convert numbers from hexadecimal to binary form.
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

count1 :: [Int] -> Char
count1 xs = case xs of
             [0, 0, 0, 0] -> '0'
             [0, 0, 0, 1] -> '1'
             [0, 0, 1, 0] -> '2'
             [0, 0, 1, 1] -> '3'
             [0, 1, 0, 0] -> '4'
             [0, 1, 0, 1] -> '5'
             [0, 1, 1, 0] -> '6'
             [0, 1, 1, 1] -> '7'
             [1, 0, 0, 0] -> '8'
             [1, 0, 0, 1] -> '9'
             [1, 0, 1, 0] -> 'A'
             [1, 0, 1, 1] -> 'B'
             [1, 1, 0, 0] -> 'C'
             [1, 1, 0, 1] -> 'D'
             [1, 1, 1, 0] -> 'E'
             [1, 1, 1, 1] -> 'F'

-- This function is used to convert numbers from binary to hexadecimal form.
binary_to_hex :: Array Int Int -> Int -> [Char]
binary_to_hex bit_arr i =
  if i == 128 then []
  else count1 [bit_arr ! i, bit_arr ! (i + 1), bit_arr ! (i + 2), bit_arr ! (i + 3)] : binary_to_hex bit_arr (i + 4)

-- This function re - orders the 128 bit block derived from the 32 hex digit level unlock code, which adds some obfuscation.
reorder_bits :: [Int] -> Array Int Int -> Array Int Int -> Int -> Array Int Int
reorder_bits [] bit_arr_in  bit_arr_out i = bit_arr_out
reorder_bits (x:xs) bit_arr_in  bit_arr_out i = reorder_bits xs bit_arr_in (bit_arr_out // [(i, bit_arr_in ! x)]) (i + 1)

-- This function checks a specified block of bits from the 128 bit unlock code and returns the integer that this encodes.
det_state_values :: Array Int Int -> Int -> Int -> Int -> Int -> Int -> Int
det_state_values bit_arr total order len c i =
  if c == len then total
  else
    if bit_arr ! i == 1 then det_state_values bit_arr (total + 2 ^ order) (order - 1) len (c + 1) (i + 1)
    else det_state_values bit_arr total (order - 1) len (c + 1) (i + 1)

-- This function is used to encode the difficulty field of Play_state1 as an integer.
difficulty_to_int :: ([Char], Int, Int, Int) -> Int
difficulty_to_int ("Hey, not too risky!", 6, 8, 10) = 0
difficulty_to_int ("Plenty of danger please.", 6, 10, 14) = 1
difficulty_to_int ("Ultra danger.", 10, 15, 20) = 2
difficulty_to_int ("Health and safety nightmare!", 15, 20, 25) = 3

-- This function encodes the value of the keys field of Play_state1 as an integer.
keys_to_int :: [Int] -> Int
keys_to_int (red : green : blue : yellow : purple : white : xs) =
  let key_value = \key -> if key == 77 then 32
                          else if key == 78 then 16
                          else if key == 79 then 8
                          else if key == 80 then 4
                          else if key == 81 then 2
                          else if key == 82 then 1
                          else 0
  in (key_value red) + (key_value green) + (key_value blue) + (key_value yellow) + (key_value purple) + (key_value white)

-- This function is used to set the difficulty based on an integer.
set_difficulty :: Int -> ([Char], Int, Int, Int)
set_difficulty 0 = ("Hey, not too risky!", 6, 8, 10)
set_difficulty 1 = ("Plenty of danger please.", 6, 10, 14)
set_difficulty 2 = ("Ultra danger.", 10, 15, 20)
set_difficulty 3 = ("Health and safety nightmare!", 15, 20, 25)

-- This function is used to set the keys value within Play_state1.
set_keys :: [Int] -> Int -> [Int]
set_keys [] char_num = []
set_keys (x:xs) char_num =
  if x == 1 then char_num : set_keys xs (char_num + 1)
  else 0 : set_keys xs (char_num + 1)

-- This function initialises certain values in the Play_state0 and Play_state1 structures based on the information encoded in the level unlock code.
extract_state_values :: Array Int Int -> Play_state0 -> Play_state1 -> Int -> Maybe (Play_state0, Play_state1)
extract_state_values bit_arr s0 s1 0 =
  let health_ = det_state_values bit_arr 0 15 16 0 0
  in if health_ < 256 then extract_state_values bit_arr s0 (s1 {health = health_}) 1
     else Nothing
extract_state_values bit_arr s0 s1 1 =
  let ammo_ = det_state_values bit_arr 0 15 16 0 16
  in if ammo_ < 256 then extract_state_values bit_arr s0 (s1 {ammo = ammo_}) 2
     else Nothing
extract_state_values bit_arr s0 s1 2 =
  let gems_ = det_state_values bit_arr 0 15 16 0 32
  in if gems_ < 256 then extract_state_values bit_arr s0 (s1 {gems = gems_}) 3
     else Nothing
extract_state_values bit_arr s0 s1 3 =
  let torches_ = det_state_values bit_arr 0 15 16 0 48
  in if torches_ < 256 then extract_state_values bit_arr s0 (s1 {torches = torches_}) 4
     else Nothing
extract_state_values bit_arr s0 s1 4 =
  let keys_ = det_state_values bit_arr 0 15 16 0 64
  in if keys_ < 64 then extract_state_values bit_arr s0 (s1 {keys = set_keys (pad (decimal_binary (fromIntegral keys_) 32) [] 5 0) 77}) 5
     else Nothing
extract_state_values bit_arr s0 s1 5 =
  let difficulty_ = det_state_values bit_arr 0 15 16 0 80
  in if difficulty_ < 4 then extract_state_values bit_arr s0 (s1 {difficulty = set_difficulty difficulty_}) 6
     else Nothing
extract_state_values bit_arr s0 s1 6 =
  let time = det_state_values bit_arr 0 15 16 0 96
  in if time < 65536 then extract_state_values bit_arr (s0 {game_clock = (time * 40, fromIntegral (time * 40), 1)}) s1 7
     else Nothing
extract_state_values bit_arr s0 s1 7 =
  let story_state_ = det_state_values bit_arr 0 15 16 0 112
  in if story_state_ < 256 then Just (s0, s1 {story_state = story_state_})
     else Nothing

encode_state_values :: Play_state0 -> Play_state1 -> [Int]
encode_state_values s0 s1 =
  let health_block = pad (decimal_binary (health s1) (2 ^ 15)) [] 15 0
      ammo_block = pad (decimal_binary (ammo s1) (2 ^ 15)) [] 15 0
      gems_block = pad (decimal_binary (gems s1) (2 ^ 15)) [] 15 0
      torches_block = pad (decimal_binary (torches s1) (2 ^ 15)) [] 15 0
      keys_block = pad (decimal_binary (keys_to_int (keys s1)) (2 ^ 15)) [] 15 0
      difficulty_block = pad (decimal_binary (difficulty_to_int (difficulty s1)) (2 ^ 15)) [] 15 0
      time_block = pad (decimal_binary (div (fst__ (game_clock s0)) 40) (2 ^ 15)) [] 15 0
      story_state_block = pad (decimal_binary (story_state s1) (2 ^ 15)) [] 15 0
  in health_block ++ ammo_block ++ gems_block ++ torches_block ++ keys_block ++ difficulty_block ++ time_block ++ story_state_block

