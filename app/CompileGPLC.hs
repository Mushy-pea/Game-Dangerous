-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module Main where

import System.IO
import System.Console.ANSI
import System.Environment
import Control.Exception
import Data.List.Split
import Data.Array.IArray

data Token = Token {column :: Int, content :: [Char], textColour :: [Char]} deriving (Eq, Show)

def_token = Token {column = 0, content = [], textColour = []}

detArrayDim :: [[Char]] -> Int -> Int -> Int -> (Int, Int)
detArrayDim [] current_length longest_line line_num
  | current_length > longest_line = (line_num, current_length)
  | otherwise = (line_num, longest_line)
detArrayDim (x:xs) current_length longest_line line_num
  | x == "\n" =
    if current_length > longest_line then detArrayDim xs 0 current_length (line_num + 1)
    else detArrayDim xs 0 longest_line (line_num + 1)
  | otherwise = detArrayDim xs (current_length + 1) longest_line line_num

tokenise :: [[Char]] -> Array (Int, Int) Token -> Int -> Int -> Int -> Array (Int, Int) Token
tokenise [] token_arr col i j = token_arr
tokenise (x:xs) token_arr col i j
  | x == "\n" = tokenise xs token_arr 1 (i + 1) 0
  | otherwise = tokenise xs (token_arr // [((i, j), token_added)]) (col + length x + 1) i (j + 1)
  where token_added = Token {column = col + col_correction, content = x, textColour = "white"}
        col_correction = if i == 0 then 0
                         else 1

main = do
  args <- getArgs
  contents <- bracket (openFile (args !! 0) ReadMode) hClose
                      (\h -> do c <- hGetContents h; putStr ("\nprogram file size: " ++ show (length c)); return c)
  showResult contents

showResult :: [Char] -> IO ()
showResult contents =
  let split_contents = splitOn " " contents
      array_dim = detArrayDim split_contents 0 0 0
      empty_token_array = array ((0, 0), (fst array_dim, (snd array_dim) - 1)) [((i, j), def_token) | i <- [0..fst array_dim], j <- [0..(snd array_dim) - 1]]
      token_arr = tokenise split_contents empty_token_array 1 0 0
  in do
  clearScreen
  showTokenArray token_arr 0 0 (fst array_dim) ((snd array_dim) - 1)

showTokenArray :: Array (Int, Int) Token -> Int -> Int -> Int -> Int -> IO ()
showTokenArray token_arr i j i_max j_max =
  let token = token_arr ! (i, j)
  in do
  if i > i_max then return ()
  else if j > j_max then showTokenArray token_arr (i + 1) 0 i_max j_max
  else if token_arr ! (i, j) == def_token then showTokenArray token_arr (i + 1) 0 i_max j_max
  else do
    setCursorPosition i (column token - 1)
    putStr (content token)
    showTokenArray token_arr i (j + 1) i_max j_max

