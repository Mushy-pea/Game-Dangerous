module Main where

import System.IO

char_list = "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 ,'.?;:+-=!<"

find_tile :: [Char] -> Char -> Int -> Int
find_tile [] t i = i
find_tile (x:xs) t i =
  if x == t then i
  else find_tile xs t (i + 1)

conv_msg :: [Char] -> [Int]
conv_msg [] = []
conv_msg (x:xs) = find_tile char_list x 0 : conv_msg xs

main = do
  putStr "message: "
  msg <- getLine
  putStr "tiles: "
  print (conv_msg msg)
