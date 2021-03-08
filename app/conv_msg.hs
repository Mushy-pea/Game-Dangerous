-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This is a development tool used to encode text in the message tile reference format used with the GPLC scripting language.

module Main where

import System.IO

char_list = "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 ,'.?;:+-=!<"

findTile :: [Char] -> Char -> Int -> Int
findTile [] t i = i
findTile (x:xs) t i =
  if x == t then i
  else findTile xs t (i + 1)

convMsg :: [Char] -> [Int]
convMsg [] = []
convMsg (x:xs) = findTile char_list x 0 : convMsg xs

showMsg :: [Int] -> [Char]
showMsg [] = []
showMsg (x:xs) = show x ++ " " ++ showMsg xs

main = do
  putStr "message: "
  msg <- getLine
  putStr ("tiles: " ++ (showMsg (convMsg msg)))
  
