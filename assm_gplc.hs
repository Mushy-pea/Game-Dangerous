module Main where

import System.IO
import System.IO.Unsafe
import System.Environment
import Data.Maybe
import Data.List.Split
import Build_model

read_ :: [Char] -> Int -> Int
read_ x location = unsafePerformIO (putStr ("\nfrag: " ++ x ++ " location " ++ show location ++ " ") >> return (read x))

-- These functions transform GPLC assembly code into the lists of integers that encode for each program in the map
assm_gplc4 :: [[Char]] -> [Char]
assm_gplc4 [] = []
assm_gplc4 (x0:x1:xs) = x1 ++ ", " ++ assm_gplc4 xs

-- These two functions transform op - code reference arguments into the offset pointers used by the GPLC interpreter
assm_gplc3 :: Int -> [Char] -> [[Char]] -> [(Int, Int)] -> Int
assm_gplc3 t symbol [] [] = 536870910
assm_gplc3 t symbol (x:xs) (y:ys) =
  if symbol == x then
    if t == 0 then fst y
    else (snd y)
  else assm_gplc3 t symbol xs ys

assm_gplc2 :: Int -> [[Char]] -> [[Char]] -> [(Int, Int)] -> [Int]
assm_gplc2 1 (x0:x1:x2:x3:x4:xs) sym ind = (read_ x0 1) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (read_ x3 2) : [read_ x4 3]
assm_gplc2 2 (x0:x1:x2:x3:x4:x5:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : [assm_gplc3 0 x5 sym ind]
assm_gplc2 3 (x0:x1:x2:x3:x4:x5:x6:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : (assm_gplc3 0 x5 sym ind) : [assm_gplc3 0 x6 sym ind]
assm_gplc2 4 (x0:x1:x2:x3:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : [assm_gplc3 0 x3 sym ind]
assm_gplc2 5 (x0:x1:x2:x3:x4:x5:xs) sym ind = (assm_gplc3 1 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : [assm_gplc3 0 x5 sym ind]
assm_gplc2 6 (x0:x1:x2:x3:x4:x5:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : [assm_gplc3 0 x5 sym ind]
assm_gplc2 7 (x0:x1:x2:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : [assm_gplc3 0 x2 sym ind]
assm_gplc2 8 (x0:x1:x2:x3:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : [assm_gplc3 0 x3 sym ind]
assm_gplc2 9 (x0:xs) sym ind = [assm_gplc3 0 x0 sym ind]
assm_gplc2 10 (x0:x1:x2:x3:x4:x5:x6:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : (assm_gplc3 0 x5 sym ind) : [assm_gplc3 0 x6 sym ind]
assm_gplc2 11 (x0:x1:x2:x3:xs) sym ind = (read_ x0 6) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : [assm_gplc3 0 x3 sym ind]
assm_gplc2 12 (x0:x1:x2:x3:x4:x5:x6:xs) sym ind = (read_ x0 7) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : (assm_gplc3 0 x5 sym ind) : [assm_gplc3 0 x6 sym ind]
assm_gplc2 13 (x0:x1:xs) sym ind = (assm_gplc3 0 x1 sym ind) : proc_ints xs
assm_gplc2 14 (x0:x1:x2:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : [assm_gplc3 0 x2 sym ind]
assm_gplc2 15 (x0:x1:x2:x3:xs) sym ind = (read_ x0 8) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : [assm_gplc3 0 x3 sym ind]

-- This function recognises the keywords that correspond to op - codes and is the beginning of the pipeline that transforms them and their arguments into output code
assm_gplc1 :: [[Char]] -> [[Char]] -> [(Int, Int)] -> [Int]
assm_gplc1 [] sym ind = []
assm_gplc1 (x:xs) sym ind = 
  if x == "if" then 1 : (assm_gplc2 1 (take 5 xs) sym ind) ++ assm_gplc1 (drop 5 xs) sym ind
  else if x == "chg_state" then 2 : (assm_gplc2 2 (take 6 xs) sym ind) ++ assm_gplc1 (drop 6 xs) sym ind
  else if x == "chg_grid" then 3 : (assm_gplc2 3 (take 7 xs) sym ind) ++ assm_gplc1 (drop 7 xs) sym ind
  else if x == "send_signal" then 4 : (assm_gplc2 4 (take 4 xs) sym ind) ++ assm_gplc1 (drop 4 xs) sym ind
  else if x == "chg_value" then 5 : (assm_gplc2 5 (take 6 xs) sym ind) ++ assm_gplc1 (drop 6 xs) sym ind
  else if x == "chg_floor" then 6 : (assm_gplc2 6 (take 6 xs) sym ind) ++ assm_gplc1 (drop 6 xs) sym ind
  else if x == "chg_ps1" then 7 : (assm_gplc2 7 (take 3 xs) sym ind) ++ assm_gplc1 (drop 3 xs) sym ind
  else if x == "chg_obj_type" then 8 : (assm_gplc2 8 (take 4 xs) sym ind) ++ assm_gplc1 (drop 4 xs) sym ind
  else if x == "place_hold" then 9 : (assm_gplc2 9 (take 1 xs) sym ind) ++ assm_gplc1 (drop 1 xs) sym ind
  else if x == "chg_grid_" then 10 : (assm_gplc2 10 (take 7 xs) sym ind) ++ assm_gplc1 (drop 7 xs) sym ind
  else if x == "copy_ps1" then 11 : (assm_gplc2 11 (take 4 xs) sym ind) ++ assm_gplc1 (drop 4 xs) sym ind
  else if x == "copy_lstate" then 12 : (assm_gplc2 12 (take 7 xs) sym ind) ++ assm_gplc1 (drop 7 xs) sym ind
  else if x == "pass_msg" then 13 : (assm_gplc2 13 (take (read_ (xs !! 0) 9) xs) sym ind) ++ assm_gplc1 (drop (read_ (xs !! 0) 10) xs) sym ind
  else if x == "chg_ps0" then 14 : (assm_gplc2 14 (take 3 xs) sym ind) ++ assm_gplc1 (drop 3 xs) sym ind
  else if x == "copy_ps0" then 15 : (assm_gplc2 15 (take 4 xs) sym ind) ++ assm_gplc1 (drop 4 xs) sym ind
  else if x == "block" then [5, 0, 1] ++ [read_ (xs !! 0) 14] ++ [assm_gplc3 0 (xs !! 1) sym ind] ++ [assm_gplc3 0 (xs !! 2) sym ind] ++ [assm_gplc3 0 (xs !! 3) sym ind] ++ assm_gplc1 (drop 4 xs) sym ind
  else if x == "--signal" then assm_gplc1 (drop 1 xs) sym ind
  else if x == "x" then 536870911 : assm_gplc1 xs sym ind
  else (read_ x 11) : assm_gplc1 xs sym ind

assm_gplc0 :: [[Char]] -> Int -> Int -> [[Char]] -> [(Int, Int)] -> ([[Char]], [(Int, Int)])
assm_gplc0 [] offset i acc0 acc1 = (acc0, acc1)
assm_gplc0 (x0:x1:xs) offset i acc0 acc1 =
  assm_gplc0 xs offset (i + 1) (acc0 ++ [x0]) (acc1 ++ [(i, offset + i)])

det_offset :: [[Char]] -> Int -> Int
det_offset [] c = c + 3
det_offset (x:xs) c =
  if x == "if" then det_offset (drop 5 xs) (c + 6)
  else if x == "chg_state" then det_offset (drop 6 xs) (c + 7)
  else if x == "chg_grid" then det_offset (drop 7 xs) (c + 8)
  else if x == "send_signal" then det_offset (drop 4 xs) (c + 5)
  else if x == "chg_value" then det_offset (drop 6 xs) (c + 7)
  else if x == "chg_floor" then det_offset (drop 6 xs) (c + 7)
  else if x == "chg_ps1" then det_offset (drop 3 xs) (c + 4)
  else if x == "chg_obj_type" then det_offset (drop 4 xs) (c + 5)
  else if x == "place_hold" then det_offset (drop 1 xs) (c + 2)
  else if x == "chg_grid_" then det_offset (drop 7 xs) (c + 8)
  else if x == "copy_ps1" then det_offset (drop 4 xs) (c + 5)
  else if x == "copy_lstate" then det_offset (drop 7 xs) (c + 8)
  else if x == "pass_msg" then det_offset (drop 2 xs) (c + 2)
  else if x == "chg_ps0" then det_offset (drop 3 xs) (c + 4)
  else if x == "copy_ps0" then det_offset (drop 4 xs) (c + 5)
  else if x == "block" then det_offset (drop 4 xs) (c + 7)
  else if x == "--signal" then det_offset (drop 1 xs) c
  else det_offset xs (c + 1)

build_gplc :: [Char] -> [Char] -> [Char] -> [Int]
build_gplc header source0 source1 =
  let bindings = assm_gplc0 (splitOneOf "\n " source0) (det_offset (splitOneOf "\n " source1) 0) 0 [] []
      d_list_len = length (fst bindings)
      out = (assm_gplc1 (splitOneOf "\n " source1) (fst bindings) (snd bindings)) ++ [536870911]
  in
  ((length out) + 2 + d_list_len) : 0 : 0 : out

init_ :: [a] -> [a]
init_ x = take ((length x) - 2) x

show_bool :: Bool -> [Char]
show_bool True = "1, "
show_bool False = "0, "

show_ints :: [Int] -> [Char]
show_ints [] = []
show_ints (x:xs) = (show x) ++ ", " ++ show_ints xs

place_gplc :: [[Char]] -> [[Char]] -> [Char]
place_gplc [] _ = []
place_gplc (x0:x1:x2:x3:x4:xs) code =
  if read_ x4 12 == 0 then x0 ++ ", " ++ x1 ++ ", " ++ x2 ++ ", " ++ x3 ++ ", " ++ x4 ++ ", " ++ place_gplc xs code
  else x0 ++ ", " ++ x1 ++ ", " ++ x2 ++ ", " ++ x3 ++ ", " ++ (code !! ((read_ x4 13) - 1)) ++ ", " ++ place_gplc xs code

main = do
  args <- getArgs
  h0 <- openFile (args !! 0) ReadMode
  h1 <- openFile (args !! 1) ReadMode
  h2 <- openFile (args !! 2) WriteMode
  source <- hGetContents h0
  structure <- hGetContents h1
  code <- prep_gplc (splitOn "\n~\n" source) []
  hPutStr h2 (place_gplc (splitOn ", " (concat (splitOn "\n\n" structure))) code)
  hClose h0
  hClose h1
  hClose h2

prep_gplc :: [[Char]] -> [[Char]] -> IO [[Char]]
prep_gplc [] acc = return acc
prep_gplc (x0:x1:x2:xs) acc =
  let code = (show_ints (build_gplc x0 x1 x2)) ++ (init_ (assm_gplc4 (splitOneOf " \n" x1)))
  in do
  putStr ("\n\nAssembling GPLC program " ++ ((splitOn ", " x0) !! 0) ++ ".  Output: " ++ code)
  prep_gplc xs (acc ++ [code])
