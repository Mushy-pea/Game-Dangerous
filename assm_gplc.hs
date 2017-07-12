module Main where

import System.IO
import System.IO.Unsafe
import System.Environment
import Data.Maybe
import Data.List.Split

read_ :: [Char] -> Int -> Int
read_ x location = unsafePerformIO (putStr ("\nfrag: " ++ x ++ " location " ++ show location ++ " ") >> return (read x))

init_ :: [a] -> [a]
init_ x = take ((length x) - 2) x

show_ints :: [Int] -> [Char]
show_ints [] = []
show_ints (x:xs) = (show x) ++ ", " ++ show_ints xs

proc_ints :: [[Char]] -> [Int]
proc_ints [] = []
proc_ints (x:xs) = (read x :: Int) : proc_ints xs

fst__ (a, b, c) = a
snd__ (a, b, c) = b
third_ (a, b, c) = c

-- These functions transform GPLC assembly code into the lists of integers that encode for each program in the map
assm_gplc4 :: [[Char]] -> [Int]
assm_gplc4 [] = []
assm_gplc4 (x0:x1:xs) = (read x1) : assm_gplc4 xs

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
assm_gplc2 16 (x0:x1:x2:x3:xs) sym ind = (read x0) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : [assm_gplc3 0 x3 sym ind]
assm_gplc2 17 (x0:x1:x2:x3:x4:x5:x6:x7:x8:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : (assm_gplc3 0 x5 sym ind) : (assm_gplc3 0 x6 sym ind) : (assm_gplc3 0 x7 sym ind) : [read_ x8 15]
assm_gplc2 18 (x0:x1:x2:x3:x4:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 1 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : [assm_gplc3 0 x4 sym ind]
assm_gplc2 19 (x0:x1:x2:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (read_ x1 16) : [read_ x2 17]
assm_gplc2 20 (x0:x1:x2:x3:x4:xs) sym ind = (read_ x0 18) : (read_ x1 19) : (read_ x2 20) : (read_ x3 21) : [read_ x4 22]
assm_gplc2 21 (x0:x1:x2:x3:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : [read_ x3 23]

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
  else if x == "chg_save_grid" then 16 : (assm_gplc2 16 (take 4 xs) sym ind) ++ assm_gplc1 (drop 4 xs) sym ind
  else if x == "project_init" then 17 : (assm_gplc2 17 (take 9 xs) sym ind) ++ assm_gplc1 (drop 9 xs) sym ind
  else if x == "project_update" then 18 : (assm_gplc2 18 (take 5 xs) sym ind) ++ assm_gplc1 (drop 5 xs) sym ind
  else if x == "cpede_logic" then 19 : (assm_gplc2 19 (take 3 xs) sym ind) ++ assm_gplc1 (drop 3 xs) sym ind
  else if x == "cpede_move" then 20 : (assm_gplc2 20 (take 5 xs) sym ind) ++ assm_gplc1 (drop 5 xs) sym ind
  else if x == "cpede_damage" then 21 : (assm_gplc2 21 (take 4 xs) sym ind) ++ assm_gplc1 (drop 4 xs) sym ind
  else if x == "--signal" then assm_gplc1 (drop 1 xs) sym ind
  else if x == "x" then 536870911 : assm_gplc1 xs sym ind
  else (read_ x 11) : assm_gplc1 xs sym ind

assm_gplc0 :: [[Char]] -> Int -> Int -> [[Char]] -> [(Int, Int)] -> ([[Char]], [(Int, Int)])
assm_gplc0 [] offset i acc0 acc1 = (acc0, acc1)
assm_gplc0 (x0:x1:xs) offset i acc0 acc1 =
  assm_gplc0 xs offset (i + 1) (acc0 ++ [x0]) (acc1 ++ [(i, offset + i)])

patch_code :: [Int] -> [[Char]] -> [[Char]] -> [(Int, Int)] -> [Int]
patch_code code [] sym ind = code
patch_code code (x0:x1:xs) sym ind =
  let offset = assm_gplc3 1 x0 sym ind
  in
  patch_code (take (offset + 1) code ++ [read x1] ++ drop (offset + 2) code) xs sym ind

place_gplc :: [[Char]] -> [[Int]] -> [[[Char]]] -> [[(Int, Int)]] -> [Char]
place_gplc [] code sym ind = []
place_gplc (x0:x1:x2:x3:x4:x5:xs) code sym ind =
  if read x4 == 0 then x0 ++ ", " ++ x1 ++ ", " ++ x2 ++ ", " ++ x3 ++ ", " ++ x4 ++ ", " ++ place_gplc xs code sym ind
  else if x5 == "y" then x0 ++ ", " ++ x1 ++ ", " ++ x2 ++ ", " ++ x3 ++ ", " ++ show_ints (patch_code (code !! ((read x4) - 1)) (take (read (xs !! 0)) (tail xs)) (sym !! ((read x4) - 1)) (ind !! ((read x4) - 1))) ++ place_gplc (drop (read (xs !! 0)) (tail xs)) code sym ind
  else x0 ++ ", " ++ x1 ++ ", " ++ x2 ++ ", " ++ x3 ++ ", " ++ show_ints (code !! ((read x4) - 1)) ++ place_gplc xs code sym ind

build_gplc :: Int -> [Char] -> [Char] -> [Int] -> ([Int], [[Char]], [(Int, Int)])
build_gplc c source0 source1 fst_pass =
  let bindings = assm_gplc0 (splitOneOf "\n " source0) ((length fst_pass) + 2) 0 [] []
      bindings' = assm_gplc0 (splitOneOf "\n " source0) 0 0 [] []
      d_list_len = length (fst bindings)
      out = (assm_gplc1 (splitOneOf "\n " source1) (fst bindings) (snd bindings)) ++ [536870911]
      out' = (assm_gplc1 (splitOneOf "\n " source1) (fst bindings') (snd bindings')) ++ [536870911]
  in
  if c == 0 then build_gplc 1 source0 source1 out'
  else (((length out) + 2 + d_list_len) : 0 : 0 : out, fst bindings, snd bindings)

main = do
  args <- getArgs
  h0 <- openFile (args !! 0) ReadMode
  h1 <- openFile (args !! 1) ReadMode
  h2 <- openFile (args !! 2) WriteMode
  source <- hGetContents h0
  structure <- hGetContents h1
  code <- prep_gplc (splitOn "\n~\n" source) [] [] []
  hPutStr h2 (place_gplc (splitOn ", " (concat (splitOn "\n\n" structure))) (fst__ code) (snd__ code) (third_ code))
  hClose h0
  hClose h1
  hClose h2

prep_gplc :: [[Char]] -> [[Int]] -> [[[Char]]] -> [[(Int, Int)]] -> IO ([[Int]], [[[Char]]], [[(Int, Int)]])
prep_gplc [] code_acc sym_acc ind_acc = return (code_acc, sym_acc, ind_acc)
prep_gplc (x0:x1:x2:xs) code_acc sym_acc ind_acc =
  let build_gplc' = build_gplc 0 x1 x2 []
      code = fst__ build_gplc' ++ assm_gplc4 (splitOneOf " \n" x1)
      sym = snd__ build_gplc'
      ind = third_ build_gplc'
  in do
  putStr ("\n\nAssembling GPLC program " ++ ((splitOn ", " x0) !! 0) ++ ".  Output: " ++ show_ints code)
  prep_gplc xs (code_acc ++ [code]) (sym_acc ++ [sym]) (ind_acc ++ [ind])
