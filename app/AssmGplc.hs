-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This is a development tool that generates GPLC bytecode from GPLC source code.

module Main where

import System.IO
import System.IO.Unsafe
import System.Environment
import Data.Maybe
import Data.List.Split
import Control.Exception

data Bytecode_gen_error = Invalid_opcode | Undefined_value_used deriving (Show)

instance Exception Bytecode_gen_error

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

fst' (a, b, c, d) = a
snd' (a, b, c, d) = b
third' (a, b, c, d) = c
fourth (a, b, c, d) = d

-- These six functions are part of a pipeline that transforms GPLC keywords into op - codes and their reference arguments into the data block pointers used by the interpreter.
assm_gplc5 :: [[Char]] -> [Int] -> Int -> [Int]
assm_gplc5 [] size_block size = (size_block ++ [size])
assm_gplc5 (x:xs) size_block size =
  if x == "--signal" then assm_gplc5 (drop 1 xs) (size_block ++ [size]) 0
  else if x == "pass_msg" then assm_gplc5 (drop 1 xs) size_block (size + 1)
  else if x == "block" then assm_gplc5 xs size_block (size + 3)
  else if x == "npc_damage" then assm_gplc5 xs size_block (size + 1)
  else assm_gplc5 xs size_block (size + 1)

assm_gplc4 :: [[Char]] -> [Int]
assm_gplc4 [] = []
assm_gplc4 (x0:x1:xs) = (read x1) : assm_gplc4 xs

assm_gplc3 :: Int -> [Char] -> [[Char]] -> [(Int, Int)] -> Int
assm_gplc3 t symbol [] [] = error ("Undeclared op - code reference argument used: " ++ symbol)
assm_gplc3 t symbol (x:xs) (y:ys) =
  if symbol == x then
    if t == 0 then fst y
    else (snd y)
  else assm_gplc3 t symbol xs ys

assm_gplc2 :: Int -> [[Char]] -> [[Char]] -> [(Int, Int)] -> [Int]
assm_gplc2 1 (x0:x1:x2:x3:x4:xs) sym ind = (read x0) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (read x3) : [read x4]
assm_gplc2 2 (x0:x1:x2:x3:x4:x5:x6:x7:x8:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : (assm_gplc3 0 x5 sym ind) : (assm_gplc3 0 x6 sym ind) : (assm_gplc3 0 x7 sym ind) : [assm_gplc3 0 x8 sym ind]
assm_gplc2 3 (x0:x1:x2:x3:x4:x5:x6:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : (assm_gplc3 0 x5 sym ind) : [assm_gplc3 0 x6 sym ind]
assm_gplc2 4 (x0:x1:x2:x3:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : [assm_gplc3 0 x3 sym ind]
assm_gplc2 5 (x0:x1:x2:x3:x4:x5:xs) sym ind = (assm_gplc3 1 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : [assm_gplc3 0 x5 sym ind]
assm_gplc2 6 (x0:x1:x2:x3:x4:x5:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : [assm_gplc3 0 x5 sym ind]
assm_gplc2 7 (x0:x1:x2:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : [assm_gplc3 0 x2 sym ind]
assm_gplc2 8 (x0:x1:x2:x3:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : [assm_gplc3 0 x3 sym ind]
assm_gplc2 9 (x0:xs) sym ind = [assm_gplc3 0 x0 sym ind]
assm_gplc2 10 (x0:x1:x2:x3:x4:x5:x6:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : (assm_gplc3 0 x5 sym ind) : [assm_gplc3 0 x6 sym ind]
assm_gplc2 11 (x0:x1:x2:x3:xs) sym ind = (read x0) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : [assm_gplc3 0 x3 sym ind]
assm_gplc2 12 (x0:x1:x2:x3:x4:x5:x6:xs) sym ind = (read x0) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : (assm_gplc3 0 x5 sym ind) : [assm_gplc3 0 x6 sym ind]
assm_gplc2 13 (x0:x1:xs) sym ind = (assm_gplc3 0 x1 sym ind) : proc_ints xs
assm_gplc2 14 (x0:x1:x2:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : [assm_gplc3 0 x2 sym ind]
assm_gplc2 15 (x0:x1:x2:x3:xs) sym ind = (read x0) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : [assm_gplc3 0 x3 sym ind]
assm_gplc2 16 (x0:x1:x2:x3:x4:x5:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : [read x5]
assm_gplc2 17 (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 0 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : (assm_gplc3 0 x4 sym ind) : (assm_gplc3 0 x5 sym ind) : (assm_gplc3 0 x6 sym ind) : (assm_gplc3 0 x7 sym ind) : (assm_gplc3 0 x8 sym ind) : (assm_gplc3 0 x9 sym ind) : (assm_gplc3 0 x10 sym ind) : (read x11) : [assm_gplc3 0 x12 sym ind]
assm_gplc2 18 (x0:x1:x2:x3:x4:xs) sym ind = (assm_gplc3 0 x0 sym ind) : (assm_gplc3 1 x1 sym ind) : (assm_gplc3 0 x2 sym ind) : (assm_gplc3 0 x3 sym ind) : [assm_gplc3 0 x4 sym ind]
assm_gplc2 19 (x0:x1:xs) sym ind = (assm_gplc3 0 x0 sym ind) : [assm_gplc3 0 x1 sym ind]
assm_gplc2 20 (x0:xs) sym ind = [assm_gplc3 1 x0 sym ind]
assm_gplc2 21 (x0:xs) sym ind = [assm_gplc3 1 x0 sym ind]
assm_gplc2 22 (x0:xs) sym ind = [read x0]
assm_gplc2 23 (x0:x1:xs) sym ind = (assm_gplc3 1 x0 sym ind) : [read x1]

assm_gplc1 :: [[Char]] -> Int -> Int -> [[Char]] -> [(Int, Int)] -> [Char] -> [Char] -> Int -> ([[Char]], [(Int, Int)], [Char])
assm_gplc1 [] offset i acc0 acc1 log prog_name mode = (acc0, acc1, log)
assm_gplc1 (x0:x1:xs) offset i acc0 acc1 log prog_name mode =
  if mode == 1 then 
    if i == 0 then assm_gplc1 xs offset (i + 1) (acc0 ++ [x0]) (acc1 ++ [(i, offset + i)]) (log ++ "\n\nProgram " ++ prog_name ++ " starts here...\n\nSymbol: " ++ x0 ++ "  Read binding: " ++ show i ++ "  Write binding: " ++ show (offset + i)) prog_name mode
    else assm_gplc1 xs offset (i + 1) (acc0 ++ [x0]) (acc1 ++ [(i, offset + i)]) (log ++ "\n\nSymbol: " ++ x0 ++ "  Read binding: " ++ show i ++ "  Write binding: " ++ show (offset + i)) prog_name mode
  else assm_gplc1 xs offset (i + 1) (acc0 ++ [x0]) (acc1 ++ [(i, offset + i)]) log prog_name mode
assm_gplc1 (x0:xs) offset i acc0 acc1 log prog_name mode = error ("\n\nassm_gplc1: " ++ x0 ++ "...")

-- This function recognises the keywords that correspond to op - codes and is the beginning of the pipeline that transforms them and their arguments into bytecode.
assm_gplc0 :: [[Char]] -> [[Char]] -> [(Int, Int)] -> [Int] -> [Int] -> [Int] -> [Char] -> Int -> [Int]
assm_gplc0 [] sym ind size_block sig_block code_block prog_name c = sig_block ++ [536870911] ++ code_block ++ [536870911]
assm_gplc0 (x:xs) sym ind size_block sig_block code_block prog_name c =
  let msg_length = (read (xs !! 0))
  in
  if x == "if" then assm_gplc0 (drop 5 xs) sym ind size_block sig_block (code_block ++ [1] ++ (assm_gplc2 1 (take 5 xs) sym ind)) prog_name (c + 6)
  else if x == "chg_state" then assm_gplc0 (drop 9 xs) sym ind size_block sig_block (code_block ++ [2] ++ (assm_gplc2 2 (take 9 xs) sym ind)) prog_name (c + 10)
  else if x == "chg_grid" then assm_gplc0 (drop 7 xs) sym ind size_block sig_block (code_block ++ [3] ++ (assm_gplc2 3 (take 7 xs) sym ind)) prog_name (c + 8)
  else if x == "send_signal" then assm_gplc0 (drop 4 xs) sym ind size_block sig_block (code_block ++ [4] ++ (assm_gplc2 4 (take 4 xs) sym ind)) prog_name (c + 5)
  else if x == "chg_value" then assm_gplc0 (drop 6 xs) sym ind size_block sig_block (code_block ++ [5] ++ (assm_gplc2 5 (take 6 xs) sym ind)) prog_name (c + 7)
  else if x == "chg_floor" then assm_gplc0 (drop 6 xs) sym ind size_block sig_block (code_block ++ [6] ++ (assm_gplc2 6 (take 6 xs) sym ind)) prog_name (c + 7)
  else if x == "chg_ps1" then assm_gplc0 (drop 3 xs) sym ind size_block sig_block (code_block ++ [7] ++ (assm_gplc2 7 (take 3 xs) sym ind)) prog_name (c + 4)
  else if x == "chg_obj_type" then assm_gplc0 (drop 4 xs) sym ind size_block sig_block (code_block ++ [8] ++ (assm_gplc2 8 (take 4 xs) sym ind)) prog_name (c + 5)
  else if x == "place_hold" then assm_gplc0 (drop 1 xs) sym ind size_block sig_block (code_block ++ [9] ++ (assm_gplc2 9 (take 1 xs) sym ind)) prog_name (c + 2)
  else if x == "chg_grid_" then assm_gplc0 (drop 7 xs) sym ind size_block sig_block (code_block ++ [10] ++ (assm_gplc2 10 (take 7 xs) sym ind)) prog_name (c + 8)
  else if x == "copy_ps1" then assm_gplc0 (drop 4 xs) sym ind size_block sig_block (code_block ++ [11] ++ (assm_gplc2 11 (take 4 xs) sym ind)) prog_name (c + 5)
  else if x == "copy_lstate" then assm_gplc0 (drop 7 xs) sym ind size_block sig_block (code_block ++ [12] ++ (assm_gplc2 12 (take 7 xs) sym ind)) prog_name (c + 8)
  else if x == "pass_msg" then assm_gplc0 (drop msg_length xs) sym ind size_block sig_block (code_block ++ [13] ++ (assm_gplc2 13 (take msg_length xs) sym ind)) prog_name (c + msg_length)
  else if x == "chg_ps0" then assm_gplc0 (drop 3 xs) sym ind size_block sig_block (code_block ++ [14] ++ (assm_gplc2 14 (take 3 xs) sym ind)) prog_name (c + 4)
  else if x == "copy_ps0" then assm_gplc0 (drop 4 xs) sym ind size_block sig_block (code_block ++ [15] ++ (assm_gplc2 15 (take 4 xs) sym ind)) prog_name (c + 5)
  else if x == "block" then assm_gplc0 (drop 4 xs) sym ind size_block sig_block (code_block ++ [5, 536870910, 0, read (xs !! 0), assm_gplc3 0 (xs !! 1) sym ind, assm_gplc3 0 (xs !! 2) sym ind, assm_gplc3 0 (xs !! 3) sym ind]) prog_name (c + 7)
  else if x == "binary_dice" then assm_gplc0 (drop 6 xs) sym ind size_block sig_block (code_block ++ [16] ++ (assm_gplc2 16 (take 6 xs) sym ind)) prog_name (c + 7)
  else if x == "project_init" then assm_gplc0 (drop 13 xs) sym ind size_block sig_block (code_block ++ [17] ++ (assm_gplc2 17 (take 13 xs) sym ind)) prog_name (c + 14)
  else if x == "project_update" then assm_gplc0 (drop 5 xs) sym ind size_block sig_block (code_block ++ [18] ++ (assm_gplc2 18 (take 5 xs) sym ind)) prog_name (c + 6)
  else if x == "init_npc" then assm_gplc0 (drop 2 xs) sym ind size_block sig_block (code_block ++ [19] ++ assm_gplc2 19 (take 2 xs) sym ind) prog_name (c + 3)
  else if x == "npc_decision" then assm_gplc0 (tail xs) sym ind size_block sig_block (code_block ++ [20] ++ assm_gplc2 20 (take 1 xs) sym ind) prog_name (c + 2)
  else if x == "npc_move" then assm_gplc0 (tail xs) sym ind size_block sig_block (code_block ++ [21] ++ assm_gplc2 21 (take 1 xs) sym ind) prog_name (c + 2)
  else if x == "npc_damage" then assm_gplc0 (drop 1 xs) sym ind size_block sig_block (code_block ++ [22] ++ assm_gplc2 22 (take 1 xs) sym ind) prog_name (c + 2)
  else if x == "cpede_move" then assm_gplc0 (drop 2 xs) sym ind size_block sig_block (code_block ++ [23] ++ assm_gplc2 23 (take 2 xs) sym ind) prog_name (c + 3)
  else if x == "--signal" then assm_gplc0 (drop 1 xs) sym ind (tail size_block) (sig_block ++ [read (xs !! 0), c, head size_block]) code_block prog_name c
  else error ("\nprog_name: " ++ prog_name ++ "\nc: " ++ show c ++ "\nInvalid op_code: " ++ x)

-- This function is used for program instancing.  The map structure data is used to specify where each GPLC program is placed in the map.  A single program can be placed in multiple
-- locations with its value block patched to different states in each case.
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

build_gplc :: [Char] -> [Char] -> [Char] -> [Int] -> Int -> ([Int], [[Char]], [(Int, Int)], [Char])
build_gplc prog_name source0 source1 fst_pass c =
  let block_sizes = assm_gplc5 (drop 2 (splitOneOf "\n " source1)) [] 0
      bindings = assm_gplc1 (splitOneOf "\n " source0) ((length fst_pass) + 2) 0 [] [] [] prog_name 1
      bindings' = assm_gplc1 (splitOneOf "\n " source0) 0 0 [] [] [] prog_name 0
      d_list_len = length (fst__ bindings)
      out = (assm_gplc0 (splitOneOf "\n " source1) (fst__ bindings) (snd__ bindings) block_sizes [] [] prog_name 0)
      out' = (assm_gplc0 (splitOneOf "\n " source1) (fst__ bindings') (snd__ bindings') block_sizes [] [] prog_name 0)
  in
  if c == 0 then build_gplc prog_name source0 source1 out' 1
  else (((length out) + 2 + d_list_len) : 0 : 0 : out, fst__ bindings, snd__ bindings, third_ bindings)

main = do
  args <- getArgs
  h0 <- openFile (args !! 0) ReadMode
  h1 <- openFile (args !! 1) ReadMode
  h2 <- openFile (args !! 2) WriteMode
  source <- hGetContents h0
  structure <- hGetContents h1
  code <- prep_gplc (splitOn "\n~\n" source) [] [] [] 0
  hPutStr h2 (place_gplc (filter (/= "~") (splitOneOf " \n" structure)) (fst__ code) (snd__ code) (third_ code))
  hClose h0
  hClose h1
  hClose h2

prep_gplc :: [[Char]] -> [[Int]] -> [[[Char]]] -> [[(Int, Int)]] -> Int -> IO ([[Int]], [[[Char]]], [[(Int, Int)]])
prep_gplc [] code_acc sym_acc ind_acc c = return (code_acc, sym_acc, ind_acc)
prep_gplc (x0:x1:x2:xs) code_acc sym_acc ind_acc c =
  let build_gplc' = build_gplc x0 x1 x2 [] 0
      code = fst' build_gplc' ++ assm_gplc4 (splitOneOf " \n" x1)
      sym = snd' build_gplc'
      ind = third' build_gplc'
  in do
  putStr ("\n\nGPLC program " ++ x0 ++ " starts at index " ++ show c)
  prep_gplc xs (code_acc ++ [code]) (sym_acc ++ [sym]) (ind_acc ++ [ind]) (c + ((head (fst' build_gplc')) + 5))
