-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module Main where

import System.IO
import System.Console.ANSI
import System.Environment
import Control.Exception
import Data.List.Split
import Data.Array.IArray
import Data.Char

fst__ (a, b, c) = a
snd__ (a, b, c) = b
third_ (a, b, c) = c

data Token = Token {column :: Int, content :: [Char], textColour :: [Char]} deriving (Eq, Show)

defToken = Token {column = 0, content = [], textColour = []}

data Symbol_binding = Symbol_binding {symbol :: [Char], initialValue :: Int, readDeRefKey :: Int, writeDeRefKey :: Int} deriving Show

data Opcode_arg_type = RefRead | RefWrite | Const

data Opcode = Opcode {bytecode :: Int, instructionLength :: Int, arguments :: [Opcode_arg_type]}

detArrayDim :: [[Char]] -> Int -> Int -> Int -> (Int, Int, [Char])
detArrayDim [] current_length longest_line line_index
  | current_length > longest_line = (line_index, current_length, [])
  | otherwise = (line_index, longest_line, [])
detArrayDim (x:xs) current_length longest_line line_index
  | x == "\n" =
    if current_length > 16 then (line_index, current_length, error_string)
    else if current_length > longest_line then detArrayDim xs 0 current_length (line_index + 1)
    else detArrayDim xs 0 longest_line (line_index + 1)
  | otherwise = detArrayDim xs (current_length + 1) longest_line line_index
  where error_string = ("\nA line in a GPLC program can't be longer than 16 tokens (line " ++ show (line_index + 1) ++ ").")

tokenise :: [[Char]] -> Array (Int, Int) Token -> Int -> Int -> Int -> Array (Int, Int) Token
tokenise [] token_arr col i j = token_arr
tokenise (x:xs) token_arr col i j
  | x == "\n" = tokenise xs token_arr 1 (i + 1) 0
  | otherwise = tokenise xs (token_arr // [((i, j), token_added)]) (col + length x + 1) i (j + 1)
  where token_added = Token {column = col + col_correction, content = x, textColour = "white"}
        col_correction = if i == 0 then 0
                         else 1

interpretValueBlock :: Array (Int, Int) Token -> [Symbol_binding] -> [[Char]] -> Int -> Int -> Int -> Int -> ([Symbol_binding], [[Char]])
interpretValueBlock token_arr binding_set error_list i j de_ref_key i_max
  | i > i_max = (binding_set, error_list ++ ["\nNo code block found."])
  | j > 14 || symbol_token == defToken = interpretValueBlock token_arr binding_set error_list (i + 1) 0 de_ref_key i_max
  | content symbol_token == "~" = (binding_set, error_list)
  | isDigit (head (content symbol_token)) = interpretValueBlock token_arr binding_set (error_list ++ [symbol_error]) i (j + 2) de_ref_key i_max
  | head (content value_token) == '-' && not (all isDigit (tail (content value_token))) =
    interpretValueBlock token_arr binding_set (error_list ++ [value_error]) i (j + 2) de_ref_key i_max
  | not (head (content value_token) == '-') && not (all isDigit (content value_token)) =
    interpretValueBlock token_arr binding_set (error_list ++ [value_error]) i (j + 2) de_ref_key i_max
  | otherwise = interpretValueBlock token_arr (binding_set ++ [bound_symbol]) error_list i (j + 2) (de_ref_key + 1) i_max
  where symbol_token = token_arr ! (i, j)
        value_token = token_arr ! (i, j + 1)
        error_location = "\n(" ++ show (i + 1) ++ ", " ++ show (column symbol_token) ++ "): "
        symbol_error = error_location ++ "A symbolic binding can't start with a numeric character."
        value_error = error_location ++ "The initial value bound to a symbol must be an integer."
        bound_symbol = Symbol_binding {symbol = content symbol_token, initialValue = read (content value_token),
                                       readDeRefKey = de_ref_key, writeDeRefKey = 0}

matchOpcode :: [Char] -> Opcode
matchOpcode "if" = Opcode {bytecode = 1, instructionLength = 6, arguments = [Const, RefRead, RefRead, Const, Const]}
matchOpcode "chg_state" = Opcode {bytecode = 2, instructionLength = 10, arguments = [RefRead, RefRead, RefRead, RefRead,
                                                                                     RefRead, RefRead, RefRead, RefRead, RefRead]}
matchOpcode "chg_grid" = Opcode {bytecode = 3, instructionLength = 8, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchOpcode "send_signal" = Opcode {bytecode = 4, instructionLength = 5, arguments = [RefRead, RefRead, RefRead, RefRead]}
matchOpcode "chg_value" = Opcode {bytecode = 5, instructionLength = 7, arguments = [RefWrite, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchOpcode "chg_value_" = Opcode {bytecode = 5, instructionLength = 7, arguments = [Const, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchOpcode "chg_floor" = Opcode {bytecode = 6, instructionLength = 7, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchOpcode "chg_ps1" = Opcode {bytecode = 7, instructionLength = 4, arguments = [RefRead, RefRead, RefRead]}
matchOpcode "chg_obj_type" = Opcode {bytecode = 8, instructionLength = 5, arguments = [RefRead, RefRead, RefRead, RefRead]}
matchOpcode "place_light" = Opcode {bytecode = 9, instructionLength = 7, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchOpcode "chg_grid_" = Opcode {bytecode = 10, instructionLength = 8, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchOpcode "copy_ps1" = Opcode {bytecode = 11, instructionLength = 5, arguments = [RefWrite, RefRead, RefRead, RefRead]}
matchOpcode "copy_lstate" = Opcode {bytecode = 12, instructionLength = 8, arguments = [RefWrite, RefRead, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchOpcode "chg_ps0" = Opcode {bytecode = 14, instructionLength = 4, arguments = [RefRead, RefRead, RefRead]}
matchOpcode "copy_ps0" = Opcode {bytecode = 15, instructionLength = 5, arguments = [RefWrite, RefRead, RefRead, RefRead]}
matchOpcode "binary_dice" = Opcode {bytecode = 16, instructionLength = 7, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefWrite]}
matchOpcode "project_init" = Opcode {bytecode = 17, instructionLength = 14, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefRead,
                                                                                         RefRead, RefRead, RefRead, RefRead, RefRead, Const, RefRead]}
matchOpcode "project_update" = Opcode {bytecode = 18, instructionLength = 6, arguments = [RefRead, RefWrite, RefRead, RefRead, RefRead]}
matchOpcode "init_npc" = Opcode {bytecode = 19, instructionLength = 3, arguments = [RefRead, RefRead]}
matchOpcode "npc_decision" = Opcode {bytecode = 20, instructionLength = 2, arguments = [RefWrite]}
matchOpcode "npc_move" = Opcode {bytecode = 21, instructionLength = 2, arguments = [RefWrite]}
matchOpcode "npc_damage" = Opcode {bytecode = 22, instructionLength = 2, arguments = [Const]}
matchOpcode "cpede_move" = Opcode {bytecode = 23, instructionLength = 3, arguments = [RefWrite, Const]}

main = do
  args <- getArgs
  contents <- bracket (openFile (args !! 0) ReadMode) hClose
                      (\h -> do c <- hGetContents h; putStr ("\nprogram file size: " ++ show (length c)); return c)
  showResult contents

showResult :: [Char] -> IO ()
showResult contents =
  let split_contents = splitOn " " contents
      array_dim = detArrayDim split_contents 0 0 0
      empty_token_array = array ((0, 0), (fst__ array_dim, (snd__ array_dim) - 1))
                                [((i, j), defToken) | i <- [0..fst__ array_dim], j <- [0..(snd__ array_dim) - 1]]
      token_arr = tokenise split_contents empty_token_array 1 0 0
      bound_symbols = interpretValueBlock token_arr [] [] 0 0 0 (fst__ array_dim)
  in do
  if length (third_ array_dim) > 0 then error (third_ array_dim)
  else if snd bound_symbols /= [] then putStr ("\nValue block errors: " ++ show (snd bound_symbols))
  else showSymbolBindings (fst bound_symbols)

showSymbolBindings :: [Symbol_binding] -> IO ()
showSymbolBindings [] = return ()
showSymbolBindings (x:xs) = do
  putStr ("\n" ++ show x)
  showSymbolBindings xs

showTokenArray :: Array (Int, Int) Token -> Int -> Int -> Int -> Int -> IO ()
showTokenArray token_arr i j i_max j_max =
  let token = token_arr ! (i, j)
  in do
  if i > i_max then return ()
  else if j > j_max then showTokenArray token_arr (i + 1) 0 i_max j_max
  else if token_arr ! (i, j) == defToken then showTokenArray token_arr (i + 1) 0 i_max j_max
  else do
    setCursorPosition i (column token - 1)
    putStr (content token)
    showTokenArray token_arr i (j + 1) i_max j_max

