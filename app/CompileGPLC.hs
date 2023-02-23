-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- The new implementation of the GPLC compiler, which is to be moved to /src and integrated into the 
-- server when ready.

module Main where

import System.IO
import System.Environment
import Control.Exception
import Data.List.Split
import Data.Array.IArray
import Data.Char
import Data.Maybe
import qualified Data.Sequence as SEQ

fst__ (a, b, c) = a
snd__ (a, b, c) = b
third_ (a, b, c) = c

data Token = Token {column :: Int, content :: [Char], textColour :: [Char]} deriving (Eq, Show)

defToken = Token {column = 0, content = [], textColour = []}

-- The readRefKey and writeRefKey bind a symbolic reference to a value in GPLC source code to its in memory representation 
-- at engine runtime.  They thereby allow for reference based argument passing to the engine functions that implement 
-- GPLC opcodes, for reading and writing respectively.
data Symbol_binding = Symbol_binding {symbol :: [Char], initialValue :: Int, readRefKey :: Int, writeRefKey :: Int} deriving Show

data Instruction_arg_type = RefRead | RefWrite | Const

data Instruction = Instruction {opcode :: Int, instructionLength :: Int, arguments :: [Instruction_arg_type]}

-- The GPLC source code is parsed into lexical tokens and placed in an array, ready to be passed to the interpreter functions.
-- These two functions determine an appropriate size for that array and perform the parsing, respectively.
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

-- This function determines the symbolic binding for each value in the scope of a particular program, except for the writeRefKey.
interpretValueBlock :: Array (Int, Int) Token -> [Symbol_binding] -> [[Char]] -> Int -> Int -> Int -> Int -> ([Symbol_binding], Int, [[Char]])
interpretValueBlock token_arr binding_set error_list i j ref_key i_max
  | i > i_max = (binding_set, i + 1, error_list ++ ["\nNo code block found."])
  | j > 14 || symbol_token == defToken = interpretValueBlock token_arr binding_set error_list (i + 1) 0 ref_key i_max
  | content symbol_token == "~" = (binding_set, i + 1, error_list)
  | isDigit (head (content symbol_token)) = interpretValueBlock token_arr binding_set (error_list ++ [symbol_error]) i (j + 2) ref_key i_max
  | head (content value_token) == '-' && not (all isDigit (tail (content value_token))) =
    interpretValueBlock token_arr binding_set (error_list ++ [value_error]) i (j + 2) ref_key i_max
  | not (head (content value_token) == '-') && not (all isDigit (content value_token)) =
    interpretValueBlock token_arr binding_set (error_list ++ [value_error]) i (j + 2) ref_key i_max
  | otherwise = interpretValueBlock token_arr (binding_set ++ [bound_symbol]) error_list i (j + 2) (ref_key + 1) i_max
  where symbol_token = token_arr ! (i, j)
        value_token = token_arr ! (i, j + 1)
        error_location = "\n(" ++ show (i + 1) ++ ", " ++ show (column symbol_token) ++ "): "
        symbol_error = error_location ++ "A symbolic binding can't start with a numeric character."
        value_error = error_location ++ "The initial value bound to a symbol must be an integer."
        bound_symbol = Symbol_binding {symbol = content symbol_token, initialValue = read (content value_token),
                                       readRefKey = ref_key, writeRefKey = 0}

-- This function is used to recognise each GPLC keyword.  Except for opcode 5 there is a one to one mapping between 
-- keywords at source code level and opcodes at bytecode level.  See the GPLC Specification for more details.
matchKeyword :: [Char] -> Maybe Instruction
matchKeyword "if" = Just Instruction {opcode = 1, instructionLength = 6, arguments = [Const, RefRead, RefRead, Const, Const]}
matchKeyword "chg_state" = Just Instruction {opcode = 2, instructionLength = 10, arguments = [RefRead, RefRead, RefRead, RefRead,
                                                                                         RefRead, RefRead, RefRead, RefRead, RefRead]}
matchKeyword "chg_grid" = Just Instruction {opcode = 3, instructionLength = 8, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchKeyword "send_signal" = Just Instruction {opcode = 4, instructionLength = 5, arguments = [RefRead, RefRead, RefRead, RefRead]}
matchKeyword "chg_value" = Just Instruction {opcode = 5, instructionLength = 7, arguments = [RefWrite, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchKeyword "chg_value_" = Just Instruction {opcode = 5, instructionLength = 7, arguments = [Const, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchKeyword "chg_floor" = Just Instruction {opcode = 6, instructionLength = 7, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchKeyword "chg_ps1" = Just Instruction {opcode = 7, instructionLength = 4, arguments = [RefRead, RefRead, RefRead]}
matchKeyword "chg_obj_type" = Just Instruction {opcode = 8, instructionLength = 5, arguments = [RefRead, RefRead, RefRead, RefRead]}
matchKeyword "place_light" = Just Instruction {opcode = 9, instructionLength = 7, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchKeyword "chg_grid_" = Just Instruction {opcode = 10, instructionLength = 8, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchKeyword "copy_ps1" = Just Instruction {opcode = 11, instructionLength = 5, arguments = [RefWrite, RefRead, RefRead, RefRead]}
matchKeyword "copy_lstate" = Just Instruction {opcode = 12, instructionLength = 8, arguments = [RefWrite, RefRead, RefRead, RefRead, RefRead, RefRead, RefRead]}
matchKeyword "pass_msg" = Just Instruction {opcode = 13, instructionLength = 2, arguments = [RefRead]}
matchKeyword "chg_ps0" = Just Instruction {opcode = 14, instructionLength = 4, arguments = [RefRead, RefRead, RefRead]}
matchKeyword "copy_ps0" = Just Instruction {opcode = 15, instructionLength = 5, arguments = [RefWrite, RefRead, RefRead, RefRead]}
matchKeyword "binary_dice" = Just Instruction {opcode = 16, instructionLength = 7, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefWrite]}
matchKeyword "project_init" = Just Instruction {opcode = 17, instructionLength = 14, arguments = [RefRead, RefRead, RefRead, RefRead, RefRead, RefRead,
                                                                                             RefRead, RefRead, RefRead, RefRead, RefRead, Const, RefRead]}
matchKeyword "project_update" = Just Instruction {opcode = 18, instructionLength = 6, arguments = [RefRead, RefWrite, RefRead, RefRead, RefRead]}
matchKeyword "init_npc" = Just Instruction {opcode = 19, instructionLength = 3, arguments = [RefRead, RefRead]}
matchKeyword "npc_decision" = Just Instruction {opcode = 20, instructionLength = 2, arguments = [RefWrite]}
matchKeyword "npc_move" = Just Instruction {opcode = 21, instructionLength = 2, arguments = [RefWrite]}
matchKeyword "npc_damage" = Just Instruction {opcode = 22, instructionLength = 2, arguments = [Const]}
matchKeyword "cpede_move" = Just Instruction {opcode = 23, instructionLength = 3, arguments = [RefWrite, Const]}
matchKeyword _ = Nothing

-- These three functions are connected with interpretation of the code block in GPLC source code (see GPLC Specification).
rowsToJump :: Int -> Int -> Int
rowsToJump instruction_length j_size =
  let full_rows = div instruction_length j_size
      part_row = if mod instruction_length j_size > 0 then 1
                 else 0
  in
  if full_rows == 0 || (full_rows == 1 && part_row == 0) then 1
  else full_rows + part_row

genSignalBlock :: Array (Int, Int) Token -> Int -> Int -> Int -> Int -> SEQ.Seq Int -> [[Char]] -> (SEQ.Seq Int, [[Char]])
genSignalBlock token_arr i i_max size offset signal_block error_list
  | i > i_max = (signal_block SEQ.|> size, error_list)
  | content keyword == "pass_msg" && not (all isDigit (content first_arg)) = (signal_block, error_list ++ [first_arg_error])
  | content keyword == "pass_msg" = genSignalBlock token_arr (i + rows_to_jump) i_max (size + first_arg_int) offset signal_block error_list
  | content keyword == "--signal" && not (all isDigit (content first_arg)) = (signal_block, error_list ++ [first_arg_error])
  | content keyword == "--signal" =
    if SEQ.length signal_block == 0 then
      genSignalBlock token_arr (i + 1) i_max 0 (offset + size) (signal_block SEQ.|> first_arg_int SEQ.|> 0) error_list
    else genSignalBlock token_arr (i + 1) i_max 0 (offset + size) (signal_block SEQ.|> size SEQ.|> first_arg_int SEQ.|> (offset + size)) error_list
  | isNothing matched_keyword = genSignalBlock token_arr (i + 1) i_max size offset signal_block (error_list ++ [keyword_error])
  | otherwise = genSignalBlock token_arr (i + 1) i_max (size + instructionLength (fromJust matched_keyword)) offset signal_block error_list
  where keyword = token_arr ! (i, 0)
        first_arg = token_arr ! (i, 1)
        first_arg_int = read (content first_arg)
        j_size = snd (snd (bounds token_arr))
        rows_to_jump = rowsToJump first_arg_int j_size
        matched_keyword = matchKeyword (content keyword)
        first_arg_error = "\n(" ++ show (i + 1) ++ ", " ++ show (column first_arg) ++ "): \"--signal\" and \"pass_msg\" must be followed by an integer."
        keyword_error = "\n(" ++ show (i + 1) ++ ", " ++ show (column keyword) ++ "): " ++ content keyword ++ " is not a valid keyword."

sizeCodeBlock :: SEQ.Seq Int -> Int -> Int -> Int -> Int
sizeCodeBlock signal_block i i_max total_size =
  if i > i_max then total_size
  else sizeCodeBlock signal_block (i + 3) i_max (total_size + SEQ.index signal_block i)

addWriteRefKey :: Int -> Symbol_binding -> Symbol_binding
addWriteRefKey offset binding = binding {writeRefKey = readRefKey binding + offset}

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
      signal_block = genSignalBlock token_arr (snd__ bound_symbols) (fst__ array_dim) 0 0 SEQ.empty []
      code_block_size = sizeCodeBlock (fst signal_block) 2 (SEQ.length (fst signal_block) - 1) 0
      add_write_ref_key = addWriteRefKey (code_block_size + 3)
  in do
  if third_ array_dim /= [] then error (third_ array_dim)
  else if third_ bound_symbols /= [] then error (show (third_ bound_symbols))
  else if snd signal_block /= [] then error (show (snd signal_block))
  else showSymbolBindings (map add_write_ref_key (fst__ bound_symbols))

showSymbolBindings :: [Symbol_binding] -> IO ()
showSymbolBindings [] = return ()
showSymbolBindings (x:xs) = do
  putStr ("\n" ++ show x)
  showSymbolBindings xs

