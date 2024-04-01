-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- An implementation of the GPLC compiler, which transforms GPLC source code to the GPLC bytecode that drives augmented 
-- game logic within the engine.

module CompileGPLC where

import System.IO
import System.Environment
import Control.Exception
import Data.Array.IArray
import Data.Char
import Data.Maybe
import qualified Data.Sequence as SEQ
import BuildModel

newtype BlockNumber = BlockNumber Int deriving Eq

data Token = Token {line :: Int, column :: Int, content :: [Char], textColour :: [Char], blockNumber :: BlockNumber} deriving (Eq, Show)

defToken = Token {line = 0, column = 0, content = [], textColour = [], blockNumber = BlockNumber 0}

data BlockRegister = BlockRegister {zone :: Int, c1 :: Int, c2 :: Int, c3 :: Int, pass :: Int} deriving Show

instance Show BlockNumber where
  show (BlockNumber a) = show a

-- The readRefKey and writeRefKey bind a symbolic reference to a value in GPLC source code to its in memory representation 
-- at engine runtime.  They thereby allow for reference based argument passing to the engine functions that implement 
-- GPLC opcodes, for reading and writing respectively.
data Symbol_binding = Symbol_binding {symbol :: [Char], initialValue :: Int, readRefKey :: Int, writeRefKey :: Int} deriving Show

data Instruction_arg_type = RefRead | RefWrite | Const deriving Eq

data Instruction = Instruction {opcode :: Int, instructionLength :: Int, arguments :: [Instruction_arg_type]}

-- These functions determine the if block structure of a GPLC program.  In GPLC source code the block sizes are manually 
-- given as the last two arguments of if statements and there is no use of whitespace or blocks within braces.  The meta 
-- data added to the token array through these functions allows the client to present an annotated version of the source 
-- code with visible blocks.
nullBlockCombiner :: BlockNumber -> BlockNumber -> BlockNumber
nullBlockCombiner a b = a

blockCombiner :: BlockNumber -> BlockNumber -> BlockNumber
blockCombiner (BlockNumber a) (BlockNumber b)
  | b == 1 && (a == 0 || a == -1) = BlockNumber 1
  | b == 2 && (a == 0 || a == -1) = BlockNumber 2
  | b == 1 && a == 1 = BlockNumber 3
  | b == 1 && a == 2 = BlockNumber 4
  | b == 2 && a == 1 = BlockNumber 5
  | b == 2 && a == 2 = BlockNumber 6
  | otherwise = BlockNumber 0

traverseSymbols :: Int -> [Char] -> BlockRegister -> BlockRegister
traverseSymbols index symbol state
  | zone state == 0 && pass state == 1 && symbol == "if" =
    BlockRegister {zone = -1, c1 = index + 4, c2 = index + 5, c3 = index + 6, pass = pass state}
  | zone state == 0 && pass state == 2 && symbol == "if_" =
    BlockRegister {zone = -1, c1 = index + 4, c2 = index + 5, c3 = index + 6, pass = pass state}
  | zone state == -1 && index == c1 state =
    BlockRegister {zone = -1, c1 = read symbol, c2 = c2 state, c3 = c3 state, pass = pass state}
  | zone state == -1 && index == c2 state =
    BlockRegister {zone = -1, c1 = c1 state, c2 = read symbol, c3 = c3 state, pass = pass state}
  | zone state == -1 && index == c3 state =
    BlockRegister {zone = 1, c1 = c1 state - 1, c2 = c2 state, c3 = 0, pass = pass state}
  | zone state == -1 =
    BlockRegister {zone = -1, c1 = c1 state, c2 = c2 state, c3 = c3 state, pass = pass state}
  | zone state == 1 && c1 state > 0 =
    BlockRegister {zone = 1, c1 = c1 state - 1, c2 = c2 state, c3 = 0, pass = pass state}
  | zone state == 1 && c1 state == 0 =
    BlockRegister {zone = 2, c1 = 0, c2 = c2 state - 2, c3 = 0, pass = pass state}
  | zone state == 2 && c2 state > 0 =
    BlockRegister {zone = 2, c1 = 0, c2 = c2 state - 1, c3 = 0, pass = pass state}
  | otherwise =
    BlockRegister {zone = 0, c1 = 0, c2 = 0, c3 = 0, pass = pass state}

traverseIfBlocks :: [[Char]] -> Array (Int, Int) BlockNumber -> Array (Int, Int) BlockNumber -> (BlockNumber -> BlockNumber -> BlockNumber) -> BlockRegister
                 -> Int -> Int -> Int -> Array (Int, Int) BlockNumber
traverseIfBlocks [] block_arr_a block_arr_b block_combiner state i j index = block_arr_a
traverseIfBlocks (x:xs) block_arr_a block_arr_b block_combiner state i j index
  | x == "\n" = traverseIfBlocks xs block_arr_a block_arr_b block_combiner state (i + 1) 0 index
  | otherwise = traverseIfBlocks xs (block_arr_a // [((i, j), combined_blocks)]) block_arr_b block_combiner next_state i (j + 1) (index + 1)
  where next_state = traverseSymbols index x state
        combined_blocks = block_combiner (BlockNumber (zone next_state)) (block_arr_b ! (i, j))

labelIfBlocks :: [[Char]] -> Int -> Int -> Array (Int, Int) BlockNumber
labelIfBlocks split_source i_max j_max =
  let empty_block_arr_a = array ((0, 0), (i_max, j_max)) [((i, j), BlockNumber 0) | i <- [0..i_max], j <- [0..j_max]]
      empty_block_arr_b = array ((0, 0), (i_max, j_max)) [((i, j), BlockNumber 0) | i <- [0..i_max], j <- [0..j_max]]
      init_state = BlockRegister {zone = 3, c1 = 0, c2 = 0, c3 = 0, pass = 0}
      fst_pass = traverseIfBlocks split_source empty_block_arr_a empty_block_arr_b nullBlockCombiner (init_state {pass = 1})
                                  0 0 0
      snd_pass = traverseIfBlocks split_source empty_block_arr_a fst_pass blockCombiner (init_state {pass = 2})
                                  0 0 0
  in snd_pass

-- The GPLC source code is parsed into lexical tokens and placed in an array, ready to be passed to the interpreter functions.
-- These two functions determine an appropriate size for that array and perform the parsing, respectively.
detArrayDim :: [[Char]] -> Int -> Int -> Int -> (Int, Int, [[Char]])
detArrayDim [] current_length longest_line line_index
  | current_length > longest_line = (line_index, 16, [])
  | otherwise = (line_index, 16, [])
detArrayDim (x:xs) current_length longest_line line_index
  | x == "\n" =
    if current_length > 16 then (line_index, current_length, [error_string])
    else if current_length > longest_line then detArrayDim xs 0 current_length (line_index + 1)
    else detArrayDim xs 0 longest_line (line_index + 1)
  | otherwise = detArrayDim xs (current_length + 1) longest_line line_index
  where error_string = ("A line in a GPLC program can't be longer than 16 tokens (line " ++ show (line_index + 1) ++ ").")

tokenise :: [[Char]] -> Array (Int, Int) Token -> Array (Int, Int) BlockNumber -> Int -> Int -> Int -> Array (Int, Int) Token
tokenise [] token_arr block_arr col i j = token_arr
tokenise (x:xs) token_arr block_arr col i j
  | x == "\n" = tokenise xs token_arr block_arr 1 (i + 1) 0
  | otherwise = tokenise xs (token_arr // [((i, j), token_added)]) block_arr (col + length x + 1) i (j + 1)
  where token_added = Token {line = 0, column = col + col_correction, content = x, textColour = "Black", blockNumber = block_arr ! (i, j)}
        col_correction = if i == 0 then 0
                         else 1

-- This function generates the symbolic binding for each value in the scope of a particular program, except for the writeRefKey.
genSymbolBindings :: Array (Int, Int) Token -> [Symbol_binding] -> [[Char]] -> Int -> Int -> Int -> Int -> ([Symbol_binding], Int, [[Char]])
genSymbolBindings token_arr binding_set error_list i j ref_key i_max
  | i > i_max = (binding_set, i + 1, error_list ++ ["No code block found."])
  | j > j_max || symbol_token == defToken = genSymbolBindings token_arr binding_set error_list (i + 1) 0 ref_key i_max
  | content symbol_token == "~" = (binding_set, i + 1, error_list)
  | isDigit (head (content symbol_token)) = genSymbolBindings token_arr binding_set (error_list ++ [symbol_error]) i (j + 3) ref_key i_max
  | head (content value_token) == '-' && not (all isDigit (tail (content value_token))) =
    genSymbolBindings token_arr binding_set (error_list ++ [value_error]) i (j + 3) ref_key i_max
  | not (head (content value_token) == '-') && not (all isDigit (content value_token)) =
    genSymbolBindings token_arr binding_set (error_list ++ [value_error]) i (j + 3) ref_key i_max
  | content (token_arr ! (i, j + 1)) /= "=" = genSymbolBindings token_arr binding_set (error_list ++ [separator_error]) i (j + 3) ref_key i_max
  | otherwise = genSymbolBindings token_arr (binding_set ++ [bound_symbol]) error_list i (j + 3) (ref_key + 1) i_max
  where symbol_token = token_arr ! (i, j)
        value_token = token_arr ! (i, j + 2)
        error_location = "(" ++ show (i + 1) ++ ", " ++ show (column symbol_token) ++ "): "
        symbol_error = error_location ++ "A symbolic binding can't start with a numeric character."
        value_error = error_location ++ "The initial value bound to a symbol must be an integer."
        separator_error = error_location ++ "Symbol bindings must be of the form \"symbol = value\""
        bound_symbol = Symbol_binding {symbol = content symbol_token, initialValue = read (content value_token),
                                       readRefKey = ref_key, writeRefKey = 0}
        j_max = snd (snd (bounds token_arr))

-- This function is used to recognise each GPLC keyword at source code level.  Except for opcode 5 there is a one to one mapping between 
-- keywords at source code level and opcodes at bytecode level.  See the GPLC Specification for more details.
matchKeyword :: [Char] -> Maybe Instruction
matchKeyword "if" = Just Instruction {opcode = 1, instructionLength = 6, arguments = [Const, RefRead, RefRead, Const, Const]}
matchKeyword "if_" = Just Instruction {opcode = 1, instructionLength = 6, arguments = [Const, RefRead, RefRead, Const, Const]}
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
matchKeyword "copy_lstate" = Just Instruction {opcode = 12, instructionLength = 8, arguments = [RefWrite, RefRead, RefRead,
                                                                                                RefRead, RefRead, RefRead, RefRead]}
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
matchKeyword "do_nothing" = Just Instruction {opcode = 24, instructionLength = 0, arguments = [Const]}
matchKeyword _ = Nothing

-- These two functions generate the signal block part of the bytecode output.
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
  | i > i_max = ((SEQ.fromList [0, 0] SEQ.>< signal_block) SEQ.|> size SEQ.|> 536870911 SEQ.|> 536870912, error_list)
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
        j_size = snd (snd (bounds token_arr)) + 1
        rows_to_jump = rowsToJump (first_arg_int + 1) j_size
        matched_keyword = matchKeyword (content keyword)
        first_arg_error = "(" ++ show (i + 1) ++ ", " ++ show (column first_arg) ++ "): \"--signal\" and \"pass_msg\" must be followed by an integer."
        keyword_error = "(" ++ show (i + 1) ++ ", " ++ show (column keyword) ++ "): " ++ content keyword ++ " is not a valid keyword."

-- This function determines the combined size that the signal and code blocks will have in the bytecode output.
sizeSignalCodeBlock :: SEQ.Seq Int -> Int -> Int -> Int -> Int
sizeSignalCodeBlock signal_block i i_max total_size =
  if i > i_max then total_size
  else sizeSignalCodeBlock signal_block (i + 3) i_max (total_size + SEQ.index signal_block i + 3)

-- Once the combined size of the signal and code blocks is known, the writeRefKey can be determined for each symbolic binding.
addWriteRefKey :: Int -> Symbol_binding -> Symbol_binding
addWriteRefKey offset binding = binding {writeRefKey = readRefKey binding + offset}

-- This function processes the ...message... argument of the pass_msg keyword.
readMsg :: Array (Int, Int) Token -> Int -> Int -> Int -> Int -> Int -> SEQ.Seq Int -> Maybe (SEQ.Seq Int)
readMsg token_arr i j j_max c msg_length result
  | c == msg_length = Just result
  | j > j_max = readMsg token_arr (i + 1) 0 j_max c msg_length result
  | head msg_unit == '-' && not (all isDigit (tail msg_unit)) = Nothing
  | head msg_unit /= '-' && not (all isDigit msg_unit) = Nothing
  | otherwise = readMsg token_arr i (j + 1) j_max (c + 1) msg_length (result SEQ.|> read msg_unit)
  where msg_unit = content (token_arr ! (i, j))

-- These two functions transform the read reference, write reference and literal constant type arguments of each keyword
-- to bytecode output.
matchSymbol :: [Symbol_binding] -> [Char] -> Instruction_arg_type -> Maybe Int
matchSymbol [] token_content arg_type = Nothing
matchSymbol (x:xs) token_content arg_type
  | symbol x == token_content =
    if arg_type == RefRead then Just (readRefKey x)
    else Just (writeRefKey x)
  | otherwise = matchSymbol xs token_content arg_type

interpretArgs :: [Instruction_arg_type] -> Array (Int, Int) Token -> [Symbol_binding] -> Int -> Int -> SEQ.Seq Int -> [[Char]] -> (SEQ.Seq Int, [[Char]])
interpretArgs [] token_arr bound_symbols i j result error_list = (result, error_list)
interpretArgs (x:xs) token_arr bound_symbols i j result error_list
  | x == Const =
    if head (content arg_token) == '-' && not (all isDigit (tail (content arg_token))) then
      interpretArgs xs token_arr bound_symbols i (j + 1) result (error_list ++ [const_error])
    else if not (head (content arg_token) == '-') && not (all isDigit (content arg_token)) then
      interpretArgs xs token_arr bound_symbols i (j + 1) result (error_list ++ [const_error])
    else interpretArgs xs token_arr bound_symbols i (j + 1) (result SEQ.|> read (content arg_token)) error_list
  | isNothing matched_symbol = interpretArgs xs token_arr bound_symbols i (j + 1) result (error_list ++ [symbol_error])
  | otherwise = interpretArgs xs token_arr bound_symbols i (j + 1) (result SEQ.|> fromJust matched_symbol) error_list
  where arg_token = token_arr ! (i, j)
        matched_symbol = matchSymbol bound_symbols (content arg_token) x
        error_location = "(" ++ show (i + 1) ++ ", " ++ show (column arg_token) ++ "): "
        const_error = error_location ++ content arg_token ++ " is not a literal integer."
        symbol_error = error_location ++ content arg_token ++ " is not a bound symbol in the scope of this program."

-- This function transforms each instruction line in the source code (keyword plus argument set) into bytecode output.
genCodeBlock :: Array (Int, Int) Token -> [Symbol_binding] -> Int -> Int -> SEQ.Seq Int -> [[Char]] -> (SEQ.Seq Int, [[Char]])
genCodeBlock token_arr bound_symbols i i_max code_block error_list
  | i > i_max = (code_block SEQ.|> 536870911, error_list)
  | content keyword == "--signal" = genCodeBlock token_arr bound_symbols (i + 1) i_max code_block error_list
  | content keyword == "pass_msg" && isNothing read_msg =
    genCodeBlock token_arr bound_symbols (i + rows_to_jump) i_max code_block (error_list ++ ["Message read error"])
  | content keyword == "pass_msg" =
    genCodeBlock token_arr bound_symbols (i + rows_to_jump) i_max
                 ((code_block SEQ.|> 13) SEQ.>< fst interpreted_args_ SEQ.>< (fromJust read_msg SEQ.|> line_term))
                 (error_list ++ snd interpreted_args_)
  | otherwise =
    if opcode matched_keyword == 24 then
      genCodeBlock token_arr bound_symbols (i + 1) i_max code_block (error_list ++ snd interpreted_args)
    else
      genCodeBlock token_arr bound_symbols (i + 1) i_max ((code_block SEQ.|> opcode matched_keyword) SEQ.>< (fst interpreted_args SEQ.|> line_term))
                   (error_list ++ snd interpreted_args)
  where keyword = token_arr ! (i, 0)
        matched_keyword = fromJust (matchKeyword (content keyword))
        l_arg = read (content (token_arr ! (i, 1)))
        j_size = snd (snd (bounds token_arr)) + 1
        rows_to_jump = rowsToJump (l_arg + 1) j_size
        read_msg = readMsg token_arr i 3 (j_size - 1) 0 (l_arg - 2) SEQ.empty
        interpreted_args = interpretArgs (arguments matched_keyword) token_arr bound_symbols i 1 SEQ.empty []
        interpreted_args_ = interpretArgs [RefRead] token_arr bound_symbols i 2 SEQ.empty []
        line_term = 536870912

-- The data block part of the bytecode output is the sequence of initial values assigned to bound symbols in the
-- source code, in the order the symbol bindings appear in the source code.
genDataBlock :: [Symbol_binding] -> SEQ.Seq Int -> SEQ.Seq Int
genDataBlock [] data_block = data_block SEQ.|> 536870912
genDataBlock (x:xs) data_block = genDataBlock xs (data_block SEQ.|> initialValue x)

-- These two functions annotate the source code with text colours.
mapColour :: Instruction_arg_type -> [Char]
mapColour RefRead = "Green"
mapColour RefWrite = "Red"
mapColour Const = "Black"

addColour :: Array (Int, Int) Token -> Int -> Int -> [((Int, Int), Token)] -> [((Int, Int), Token)]
addColour token_arr i i_max token_arr_upd
  | i > i_max = token_arr_upd
  | isNothing matched_keyword = addColour token_arr (i + 1) i_max token_arr_upd
  | opcode (fromJust matched_keyword) == 24 = addColour token_arr (i + 1) i_max token_arr_upd
  | otherwise = addColour token_arr (i + 1) i_max (update ++ token_arr_upd)
  where matched_keyword = matchKeyword (content (token_arr ! (i, 0)))
        target = \j -> token_arr ! (i, j)
        colour_scheme = map mapColour (arguments (fromJust matched_keyword))
        update = ((i, 0), (target 0) {textColour = "Blue"})
                 : [((i, j), (target j) {textColour = colour_scheme !! (j - 1)}) | j <- [1..instructionLength (fromJust matched_keyword) - 1]]

