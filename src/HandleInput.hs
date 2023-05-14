-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This module is part of the map development server and interprets commands received by the server.

{-# LANGUAGE FlexibleInstances #-}

module HandleInput where

import Data.List.Split
import Data.Array.IArray
import Data.Maybe
import Data.List
import qualified Data.Sequence as SEQ
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Crypto.Hash.SHA256 as SHA256
import BuildModel hiding (Game_state, w_grid_, f_grid_, obj_grid_)
import CompileGPLC
import qualified IndexWrapper1 as IW

data GPLC_program = GPLC_program {name :: [Char], hash :: [Char], source :: [Char], bytecode :: [Char]}

empty_gplc_program = GPLC_program {name = [], hash = [], source = [], bytecode = []}

data Server_state = Server_state {w_grid_ :: Array (Int, Int, Int) Wall_grid, f_grid_ :: Array (Int, Int, Int) Floor_grid,
                                  obj_grid_ :: Array (Int, Int, Int) Obj_grid, gplcPrograms :: Array Int GPLC_program}

-- This recursive data type is used to implement the control structure that updates the map state in response to commands received by the server.
data Comm_struct = Comm_struct {dictionaryPage :: [[Char]], branches :: Maybe (Array Int Comm_struct),
                                readWriteGameState :: Maybe (Server_state -> [[Char]] -> (Maybe Server_state, [Char]))}

-- These are the functions that update the map state in response to the server receiving commands.
parseTerrain :: [Char] -> Terrain
parseTerrain t_name
  | t_name == "Positive_u" = BuildModel.Positive_u
  | t_name == "Negative_u" = BuildModel.Negative_u
  | t_name == "Positive_v" = BuildModel.Positive_v
  | t_name == "Negative_v" = BuildModel.Negative_v
  | t_name == "Flat" = BuildModel.Flat
  | t_name == "Open" = BuildModel.Open

writeFloorGrid :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
writeFloorGrid server_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      height = read (args !! 3)
      terrain = parseTerrain (args !! 4)
      f' = Floor_grid {w_ = height, surface = terrain, local_up_ramp = (0, 0), local_down_ramp = (0, 0)}
      success_str = "writeFloorGrid succeeded.  Arguments passed were w: "
      boundsCheck = IW.boundsCheck (f_grid_ server_state) (w, u, v) "writeFloorGrid"
  in
  if isNothing boundsCheck then 
    (Just server_state {f_grid_ = (f_grid_ server_state) // [((w, u, v), f')]},
     success_str ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " height: " ++ (args !! 3) ++ " terrain: " ++ (args !! 4))
  else (Nothing, fromJust boundsCheck)

constructProgBlock :: [[Char]] -> [Int]
constructProgBlock [] = []
constructProgBlock (x:xs) = (read x) : constructProgBlock xs

writeObjGrid :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
writeObjGrid server_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      obj_type = read (args !! 3)
      boundsCheck = IW.boundsCheck (obj_grid_ server_state) (w, u, v) "writeObjGrid"
  in
  if isNothing boundsCheck then
    (Just server_state {obj_grid_ = (obj_grid_ server_state) // [((w, u, v), Obj_grid {objType = obj_type,
                                                                                   program = constructProgBlock (drop 4 args),
                                                                                   programName = []})]},
     "writeObjGrid succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " obj_type: " ++ (args !! 3))
  else (Nothing, fromJust boundsCheck)

writeWallGridStructure :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
writeWallGridStructure server_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = \i -> intToBool (read (args !! i))
      upd_ = [read (args !! 7), read (args !! 8), read (args !! 9), read (args !! 10)]
      w' = ((w_grid_ server_state) ! (w, u, v)) {u1 = upd 3, u2 = upd 4, v1 = upd 5, v2 = upd 6, wall_flag = upd_}
      success_str = "writeWallGridStructure succeeded.  Arguments passed were w: "
      boundsCheck = IW.boundsCheck (w_grid_ server_state) (w, u, v) "writeWallGridStructure"
  in
  if isNothing boundsCheck then
    (Just server_state {w_grid_ = (w_grid_ server_state) // [((w, u, v), w')]},
     success_str ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))
  else (Nothing, fromJust boundsCheck)

writeWallGridTextures :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
writeWallGridTextures server_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = [read (args !! 3), read (args !! 4), read (args !! 5), read (args !! 6)]
      success_str = "writeWallGridTextures succeeded.  Arguments passed were w: "
      boundsCheck = IW.boundsCheck (w_grid_ server_state) (w, u, v) "writeWallGridTextures"
  in
  if isNothing boundsCheck then
    (Just server_state {w_grid_ = (w_grid_ server_state) // [((w, u, v), ((w_grid_ server_state) ! (w, u, v)) {texture = upd})]},
     success_str ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))
  else (Nothing, fromJust boundsCheck)

writeObjPlace :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
writeObjPlace server_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = \i -> read (args !! i)
      upd_ = \i -> read (args !! i)
      w_grid__ = w_grid_ server_state
      obj' = def_obj_place {ident_ = upd 3, u__ = upd_ 4, v__ = upd_ 5, w__ = upd_ 6, texture__ = upd 7, num_elem = read (args !! 8), obj_flag = upd 9}
      success_str = "writeObjPlace succeeded.  Arguments passed were w: "
      boundsCheck = IW.boundsCheck (w_grid_ server_state) (w, u, v) "writeObjPlace"
  in
  if isNothing boundsCheck then
    (Just server_state {w_grid_ = w_grid__ // [((w, u, v), (w_grid__ ! (w, u, v)) {obj = Just obj'})]},
     success_str ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))
  else (Nothing, fromJust boundsCheck)

-- This class is used by the read commands for Wall_grid, Floor_grid and Obj_grid to serialise these types to JSON for
-- sending to the client.
class Serialise a where
  toJSON :: Maybe a -> [Char]

instance Serialise Obj_place where
  toJSON Nothing = "null"
  toJSON (Just a) =
    "{\n"
    ++ "  \"ident_\": " ++ show (ident_ a) ++ ",\n"
    ++ "  \"u__\": " ++ show (u__ a) ++ ",\n"
    ++ "  \"v__\": " ++ show (v__ a) ++ ",\n"
    ++ "  \"w__\": " ++ show (w__ a) ++ ",\n"
    ++ "  \"texture__\": " ++ show (texture__ a) ++ ",\n"
    ++ "  \"num_elem\": " ++ show (num_elem a)
    ++ "\n}"

toJSBool :: Bool -> [Char]
toJSBool True = "true"
toJSBool False = "false"

instance Serialise Wall_grid where
  toJSON (Just a) =
    "{\n"
    ++ "  \"u1\": " ++ toJSBool (u1 a) ++ ",\n"
    ++ "  \"u2\": " ++ toJSBool (u2 a) ++ ",\n"
    ++ "  \"v1\": " ++ toJSBool (v1 a) ++ ",\n"
    ++ "  \"v2\": " ++ toJSBool (v2 a) ++ ",\n"
    ++ "  \"u1Texture\": " ++ show ((texture a) !! 0) ++ ",\n"
    ++ "  \"u2Texture\": " ++ show ((texture a) !! 1) ++ ",\n"
    ++ "  \"v1Texture\": " ++ show ((texture a) !! 2) ++ ",\n"
    ++ "  \"v2Texture\": " ++ show ((texture a) !! 3) ++ ",\n"
    ++ toJSON (obj a)
    ++ "\n}" 

instance Serialise Floor_grid where
  toJSON (Just a) =
    "{\n"
    ++ "  \"surface\": \"" ++ show (surface a) ++ "\""
    ++ "\n}"

instance Serialise Obj_grid where
  toJSON (Just a) =
    "{\n"
    ++ "  \"objType\": " ++ show (objType a) ++ ",\n"
    ++ "  \"program\": " ++ show (program a) ++ ",\n"
    ++ "  \"programName\": " ++ show (programName a)
    ++ "\n}"

instance Serialise Token where
  toJSON (Just a) =
    "{\n"
    ++ "  \"line\": " ++ show (line a) ++ ",\n"
    ++ "  \"column\": " ++ show (column a) ++ ",\n"
    ++ "  \"content\": " ++ show (content a) ++ ",\n"
    ++ "  \"textColour\": " ++ show (textColour a)
    ++ "\n}"

-- These three functions read the state of a set of voxels in the map, which is serialised and sent to the client.
readVoxel :: Server_state -> [Char] -> Int -> Int -> Int -> [Char]
readVoxel server_state voxel_type w u v
  | voxel_type == "Wall_grid" = toJSON (Just ((w_grid_ server_state) ! (w, u, v)))
  | voxel_type == "Floor_grid" = toJSON (Just ((f_grid_ server_state) ! (w, u, v)))
  | voxel_type == "Obj_grid" = toJSON (Just ((obj_grid_ server_state) ! (w, u, v)))

readVoxels :: Server_state -> Int -> Int -> Int -> Int -> Int -> [Char] -> [Char] -> [Char]
readVoxels server_state w u v u_max v_max voxel_type acc
  | u > u_max = take ((length acc) - 2) acc
  | v > v_max = readVoxels server_state w (u + 1) 0 u_max v_max voxel_type acc
  | otherwise = readVoxels server_state w u (v + 1) u_max v_max voxel_type (voxel ++ ",\n" ++ acc)
  where voxel = readVoxel server_state voxel_type w u v

readVoxelsCommand :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
readVoxelsCommand server_state args =
  let w = read (args !! 0)
      u_min = read (args !! 1)
      v_min = read (args !! 2)
      u_max = read (args !! 3)
      v_max = read (args !! 4)
      voxel_type = args !! 5
  in (Nothing, "[" ++ readVoxels server_state w u_min v_min u_max v_max voxel_type [] ++ "\n]")

-- This function serialises annotated GPLC source code to JSON for sending to the client.
serialiseSourceCode :: Array (Int, Int) Token -> Int -> Int -> Int -> Int -> [Char] -> [Char]
serialiseSourceCode token_arr i j i_max j_max output
  | i > i_max = take ((length output) - 2) output
  | j > j_max = serialiseSourceCode token_arr (i + 1) 0 i_max j_max output
  | token == defToken = serialiseSourceCode token_arr (i + 1) 0 i_max j_max output
  | otherwise = serialiseSourceCode token_arr i (j + 1) i_max j_max (toJSON (Just (token {line = i + 1})) ++ ",\n" ++ output)
  where token = token_arr ! (i, j)

-- This function serialises GPLC bytecode for sending to the client.
serialiseBytecode :: Int -> SEQ.Seq Int -> [Char] -> [Char]
serialiseBytecode mode SEQ.Empty output = reverse output
serialiseBytecode mode (x SEQ.:<| xs) output
  | x == 536870912 = serialiseBytecode mode xs ("\n" ++ output)
  | otherwise = serialiseBytecode mode xs (delimiter ++ reverse (show x) ++ output)
  where delimiter = if mode == 0 then " " else " ,"

-- A hash is generated from the source code of a GPLC program at compile time, which the client will append to the
-- program name at deployment time.  This is so the client can tell if the source code for an existing program in the map
-- has been modified since it was last deployed (i.e. indicating that source and bytecode no longer represent the same program).
-- Such a change would break the client's logic for modifying the properties of an existing program, so this needs to be disabled
-- in this situation.  These two functions convert the ByteString returned by SHA256.hash to a hex string.
genHexTable :: Int -> Int -> Int -> [(Int, [Char])] -> Array Int [Char]
genHexTable i j k tableList
  | i > 15 = array (0, 255) tableList
  | j > 15 = genHexTable (i + 1) 0 k tableList
  | otherwise = genHexTable i (j + 1) (k + 1) ((k, [hexSymbols ! j, hexSymbols ! i]) : tableList)
  where hexSymbols = array (0, 15) [(0, '0'), (1, '1'), (2, '2'), (3, '3'), (4, '4'), (5, '5'), (6, '6'), (7, '7'),
                                    (8, '8'), (9, '9'), (10, 'a'), (11, 'b'), (12, 'c'), (13, 'd'), (14, 'e'), (15, 'f')] :: Array Int Char

formatHash :: BS.ByteString -> Array Int [Char] -> Int -> [Char] -> [Char]
formatHash binaryHash hexTable i hexHash
  | i > 31 = reverse hexHash
  | otherwise = formatHash binaryHash hexTable (i + 1) ((hexTable ! (fromIntegral (BS.index binaryHash i))) ++ hexHash)

-- This is the entry point function for the logic in CompileGPLC and handles the compilation of GPLC
-- programs to bytecode.
compileProgram :: [Char] -> [Char] -> GPLC_program
compileProgram name source =
  let split_contents = splitOn " " source
      array_dim = detArrayDim split_contents 0 0 0
      empty_token_array = array ((0, 0), (fst__ array_dim, (snd__ array_dim) - 1))
                                [((i, j), defToken) | i <- [0..fst__ array_dim], j <- [0..(snd__ array_dim) - 1]]
      token_arr = tokenise split_contents empty_token_array 1 0 0
      bound_symbols = genSymbolBindings token_arr [] [] 0 0 0 (fst__ array_dim)
      signal_block = genSignalBlock token_arr (snd__ bound_symbols) (fst__ array_dim) 0 0 SEQ.empty []
      signal_code_block_size = sizeSignalCodeBlock (fst signal_block) 2 (SEQ.length (fst signal_block) - 1) 0
      add_write_ref_key = addWriteRefKey (signal_code_block_size + 4)
      code_block = genCodeBlock token_arr (map add_write_ref_key (fst__ bound_symbols)) (snd__ bound_symbols) (fst__ array_dim) SEQ.empty []
      data_block = genDataBlock (fst__ bound_symbols) SEQ.empty
      show_data_block = serialiseBytecode 1 data_block []
      colour_update = addColour token_arr 0 (fst__ array_dim) []
      serialised_source = serialiseSourceCode (token_arr // colour_update) 0 0 (fst (snd (bounds token_arr))) (snd (snd (bounds token_arr))) []
  in
  if third_ array_dim /= [] then error (third_ array_dim)
  else if third_ bound_symbols /= [] then error ("\n" ++ show (third_ bound_symbols))
  else if snd signal_block /= [] then error ("\n" ++ show (snd signal_block))
  else if snd code_block /= [] then error ("\n" ++ show (snd code_block))
  else GPLC_program {name = name,
                     hash = formatHash (SHA256.hash (BSC.pack source)) (genHexTable 0 0 0 []) 0 [],
                     source = "[\n" ++ serialised_source ++ "\n]",
                     bytecode = "[" ++ serialiseBytecode 1 (fst signal_block) []
                                ++ serialiseBytecode 1 (fst code_block) []
                                ++ take ((length show_data_block) - 3) show_data_block ++ "]"}

-- This function allows the client to query the properties of the GPLC programs the server compiled at its last start time.
queryProgram :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
queryProgram server_state args =
  let i = read (args !! 0)
      program = (gplcPrograms server_state) ! i
  in (Nothing,
      "\nname: " ++ name program
      ++ "\nhash: " ++ hash program
      ++ "\nsource: " ++ source program
      ++ "\nbytecode: " ++ bytecode program)

-- This function allows the client to view a list of the GPLC programs the server compiled at its last start time.
listProgram :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
listProgram server_state args = (Nothing, "\nloaded programs: " ++ show (map name (elems (gplcPrograms server_state))))

-- These are the pages used in the hierarchical dictionary look up used to interpret server commands.
page0 = ["read", "write", "GPLC"]
page1 = ["Wall_grid", "Floor_grid", "Obj_grid"]
page2 = ["structure", "textures", "Obj_place"]
page3 = ["query", "list"]

-- These are the sets of branches that exist for non - end nodes.
baseBranches = array (0, 2) [(0, readNode), (1, writeNode), (2, gplcNode)]

writeNodeBranches = array (0, 2) [(0, wallGridNode), (1, floorGridNode), (2, objGridNode)]

wallGridNodeBranches = array (0, 2) [(0, structureNode), (1, texturesNode), (2, objPlaceNode)]

gplcNodeBranches = array (0, 1) [(0, queryNode), (1, listNode)]

-- These are the nodes of the decision tree.
baseNode = Comm_struct {dictionaryPage = page0, branches = Just baseBranches, readWriteGameState = Nothing}

readNode = Comm_struct {dictionaryPage = [], branches = Nothing, readWriteGameState = Just readVoxelsCommand}

writeNode = Comm_struct {dictionaryPage = page1, branches = Just writeNodeBranches, readWriteGameState = Nothing}

gplcNode = Comm_struct {dictionaryPage = page3, branches = Just gplcNodeBranches, readWriteGameState = Nothing}

wallGridNode = Comm_struct {dictionaryPage = page2, branches = Just wallGridNodeBranches, readWriteGameState = Nothing}

floorGridNode = Comm_struct {dictionaryPage = [], branches = Nothing, readWriteGameState = Just writeFloorGrid}

objGridNode = Comm_struct {dictionaryPage = [], branches = Nothing, readWriteGameState = Just writeObjGrid}

structureNode = Comm_struct {dictionaryPage = [], branches = Nothing, readWriteGameState = Just writeWallGridStructure}

texturesNode = Comm_struct {dictionaryPage = [], branches = Nothing, readWriteGameState = Just writeWallGridTextures}

objPlaceNode = Comm_struct {dictionaryPage = [], branches = Nothing, readWriteGameState = Just writeObjPlace}

queryNode = Comm_struct {dictionaryPage = [], branches = Nothing, readWriteGameState = Just queryProgram}

listNode = Comm_struct {dictionaryPage = [], branches = Nothing, readWriteGameState = Just listProgram}

-- This function traverses the decision tree and thereby interprets server commands.
interpretCommand :: [[Char]] -> Comm_struct -> Server_state -> (Server_state, [Char])
interpretCommand [] comm_struct server_state = (server_state, "\nInvalid command (1).")
interpretCommand (x:xs) comm_struct server_state =
  let new_game_state = (fromJust (readWriteGameState comm_struct)) server_state (x:xs)
      look_up = elemIndex x (dictionaryPage comm_struct)
  in
  if isNothing (branches comm_struct) == True then
    if isNothing (fst new_game_state) == True then (server_state, "\n" ++ snd new_game_state)
    else (fromJust (fst new_game_state), "\n" ++ snd new_game_state)
  else
    if isNothing look_up == True then (server_state, "\nInvalid command (2).")
    else interpretCommand xs ((fromJust (branches comm_struct)) ! (fromJust look_up)) server_state

