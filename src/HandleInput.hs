-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This module is part of the development server and interprets commands received by the server.

{-# LANGUAGE FlexibleInstances #-}

module HandleInput where

import Data.List.Split
import Data.Array.IArray
import Data.Maybe
import Data.List
import qualified Data.Sequence as SEQ
import qualified Data.ByteString.Char8 as BS
import qualified Crypto.Hash.SHA256 as SHA256
import BuildModel hiding (Game_state, w_grid_, f_grid_, obj_grid_)
import CompileGPLC
import qualified IndexWrapper1 as IW

data GPLC_program = GPLC_program {name :: [Char], hash :: [Char], source :: [Char], bytecode :: [Char]}

empty_gplc_program = GPLC_program {name = [], hash = [], source = [], bytecode = []}

data Server_state = Server_state {w_grid_ :: Array (Int, Int, Int) Wall_grid, f_grid_ :: Array (Int, Int, Int) Floor_grid,
                                  obj_grid_ :: Array (Int, Int, Int) Obj_grid, gplcPrograms :: Array Int GPLC_program}

-- This recursive data type is used to implement the control structure that updates the map state in response to commands received by the server.
data Comm_struct = Comm_struct {dictionary_page :: [[Char]], branches :: Maybe (Array Int Comm_struct),
                                read_write_game_state :: Maybe (Server_state -> [[Char]] -> (Maybe Server_state, [Char]))}

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
writeFloorGrid game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      height = read (args !! 3)
      terrain = parseTerrain (args !! 4)
      f' = Floor_grid {w_ = height, surface = terrain, local_up_ramp = (0, 0), local_down_ramp = (0, 0)}
      success_str = "writeFloorGrid succeeded.  Arguments passed were w: "
      boundsCheck = IW.boundsCheck (f_grid_ game_state) (w, u, v) "writeFloorGrid"
  in
  if isNothing boundsCheck then 
    (Just game_state {f_grid_ = (f_grid_ game_state) // [((w, u, v), f')]},
     success_str ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " height: " ++ (args !! 3) ++ " terrain: " ++ (args !! 4))
  else (Nothing, fromJust boundsCheck)

constructProgBlock :: [[Char]] -> [Int]
constructProgBlock [] = []
constructProgBlock (x:xs) = (read x) : constructProgBlock xs

writeObjGrid :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
writeObjGrid game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      obj_type = read (args !! 3)
      boundsCheck = IW.boundsCheck (obj_grid_ game_state) (w, u, v) "writeObjGrid"
  in
  if isNothing boundsCheck then
    (Just game_state {obj_grid_ = (obj_grid_ game_state) // [((w, u, v), Obj_grid {objType = obj_type,
                                                                                   program = constructProgBlock (drop 4 args),
                                                                                   programName = []})]},
     "writeObjGrid succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " obj_type: " ++ (args !! 3))
  else (Nothing, fromJust boundsCheck)

writeWallGridStructure :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
writeWallGridStructure game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = \i -> intToBool (read (args !! i))
      upd_ = [read (args !! 7), read (args !! 8), read (args !! 9), read (args !! 10)]
      w' = ((w_grid_ game_state) ! (w, u, v)) {u1 = upd 3, u2 = upd 4, v1 = upd 5, v2 = upd 6, wall_flag = upd_}
      success_str = "writeWallGridStructure succeeded.  Arguments passed were w: "
      boundsCheck = IW.boundsCheck (w_grid_ game_state) (w, u, v) "writeWallGridStructure"
  in
  if isNothing boundsCheck then
    (Just game_state {w_grid_ = (w_grid_ game_state) // [((w, u, v), w')]},
     success_str ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))
  else (Nothing, fromJust boundsCheck)

writeWallGridTextures :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
writeWallGridTextures game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = [read (args !! 3), read (args !! 4), read (args !! 5), read (args !! 6)]
      success_str = "writeWallGridTextures succeeded.  Arguments passed were w: "
      boundsCheck = IW.boundsCheck (w_grid_ game_state) (w, u, v) "writeWallGridTextures"
  in
  if isNothing boundsCheck then
    (Just game_state {w_grid_ = (w_grid_ game_state) // [((w, u, v), ((w_grid_ game_state) ! (w, u, v)) {texture = upd})]},
     success_str ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))
  else (Nothing, fromJust boundsCheck)

writeObjPlace :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
writeObjPlace game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = \i -> read (args !! i)
      upd_ = \i -> read (args !! i)
      w_grid__ = w_grid_ game_state
      obj' = def_obj_place {ident_ = upd 3, u__ = upd_ 4, v__ = upd_ 5, w__ = upd_ 6, texture__ = upd 7, num_elem = read (args !! 8), obj_flag = upd 9}
      success_str = "writeObjPlace succeeded.  Arguments passed were w: "
      boundsCheck = IW.boundsCheck (w_grid_ game_state) (w, u, v) "writeObjPlace"
  in
  if isNothing boundsCheck then
    (Just game_state {w_grid_ = w_grid__ // [((w, u, v), (w_grid__ ! (w, u, v)) {obj = Just obj'})]},
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
readVoxel game_state voxel_type w u v
  | voxel_type == "Wall_grid" = toJSON (Just ((w_grid_ game_state) ! (w, u, v)))
  | voxel_type == "Floor_grid" = toJSON (Just ((f_grid_ game_state) ! (w, u, v)))
  | voxel_type == "Obj_grid" = toJSON (Just ((obj_grid_ game_state) ! (w, u, v)))

readVoxels :: Server_state -> Int -> Int -> Int -> Int -> Int -> [Char] -> [Char] -> [Char]
readVoxels game_state w u v u_max v_max voxel_type acc
  | u > u_max = take ((length acc) - 2) acc
  | v > v_max = readVoxels game_state w (u + 1) 0 u_max v_max voxel_type acc
  | otherwise = readVoxels game_state w u (v + 1) u_max v_max voxel_type (voxel ++ ",\n" ++ acc)
  where voxel = readVoxel game_state voxel_type w u v

readVoxelsCommand :: Server_state -> [[Char]] -> (Maybe Server_state, [Char])
readVoxelsCommand game_state args =
  let w = read (args !! 0)
      u_min = read (args !! 1)
      v_min = read (args !! 2)
      u_max = read (args !! 3)
      v_max = read (args !! 4)
      voxel_type = args !! 5
  in (Nothing, "[" ++ readVoxels game_state w u_min v_min u_max v_max voxel_type [] ++ "\n]")

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
  where delimiter = if mode == 0 then " " else ", "

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
                     hash = BS.unpack (SHA256.hash (BS.pack source)),
                     source = serialised_source,
                     bytecode = serialiseBytecode 1 (fst signal_block) []
                                ++ serialiseBytecode 1 (fst code_block) []
                                ++ take ((length show_data_block) - 3) show_data_block}

-- These are the pages used in the hierarchical dictionary look up used to interpret server commands.
page0 = ["read", "write"]
page1 = ["Wall_grid", "Floor_grid", "Obj_grid"]
page2 = ["structure", "textures", "Obj_place"]

-- These are the sets of branches that exist for non - end nodes.
baseBranches = array (0, 1) [(0, readNode), (1, writeNode)]

setNodeBranches = array (0, 2) [(0, wallGridNode), (1, floorGridNode), (2, objGridNode)]

wallGridNodeBranches = array (0, 2) [(0, structureNode), (1, texturesNode), (2, objPlaceNode)]

-- These are the nodes of the decision tree.
baseNode = Comm_struct {dictionary_page = page0, branches = Just baseBranches, read_write_game_state = Nothing}

readNode = Comm_struct {dictionary_page = [], branches = Nothing, read_write_game_state = Just readVoxelsCommand}

writeNode = Comm_struct {dictionary_page = page1, branches = Just setNodeBranches, read_write_game_state = Nothing}

wallGridNode = Comm_struct {dictionary_page = page2, branches = Just wallGridNodeBranches, read_write_game_state = Nothing}

floorGridNode = Comm_struct {dictionary_page = [], branches = Nothing, read_write_game_state = Just writeFloorGrid}

objGridNode = Comm_struct {dictionary_page = [], branches = Nothing, read_write_game_state = Just writeObjGrid}

structureNode = Comm_struct {dictionary_page = [], branches = Nothing, read_write_game_state = Just writeWallGridStructure}

texturesNode = Comm_struct {dictionary_page = [], branches = Nothing, read_write_game_state = Just writeWallGridTextures}

objPlaceNode = Comm_struct {dictionary_page = [], branches = Nothing, read_write_game_state = Just writeObjPlace}

-- This function traverses the decision tree and thereby interprets server commands.
interpretCommand :: [[Char]] -> Comm_struct -> Server_state -> (Server_state, [Char])
interpretCommand [] comm_struct game_state = (game_state, "\nInvalid command (1).")
interpretCommand (x:xs) comm_struct game_state =
  let new_game_state = (fromJust (read_write_game_state comm_struct)) game_state (x:xs)
      look_up = elemIndex x (dictionary_page comm_struct)
  in
  if isNothing (branches comm_struct) == True then
    if isNothing (fst new_game_state) == True then (game_state, "\n" ++ snd new_game_state)
    else (fromJust (fst new_game_state), "\n" ++ snd new_game_state)
  else
    if isNothing look_up == True then (game_state, "\nInvalid command (2).")
    else interpretCommand xs ((fromJust (branches comm_struct)) ! (fromJust look_up)) game_state

