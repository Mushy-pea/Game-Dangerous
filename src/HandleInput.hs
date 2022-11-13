-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This module is part of the development server and interprets commands received by the server.  It was originaly intended to be part of a console command
-- interpreter for the engine but has been repurposed.  Some of the functionality is therefore currently unused but has been kept in case a console command
-- system is added to the engine again later.

module HandleInput where

import Data.Array.IArray
import Data.Maybe
import Data.List
import BuildModel
import qualified IndexWrapper1 as IR

-- This recursive data type is used to implement the control structure that updates the map state in response to commands received by the server.
data Comm_struct = Comm_struct {dictionary_page :: [[Char]], branches :: Maybe (Array Int Comm_struct),
                                update_game_state :: Maybe (Game_state -> [[Char]] -> (Maybe Game_state, [Char]))}

-- The following shows the structure of the decision tree implemented using the above data type.

-- unlock(0) (unlock_wrapper)

-- set(0) -> Wall_grid (1) 			-> structure (2) (set_wall_grid_structure)
--							-> textures (2) (set_wall_grid_textures)
--							-> Obj_place (2) (set_obj_place)

--  -> Floor_grid (1) (set_floor_grid)
--  -> Obj_grid(1) (set_obj_grid)
--  -> Play_state0(1) 				-> position (3) (set_player_position)
--							-> angle (3) (set_camera_angle)
--							-> rend_mode (3) (set_rend_mode)

--  -> Play_State1(1)				-> health (4) (set_health)
--							-> ammo (4) (set_ammo)
--							-> gems (4) (set_gems)
--							-> torches (4) (set_torches)
--							-> keys (4) (set_keys)
--							-> region (4) (set_region)
--							-> difficulty (4) (set_difficulty)
--							-> verbose_mode (4) (set_verbose_mode)
--							-> story_state (4) (set_story_state)

--send_signal(0) (send_signal_)

-- These are the functions that update the map state in response to the server receiving commands.
parseTerrain :: [Char] -> Terrain
parseTerrain t_name
  | t_name == "Positive_u" = BuildModel.Positive_u
  | t_name == "Negative_u" = BuildModel.Negative_u
  | t_name == "Positive_v" = BuildModel.Positive_v
  | t_name == "Negative_v" = BuildModel.Negative_v
  | t_name == "Flat" = BuildModel.Flat
  | t_name == "Open" = BuildModel.Open

setFloorGrid :: Game_state -> [[Char]] -> (Maybe Game_state, [Char])
setFloorGrid game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      height = read (args !! 3)
      terrain = parseTerrain (args !! 4)
      boundsCheck = IR.boundsCheck (f_grid_ game_state) (w, u, v) "setFloorGrid"
  in
  if isNothing boundsCheck then 
    (Just game_state {f_grid_ = (f_grid_ game_state) // [((w, u, v), Floor_grid {w_ = height, surface = terrain, local_up_ramp = (0, 0), local_down_ramp = (0, 0)})]},
     "setFloorGrid succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " height: " ++ (args !! 3)
     ++ " terrain: " ++ (args !! 4))
  else (Nothing, fromJust boundsCheck)

constructProgBlock :: [[Char]] -> [Int]
constructProgBlock [] = []
constructProgBlock (x:xs) = (read x) : constructProgBlock xs

setObjGrid :: Game_state -> [[Char]] -> (Maybe Game_state, [Char])
setObjGrid game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      obj_type = read (args !! 3)
      boundsCheck = IR.boundsCheck (obj_grid_ game_state) (w, u, v) "setObjGrid"
  in
  if isNothing boundsCheck then
    (Just game_state {obj_grid_ = (obj_grid_ game_state) // [((w, u, v), (obj_type, constructProgBlock (drop 4 args)))]},
        "setObjGrid succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " obj_type: " ++ (args !! 3))
  else (Nothing, fromJust boundsCheck)

setWallGridStructure :: Game_state -> [[Char]] -> (Maybe Game_state, [Char])
setWallGridStructure game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = \i -> intToBool (read (args !! i))
      upd_ = [read (args !! 7), read (args !! 8), read (args !! 9), read (args !! 10)]
      boundsCheck = IR.boundsCheck (w_grid_ game_state) (w, u, v) "setWallGridStructure"
  in
  if isNothing boundsCheck then
    (Just game_state {w_grid_ = (w_grid_ game_state) // [((w, u, v), ((w_grid_ game_state) ! (w, u, v)) {u1 = upd 3, u2 = upd 4, v1 = upd 5, v2 = upd 6, wall_flag = upd_})]},
        "setWallGridStructure succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))
  else (Nothing, fromJust boundsCheck)

setWallGridTextures :: Game_state -> [[Char]] -> (Maybe Game_state, [Char])
setWallGridTextures game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = [read (args !! 3), read (args !! 4), read (args !! 5), read (args !! 6)]
      boundsCheck = IR.boundsCheck (w_grid_ game_state) (w, u, v) "setWallGridTextures"
  in
  if isNothing boundsCheck then
    (Just game_state {w_grid_ = (w_grid_ game_state) // [((w, u, v), ((w_grid_ game_state) ! (w, u, v)) {texture = upd})]},
     "setWallGridTextures succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))
  else (Nothing, fromJust boundsCheck)

setObjPlace :: Game_state -> [[Char]] -> (Maybe Game_state, [Char])
setObjPlace game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = \i -> read (args !! i)
      upd_ = \i -> read (args !! i)
      w_grid__ = w_grid_ game_state
      boundsCheck = IR.boundsCheck (w_grid_ game_state) (w, u, v) "setObjPlace"
  in
  if isNothing boundsCheck then
    (Just game_state {w_grid_ = w_grid__ // [((w, u, v), (w_grid__ ! (w, u, v)) {obj = Just def_obj_place {ident_ = upd 3, u__ = upd_ 4, v__ = upd_ 5, w__ = upd_ 6, texture__ = upd 7, num_elem = read (args !! 8), obj_flag = upd 9}})]},
     "setObjPlace succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))
  else (Nothing, fromJust boundsCheck)

-- These are the pages used in the hierarchical dictionary look up used to interpret server commands.
page0 = ["set"]
page1 = ["Wall_grid", "Floor_grid", "Obj_grid"]
page2 = ["structure", "textures", "Obj_place"]

-- These are the sets of branches that exist for non - end nodes.
baseBranches = array (0, 0) [(0, setNode)]

setNodeBranches = array (0, 2) [(0, wallGridNode), (1, floorGridNode), (2, objGridNode)]

wallGridNodeBranches = array (0, 2) [(0, structureNode), (1, texturesNode), (2, objPlaceNode)]

-- These are the nodes of the decision tree.
baseNode = Comm_struct {dictionary_page = page0, branches = Just baseBranches, update_game_state = Nothing}

setNode = Comm_struct {dictionary_page = page1, branches = Just setNodeBranches, update_game_state = Nothing}

wallGridNode = Comm_struct {dictionary_page = page2, branches = Just wallGridNodeBranches, update_game_state = Nothing}

floorGridNode = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just setFloorGrid}

objGridNode = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just setObjGrid}

structureNode = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just setWallGridStructure}

texturesNode = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just setWallGridTextures}

objPlaceNode = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just setObjPlace}

-- This function traverses the decision tree and thereby interprets server commands.
interpretCommand :: [[Char]] -> Comm_struct -> Game_state -> (Game_state, [Char])
interpretCommand [] comm_struct game_state = (game_state, "\nInvalid command (1).")
interpretCommand (x:xs) comm_struct game_state =
  let new_game_state = (fromJust (update_game_state comm_struct)) game_state (x:xs)
      look_up = elemIndex x (dictionary_page comm_struct)
  in
  if isNothing (branches comm_struct) == True then
    if isNothing (fst new_game_state) == True then (game_state, "\n" ++ snd new_game_state)
    else (fromJust (fst new_game_state), "\n" ++ snd new_game_state)
  else
    if isNothing look_up == True then (game_state, "\nInvalid command (2).")
    else interpretCommand xs ((fromJust (branches comm_struct)) ! (fromJust look_up)) game_state


