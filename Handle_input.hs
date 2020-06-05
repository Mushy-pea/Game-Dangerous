-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- The engine has three modes of receiving user input, namely interactive (during game play), menu and console.  The user interface code that implements each of
-- these modes is in this module.  It is anticipated that the console will be used for testing purposes and to allow automation of engine inputs in the
-- developer build of the engine.  This automation is intended to allow a separate editor program to control the engine to make use of it as a renderer for the
-- map under construction.

module Handle_input where

import Graphics.GL.Core33
import Graphics.UI.GLUT
import Data.IORef
import Data.Array.IArray
import Data.List.Split
import Build_model
import Encode_status

-- This recursive data type is used to implement the control structure that updates the game state in response to console input.
data Comm_struct = Comm_struct {dictionary_page :: [[Char]], branches :: Maybe (Array Int Comm_struct), update_game_state :: Maybe (Game_state -> [[Char]] -> Maybe Game_state)}

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

-- These are the functions that update the game state in response to the engine receiving console commands.  They are all of type (Game_state -> [[Char]] -> Maybe Game_state).
unlock_wrapper game_state code =
  let bit_list = pad (decimal_binary (hex_decimal (code !! 0) 0) (2 ^ 127)) [] 127 0
      state_values = extract_state_values (listArray (0, 127) bit_list) (s0_ game_state) (s1_ game_state) 0
  in if third_ state_values == True then Just game_state {s0_ = fst__ state_values, s1_ = snd__ state_values}
     else Nothing

send_signal_ game_state args =
  let sig = read (args !! 0)
      w = read (args !! 1)
      u = read (args !! 2)
      v = read (args !! 3)
      bd = bounds (obj_grid_ game_state)
  in
  if w < fst__ (fst bd) || w > fst__ (snd bd) || u < snd__ (fst bd) || u > snd__ (snd bd) || v < third_ (fst bd) || v > third_ (snd bd) then Nothing
  else Just game_state {s1_ = ((s1_ game_state) {sig_q = [sig, w, u, v]})}

set_floor_grid game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      height = read (args !! 3)
      terrain = read (args !! 4)
      bd = bounds (f_grid_ game_state)
  in
  if w < fst__ (fst bd) || w > fst__ (snd bd) || u < snd__ (fst bd) || u > snd__ (snd bd) || v < third_ (fst bd) || v > third_ (snd bd) then Nothing
  else Just game_state {f_grid_ = (f_grid_ game_state) // [((w, u, v), Floor_grid {w_ = height, surface = terrain})]}

construct_prog_block :: [[Char]] -> [Int]
construct_prog_block [] = []
construct_prog_block (x:xs) = (read x) : construct_prog_block xs

set_obj_grid game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      obj_type = read (args !! 3)
      bd = bounds (obj_grid_ game_state)
  in
  if w < fst__ (fst bd) || w > fst__ (snd bd) || u < snd__ (fst bd) || u > snd__ (snd bd) || v < third_ (fst bd) || v > third_ (snd bd) then Nothing
  else Just game_state {obj_grid_ = (obj_grid_ game_state) // [((w, u, v), (obj_type, construct_prog_block (drop 4 args)))]}

--set_wall_grid_structure game_state args =
--  let w = read (args !! 0)
--      u = read (args !! 1)
--      v = read (args !! 2)
--      bd = bounds (wall_grid_ game_state)
--  in
--  if w < fst__ (fst bd) || w > fst__ (snd bd) || u < snd__ (fst bd) || u > snd__ (snd bd) || v < third_ (fst bd) || v > third_ (snd bd) then Nothing
--  else Just game_state {wall_grid_ = (wall_grid_ game_state) // [((w, u, v), ((wall_grid_ game_state) ! (w, u, v)) {

-- These are the pages used in the hierarchical dictionary look up used to interpret console input.
page0 = ["unlock", "set", "send_signal"]
page1 = ["Wall_grid", "Floor_grid", "Obj_grid", "Play_state0", "Play_State1"]
page2 = ["structure", "textures", "Obj_place"]
page3 = ["position", "angle", "rend_mode"]
page4 = ["health", "ammo", "gems", "torches", "keys", "region", "difficulty", "verbose_mode", "story_state"]

-- These are the sets of branches that exist for non - end nodes.
base_branches = array (0, 2) [(0, unlock_node), (1, set_node), (2, send_signal_node)]

set_node_branches = array (0, 4) [(0, wall_grid_node), (1, floor_grid_node), (2, obj_grid_node), (3, play_state0_node), (4, play_state1_node)]

wall_grid_node_branches = array (0, 2) [(0, structure_node), (1, textures_node), (2, obj_place_node)]

play_state0_node_branches = array (0, 2) [(0, position_node), (1, angle_node), (2, rend_mode_node)]

play_state1_node_branches = array (0, 8) [(0, health_node), (1, ammo_node), (2, gems_node), (3, torches_node), (4, keys_node), (5, region_node), (6, difficulty_node), (7, verbose_mode_node), (8, story_state_node)]

-- These are the nodes of the decision tree.
base_node = Comm_struct {dictionary_page = page0, branches = Just base_branches, update_game_state = Nothing}

unlock_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just unlock_wrapper}

set_node = Comm_struct {dictionary_page = page1, branches = Just set_node_branches, update_game_state = Nothing}

send_signal_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just send_signal_}

wall_grid_node = Comm_struct {dictionary_page = page2, branches = Just wall_grid_node_branches, update_game_state = Nothing}

floor_grid_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_floor_grid}

obj_grid_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = set_obj_grid}

play_state0_node = Comm_struct {dictionary_page = page3, branches = Just play_state0_node_branches, update_game_state = Nothing}

play_state1_node = Comm_struct {dictionary_page = page4, branches = Just play_state1_node_branches, update_game_state = Nothing}

structure_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_wall_grid_structure}

textures_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_wall_grid_textures}

obj_place_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_obj_place}

position_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_player_position}

angle_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_camera_angle}

rend_mode_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_rend_mode}

health_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_health}

ammo_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_ammo}

gems_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_gems}

torches_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_torches}

keys_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_keys}

region_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_region}

difficulty_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_difficulty}

verbose_mode_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_verbose_mode}

story_state_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_story_state}


-- When the engine is in interactive or menu input mode, this is the callback that GLUT calls each time mainLoopEvent has been called and there is keyboard input
-- in the window message queue.
get_input :: IORef Int -> Array Int Char -> Char -> Position -> IO ()
get_input ref key_set key pos = do
  if key == key_set ! 0 then writeIORef ref 2         -- Pause
  else if key == key_set ! 1 then writeIORef ref 3    -- Forward
  else if key == key_set ! 2 then writeIORef ref 4    -- Strafe right
  else if key == key_set ! 3 then writeIORef ref 5    -- Back
  else if key == key_set ! 4 then writeIORef ref 6    -- Strafe Left
  else if key == key_set ! 5 then writeIORef ref 7    -- Turn left
  else if key == key_set ! 6 then writeIORef ref 8    -- Turn right
  else if key == key_set ! 7 then writeIORef ref 9    -- Jump
  else if key == key_set ! 8 then writeIORef ref 10   -- Light torch
  else if key == key_set ! 9 then writeIORef ref 11   -- Switch view mode
  else if key == key_set ! 10 then writeIORef ref 12  -- Rotate 3rd person view
  else if key == key_set ! 11 then writeIORef ref 13  -- Fire
  else if key == key_set ! 12 then writeIORef ref 14  -- Select menu option
  else if key == key_set ! 13 then writeIORef ref 15  -- Go back one level in menu
  else if key == key_set ! 14 then writeIORef ref 16  -- Return to menu root
  else writeIORef ref 0

-- When the engine is in console input mode, this is the callback that GLUT calls each time mainLoopEvent has been called and there is keyboard input
-- in the window message queue.
get_console_input :: IORef Int -> Array Int Char -> Int -> Char -> Position -> IO ()
get_console_input ref key_table i key pos = do
  if i > 36 then return ()
  else if key_table ! i == key then writeIORef ref i
  else get_console_input ref key_table (i + 1) key pos


