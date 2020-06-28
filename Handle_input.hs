-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- The engine has three modes of receiving user input, namely interactive (during game play), menu and console.  The user interface code that implements each of
-- these modes is in this module.  It is anticipated that the console will be used for testing purposes and to allow automation of engine inputs in the
-- developer build of the engine.  This automation is intended to allow a separate editor program to control the engine to make use of it as a renderer for the
-- map under construction.

module Handle_input where

import Control.Concurrent
import Graphics.GL.Core33
import Graphics.UI.GLUT hiding (texture)
import Foreign
import Data.IORef
import Data.Array.IArray
import Data.Array.Unboxed
import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Matrix as MAT
import Unsafe.Coerce
import Build_model
import Encode_status

-- Used to load C style arrays, which are used with certain OpenGL functions.
load_array :: Storable a => [a] -> Ptr a -> Int -> IO ()
load_array [] p i = return ()
load_array (x:xs) p i = do
  pokeElemOff p i x
  load_array xs p (i + 1)

-- This recursive data type is used to implement the control structure that updates the game state in response to console input.
data Comm_struct = Comm_struct {dictionary_page :: [[Char]], branches :: Maybe (Array Int Comm_struct), update_game_state :: Maybe (Game_state -> [[Char]] -> (Maybe Game_state, [Char]))}

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
      state_values = extract_state_values (listArray (0, 127) bit_list) (s0__ game_state) (s1_ game_state) 0
  in if third_ state_values == True then (Just game_state {s0__ = fst__ state_values, s1_ = snd__ state_values}, "Map unlocked!")
     else (Nothing, "Invalid map unlock code.")

send_signal_ game_state args =
  let sig = read (args !! 0)
      w = read (args !! 1)
      u = read (args !! 2)
      v = read (args !! 3)
      bd = bounds (obj_grid_ game_state)
  in
  if w < fst__ (fst bd) || w > fst__ (snd bd) || u < snd__ (fst bd) || u > snd__ (snd bd) || v < third_ (fst bd) || v > third_ (snd bd) then (Nothing, "send_signal_ failed.  Arguments passed were sig: " ++ (args !! 0) ++ " w: " ++ (args !! 1) ++ " u: " ++ (args !! 2) ++ " v: " ++ (args !! 3))
  else (Just game_state {s1_ = ((s1_ game_state) {sig_q = [sig, w, u, v]})}, "send_signal succeeded.  Arguments passed were sig: " ++ (args !! 0) ++ " w: " ++ (args !! 1) ++ " u: " ++ (args !! 2) ++ " v: " ++ (args !! 3))

set_floor_grid game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      height = read (args !! 3)
      terrain = read (args !! 4)
      bd = bounds (f_grid_ game_state)
  in
  if w < fst__ (fst bd) || w > fst__ (snd bd) || u < snd__ (fst bd) || u > snd__ (snd bd) || v < third_ (fst bd) || v > third_ (snd bd) then (Nothing, "set_floor_grid failed.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " height: " ++ (args !! 3) ++ " terrain: " ++ (args !! 4))
  else (Just game_state {f_grid_ = (f_grid_ game_state) // [((w, u, v), Floor_grid {w_ = height, surface = terrain, local_up_ramp = (0, 0), local_down_ramp = (0, 0)})]}, "set_floor_grid succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " height: " ++ (args !! 3) ++ " terrain: " ++ (args !! 4))

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
  if w < fst__ (fst bd) || w > fst__ (snd bd) || u < snd__ (fst bd) || u > snd__ (snd bd) || v < third_ (fst bd) || v > third_ (snd bd) then (Nothing, "set_obj_grid failed.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " obj_type: " ++ (args !! 3))
  else (Just game_state {obj_grid_ = (obj_grid_ game_state) // [((w, u, v), (obj_type, construct_prog_block (drop 4 args)))]}, "set_obj_grid succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " obj_type: " ++ (args !! 3))

set_wall_grid_structure game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = \i -> int_to_bool (read (args !! i))
      upd_ = [read (args !! 7), read (args !! 8), read (args !! 9), read (args !! 10)]
      bd = bounds (w_grid_ game_state)
  in
  if w < fst__ (fst bd) || w > fst__ (snd bd) || u < snd__ (fst bd) || u > snd__ (snd bd) || v < third_ (fst bd) || v > third_ (snd bd) then (Nothing, "set_wall_grid_structure failed.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))
  else (Just game_state {w_grid_ = (w_grid_ game_state) // [((w, u, v), ((w_grid_ game_state) ! (w, u, v)) {u1 = upd 3, u2 = upd 4, v1 = upd 5, v2 = upd 6, wall_flag = upd_})]}, "set_wall_grid_structure succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))

set_wall_grid_textures game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = [read (args !! 3), read (args !! 4), read (args !! 5), read (args !! 6)]
      bd = bounds (w_grid_ game_state)
  in
  if w < fst__ (fst bd) || w > fst__ (snd bd) || u < snd__ (fst bd) || u > snd__ (snd bd) || v < third_ (fst bd) || v > third_ (snd bd) then (Nothing, "set_wall_grid_textures failed.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))
  else (Just game_state {w_grid_ = (w_grid_ game_state) // [((w, u, v), ((w_grid_ game_state) ! (w, u, v)) {texture = upd})]}, "set_wall_grid_textures succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))

set_obj_place game_state args =
  let w = read (args !! 0)
      u = read (args !! 1)
      v = read (args !! 2)
      upd = \i -> read (args !! i)
      upd_ = \i -> read (args !! i)
      w_grid__ = w_grid_ game_state
      bd = bounds w_grid__
  in
  if w < fst__ (fst bd) || w > fst__ (snd bd) || u < snd__ (fst bd) || u > snd__ (snd bd) || v < third_ (fst bd) || v > third_ (snd bd) then (Nothing, "set_obj_place failed.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))
  else (Just game_state {w_grid_ = w_grid__ // [((w, u, v), (w_grid__ ! (w, u, v)) {obj = Just def_obj_place {ident_ = upd 3, u__ = upd_ 4, v__ = upd_ 5, w__ = upd_ 6, texture__ = upd 7, num_elem = read (args !! 8), obj_flag = upd 9}})]}, "set_obj_place succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2) ++ " others: " ++ show (drop 3 args))

set_player_position game_state args =
  let w = truncate (read (args !! 0))
      u = truncate (read (args !! 1))
      v = truncate (read (args !! 2))
      bd = bounds (obj_grid_ game_state)
  in
  if w < fst__ (fst bd) || w > fst__ (snd bd) || u < snd__ (fst bd) || u > snd__ (snd bd) || v < third_ (fst bd) || v > third_ (snd bd) then (Nothing, "set_player_position failed.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2))
  else (Just game_state {s0__ = (s0__ game_state) {pos_u = read (args !! 0), pos_v = read (args !! 1), pos_w = read (args !! 2), vel = [0, 0, 0]}}, "set_player_position succeeded.  Arguments passed were w: " ++ (args !! 0) ++ " u: " ++ (args !! 1) ++ " v: " ++ (args !! 2))

set_camera_angle game_state args =
  let a = read (args !! 0)
  in if a < 0 || a > 628 then (Nothing, "set_camera_angle failed.  Arguments passed were angle: " ++ (args !! 0))
     else (Just game_state {s0__ = (s0__ game_state) {angle = a}}, "set_camera_angle succeeded.  Arguments passed were angle: " ++ (args !! 0))

set_rend_mode game_state args =
  let mode = read (args !! 0)
  in if mode < 0 || mode > 1 then (Nothing, "set_rend_mode failed.  Arguments passed were mode: " ++ (args !! 0))
  else (Just game_state {s0__ = (s0__ game_state) {rend_mode = mode}}, "set_rend_mode succeeded.  Arguments passed were mode: " ++ (args !! 0))

set_health game_state args = (Just game_state {s1_ = (s1_ game_state) {health = read (args !! 0)}}, "set_health succeeded.")

set_ammo game_state args = (Just game_state {s1_ = (s1_ game_state) {ammo = read (args !! 0)}}, "set_ammo succeeded.")

set_gems game_state args = (Just game_state {s1_ = (s1_ game_state) {gems = read (args !! 0)}}, "set_gems succeeded.")

set_torches game_state args = (Just game_state {s1_ = (s1_ game_state) {torches = read (args !! 0)}}, "set_torches succeeded.")

set_keys game_state args = (Just game_state {s1_ = (s1_ game_state) {keys = Encode_status.set_keys [read (args !! 0), read (args !! 1), read (args !! 2), read (args !! 3), read (args !! 4), read (args !! 5)] 77}}, "set_keys succeeded.")

-- This has been left as an effective no - op for now as it currently doesn't appear likely to be useful.
set_region game_state args = (Nothing, "set_region called.")

set_difficulty game_state args = (Just game_state {s1_ = (s1_ game_state) {difficulty = Encode_status.set_difficulty (read (args !! 0))}}, "set_difficulty succeeded.")

set_verbose_mode game_state args =
  let mode = read (args !! 0)
  in if mode < 0 || mode > 1 then (Nothing, "set_verbose_mode failed.  Arguments passed were mode: " ++ (args !! 0))
  else (Just game_state {s1_ = (s1_ game_state) {verbose_mode = int_to_bool mode}}, "set_verbose_mode succeeded.  Arguments passed were mode: " ++ (args !! 0))

set_story_state game_state args = (Just game_state {s1_ = (s1_ game_state) {story_state = read (args !! 0)}}, "set_story_state succeeded.")

-- These are the pages used in the hierarchical dictionary look up used to interpret console input.
page0 = ["unlock", "set", "sendsignal"]
page1 = ["wallgrid", "floorgrid", "objgrid", "playstate0", "playstate1"]
page2 = ["structure", "textures", "objplace"]
page3 = ["position", "angle", "rendmode"]
page4 = ["health", "ammo", "gems", "torches", "keys", "region", "difficulty", "verbosemode", "storystate"]

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

obj_grid_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_obj_grid}

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

keys_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just Handle_input.set_keys}

region_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_region}

difficulty_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just Handle_input.set_difficulty}

verbose_mode_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_verbose_mode}

story_state_node = Comm_struct {dictionary_page = [], branches = Nothing, update_game_state = Just set_story_state}

-- This function traverses the decision tree and thereby interprets console commands.
interpret_command :: [[Char]] -> Comm_struct -> Game_state -> (Game_state, [Char])
interpret_command [] comm_struct game_state = (game_state, "\nInvalid command.")
interpret_command (x:xs) comm_struct game_state =
  let new_game_state = (fromJust (update_game_state comm_struct)) game_state (x:xs)
      look_up = elemIndex x (dictionary_page comm_struct)
  in
  if isNothing (branches comm_struct) == True then
    if isNothing (fst new_game_state) == True then (game_state, "\n" ++ snd new_game_state)
    else (fromJust (fst new_game_state), "\n" ++ snd new_game_state)
  else
    if isNothing look_up == True then (game_state, "\nInvalid command.")
    else interpret_command xs ((fromJust (branches comm_struct)) ! (fromJust look_up)) game_state

char_list = "abcdefghijklmnopqrstuvwxyz0123456789 "

find_tile :: [Char] -> Char -> Int -> Int
find_tile [] t i = (i + 1)
find_tile (x:xs) t i =
  if x == t then (i + 1)
  else find_tile xs t (i + 1)

conv_msg_ :: [Char] -> [Char] -> [Int]
conv_msg_ [] c_list = []
conv_msg_ (x:xs) c_list = find_tile c_list x 0 : conv_msg_ xs c_list

back_space :: [Char] -> [Char]
back_space [] = []
back_space (x:xs) = init (x:xs)

console_front :: Game_state -> Io_box -> [Char] -> [Char] -> IO Game_state
console_front game_state io_box current_comm msg =
  let result = interpret_command (splitOn " " current_comm) base_node game_state
  in do
  swapBuffers
  threadDelay 16667
  mainLoopEvent
  control <- readIORef (control_ io_box)
  writeIORef (control_ io_box) 0
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  glBindVertexArray (unsafeCoerce ((fst (p_bind_ io_box)) ! 1027))
  glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst (p_bind_ io_box)) ! 1028))
  glUseProgram (unsafeCoerce ((fst (p_bind_ io_box)) ! ((snd (p_bind_ io_box)) - 3)))
  glUniform1i (fromIntegral ((uniform_ io_box) ! 38)) 0
  p_tt_matrix <- mallocBytes (glfloat * 16)
  load_array [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1] p_tt_matrix 0
  glUniformMatrix4fv (fromIntegral ((uniform_ io_box) ! 36)) 1 1 p_tt_matrix
  glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT zero_ptr
  free p_tt_matrix
  show_text (37 : (conv_msg_ msg char_list)) 0 959 (uniform_ io_box) (p_bind_ io_box) (-0.75) (-0.75) zero_ptr
  show_text (37 : (conv_msg_ current_comm char_list)) 0 959 (uniform_ io_box) (p_bind_ io_box) (-0.75) (-0.8) zero_ptr
  if control == 0 then console_front game_state io_box current_comm msg
  else if control == 38 then do
    putStr ("\nCommand run: " ++ current_comm)
    console_front (fst result) io_box [] (snd result)
  else if control == 39 then console_front game_state io_box (back_space current_comm) msg
  else if control == 40 then return game_state
  else do
    if length current_comm > 49 then console_front game_state io_box current_comm msg
    else console_front game_state io_box (current_comm ++ [char_list !! (control - 1)]) msg

-- This function handles the drawing of message tiles (letters and numbers etc) that are used for in game messages and in menus.
show_text :: [Int] -> Int -> Int -> UArray Int Int32 -> (UArray Int Word32, Int) -> Float -> Float -> Ptr GLfloat -> IO ()
show_text [] mode base uniform p_bind x y p_tt_matrix = do
  glEnable GL_DEPTH_TEST
  free p_tt_matrix
show_text (m:ms) mode base uniform p_bind x y p_tt_matrix = do
  if minusPtr p_tt_matrix zero_ptr == 0 then do
    p_tt_matrix_ <- mallocBytes (16 * glfloat)
    glBindVertexArray (unsafeCoerce ((fst p_bind) ! 933))
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 3)))
    glDisable GL_DEPTH_TEST
    show_text (m:ms) mode base uniform p_bind x y p_tt_matrix_
  else do
    load_array (MAT.toList (translation x y 0)) (castPtr p_tt_matrix) 0
    if mode == 0 && m < 83 then do
      glUniformMatrix4fv (unsafeCoerce (uniform ! 36)) 1 1 p_tt_matrix
      glUniform1i (unsafeCoerce (uniform ! 38)) 0
    else if mode == 1 && m < 83 then do
      glUniformMatrix4fv (unsafeCoerce (uniform ! 36)) 1 1 p_tt_matrix
      glUniform1i (unsafeCoerce (uniform ! 38)) 1
    else show_text ms mode base uniform p_bind x y p_tt_matrix
    glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst p_bind) ! (base + m)))
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT zero_ptr
    show_text ms mode base uniform p_bind (x + 0.04) y p_tt_matrix

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
  else if key == key_set ! 12 then writeIORef ref 14  -- Enter console mode
  else writeIORef ref 0

-- When the engine is in console input mode, these are the callbacks that GLUT calls each time mainLoopEvent has been called and there is keyboard input
-- in the window message queue.
get_console_input0 :: IORef Int -> Array Int Char -> Int -> Char -> Position -> IO ()
get_console_input0 ref key_table i key pos = do
  if i > 36 then writeIORef ref 0
  else if key_table ! i == key then writeIORef ref (i + 1)
  else get_console_input0 ref key_table (i + 1) key pos

get_console_input1 :: IORef Int -> SpecialKey -> Position -> IO ()
get_console_input1 ref s_key pos = do
  if s_key == KeyF4 then writeIORef ref 38                -- Run current command
  else if s_key == KeyLeft then writeIORef ref 39         -- Backspace
  else if s_key == KeyF6 then writeIORef ref 40           -- Exit console mode
  else writeIORef ref 0
