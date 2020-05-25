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
import Build_model

-- This recursive data type is used to implement the control structure that updates the game state in response to console input.
data Comm_struct = Comm_struct {page_dictionary :: [[Char]], branches :: Array Int Comm_struct, update_game_state :: Game_state -> [Char] -> Game_state}

-- These are the pages used in the hierarchical dictionary look up used to interpret console input.
page0 = ["unlock", "set", "send_signal"]
page1 = ["Wall_grid", "Floor_grid", "Obj_grid", "Play_state0", "Play_State1"]
page2 = ["structure", "textures", "Obj_place"]
page3 = ["position", "angle", "rend_mode"]
page4 = ["health", "ammo", "gems", "torches", "keys", "region", "difficulty", "verbose_mode", "story_state"]

--wall_grid_branch = Comm_struct {}

--branches_node0_1 = array (0, 4) [(0, Comm_struct {page_dictionary = }

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


