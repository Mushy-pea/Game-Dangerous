-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- The engine has three modes of receiving user input, namely interactive (during game play), menu and console.  The user interface code that implements each of
-- these modes is in this module.  It is anticipated that the console will be mainly used for testing purposes, although it also plays a role in the level
-- unlock system.

module Handle_input where

import Graphics.GL.Core33
import Graphics.UI.GLUT
import Data.IORef

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

get_console_input :: IORef Int -> Char -> Position -> IO ()
get_console_input ref key pos = do
  
