module PauseMenu where

import Data.Array.IArray
import BuildModel

-- This function is used to convert integers to their representation in the engine's message tile reference format.
convMsg :: Int -> [Int]
convMsg v0 =
  let v1 = mod v0 100
  in
  if v0 < 10 then [(mod v0 10) + 53]
  else if v0 < 100 then [(div v0 10) + 53, (mod v0 10) + 53]
  else [(div v0 100) + 53, (div v1 10) + 53, (mod v1 10) + 53]

showGameTime :: Int -> [Char] -> Bool -> [Char]
showGameTime t result True = reverse (take 6 (reverse ("00000" ++ result)))
showGameTime t result False =
  if t < 400 then showGameTime 0 (result ++ "0" ++ show (div t 40)) True
  else if t < 2400 then showGameTime 0 (result ++ show (div t 40)) True
  else if t < 24000 then showGameTime (mod t 2400) (result ++ "0" ++ show (div t 2400)) False
  else if t < 144000 then showGameTime (mod t 2400) (result ++ show (div t 2400)) False
  else if t < 146400 then showGameTime (mod t 144000) (show (div t 144000) ++ "00") False
  else showGameTime (mod t 144000) (show (div t 144000)) False

padProgram :: Int -> [Int]
padProgram c
  | c == 0 = []
  | otherwise = 63 : padProgram (c - 1)

-- pauseMenuScript and pauseMenu modify the following GPLC program and hot swap it into a loaded map at runtime.

-- PauseMenu
-- ~
-- w = 0 
--  u = 0 
--  v = 2 
--  msgLength = 301 
--  SaveGame = 3 
--  ReturnMainMenu = 4 
--  ExitGame = 5 
--  ~ 
--  --signal 16 
--  pass_msg 303 msgLength 04 00 00 02 -1 00 07 01 13 05 63 16 01 
--  21 19 05 04 63 63 63 63 63 63 63 63 63 63 63 63 
--  -1 00 63 63 63 63 63 63 63 63 63 63 63 63 63 63 
--  -1 00 08 31 27 38 46 34 69 63 63 63 63 63 63 63 
--  -1 00 01 39 39 41 69 63 63 63 63 63 63 63 63 63 
--  -1 00 07 31 39 45 69 63 63 63 63 63 63 63 63 63 
--  -1 00 20 41 44 29 34 31 45 69 63 63 63 63 63 63 
--  -1 00 11 31 51 45 69 63 63 63 63 63 63 63 63 63 
--  63 63 63 63 63 63 63 63 63 63 63 63 63 63 63 63 
--  -1 00 63 63 63 63 63 63 63 63 63 63 63 63 63 63 
--  -1 00 25 41 47 65 44 31 63 42 38 27 51 35 40 33 
--  69 63 63 63 63 63 63 63 63 63 63 63 63 63 63 63 
--  -1 00 20 35 39 31 69 63 63 63 63 63 63 63 63 63 
--  -1 00 63 63 63 63 63 63 63 63 63 63 63 63 63 63 
--  -1 01 18 31 45 47 39 31 63 63 63 63 63 63 63 63 
--  -1 02 19 27 48 31 63 33 27 39 31 63 63 63 63 63 
--  -1 03 18 31 46 47 44 40 63 46 41 63 39 27 35 40 
--  63 39 31 40 47 63 63 63 63 63 63 63 63 63 63 63 
--  -1 04 05 50 35 46 63 63 63 63 63 63 63 63 63 63 
--  --signal 3 
--  set_event_context SaveGame 
--  --signal 4 
--  set_event_context ReturnMainMenu 
--  --signal 5 
--  set_event_context ExitGame

pauseMenuScript :: Play_state0 -> Play_state1 -> [Int]
pauseMenuScript s0 s1 =
  let proc_time = \(x0:x1:x2:x3:x4:x5:xs) -> [read [x0] + 53, read [x1] + 53, 69, read [x2] + 53, read [x3] + 53, 69, read [x4] + 53, read [x5] + 53]
      header = [0, 0, 16, 0, 303, 3, 303, 2, 4, 305, 2, 5, 307, 2, 536870911, 13, 3, 4, 0, 0, 2]
      game_paused = [-1, 0, 7, 1, 13, 5, 63, 16, 1, 21, 19, 5, 4, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63]
      blank_line = [-1, 0, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63]
      health_ = [-1, 0, 8, 31, 27, 38, 46, 34, 69, 63] ++ convMsg (health s1)
      ammo_ = [-1, 0, 1, 39, 39, 41, 69, 63] ++ convMsg (ammo s1)
      gems_ = [-1, 0, 7, 31, 39, 45, 69, 63] ++ convMsg (gems s1)
      torches_ = [-1, 0, 20, 41, 44, 29, 34, 31, 45, 69, 63] ++ convMsg (torches s1)
      keys_ = [-1, 0, 11, 31, 51, 45, 69, 63] ++ keys s1
      playing = [-1, 0, 25, 41, 47, 65, 44, 31, 63, 42, 38, 27, 51, 35, 40, 33, 69, 63] ++ playerClass s1
      time = [-1, 0, 20, 35, 39, 31, 69, 63] ++ proc_time (showGameTime (fst__ (gameClock s0)) [] False)
      resume = [-1, 1, 18, 31, 45, 47, 39, 31, 63, 63, 63, 63, 63, 63, 63, 63]
      save_game = [-1, 2, 19, 27, 48, 31, 63, 33, 27, 39, 31, 63, 63, 63, 63, 63]
      return_main_menu = [-1, 3, 18, 31, 46, 47, 44, 40, 63, 46, 41, 63, 39, 27, 35, 40, 63, 39, 31, 40, 47, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63]
      exit_game = [-1, 4, 5, 50, 35, 46, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63]
      footer = [24, 4, 24, 5, 24, 6, 536870911, 0, 0, 2, 301, 3, 4, 5]
      program = header ++ game_paused ++ blank_line ++ health_ ++ ammo_ ++ gems_ ++ torches_ ++ keys_ ++ blank_line ++ playing ++ time ++ blank_line ++ resume
                ++ save_game ++ return_main_menu ++ exit_game
  in program ++ padProgram (318 - (length program)) ++ footer

pauseMenu :: Game_state -> Array (Int, Int, Int) Obj_grid
pauseMenu game_state =
  let pause_menu_voxel = Obj_grid {objType = 3, program = pauseMenuScript (s0_ game_state) (s1_ game_state), programName = "PauseMenu"}
  in (obj_grid_ game_state) // [((0, 0, 2), pause_menu_voxel)]

