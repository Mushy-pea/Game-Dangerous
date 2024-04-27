-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- Below is an example of a valid configuration file (when uncommented) for engine version 1.0.0.

-- map_file_path=C:\Users\steve\Software-Projects\GPLC-scripts-and-maps\Maps\
-- map_file=map1_saved.dan
-- game_save_path=C:\Users\steve\code\GD_release_4_Windows\Saved_games
-- shader_file=shaders.glsl
-- init_u=23.5
-- init_v=18.5
-- init_w=0.2
-- gravity=-1.5
-- friction=-0.3
-- run_power=180
-- jump_power=1.5
-- verbose_mode=n
-- model_data_dir=C:\Users\steve\code\GD_release_4_Windows\Models\
-- splash_image=off
-- u_limit=39
-- v_limit=53
-- w_limit=2
-- sound_data_dir=C:\Users\steve\code\GD_release_4_Windows\Sounds\
-- prob_c=0
-- frustumScale1=1.25
-- survey_start=empty
-- survey_size=empty
-- version_and_platform_string=Windows x86_64 Engine version: 1.0.0 Server version: 2.0.0 (commit 59859b1) 14/04/2024
-- cb_PAUSE=x
-- cb_FORWARD=w
-- cb_BACK=s
-- cb_STRAFE_LEFT=a
-- cb_STRAFE_RIGHT=d
-- cb_TURN_LEFT=k
-- cb_TURN_RIGHT=l
-- cb_JUMP=u
-- cb_LIGHT_TORCH=t
-- cb_SWITCH_VIEW=c
-- cb_ROTATE_VIEW=v
-- cb_FIRE= 
-- cb_MENU_SELECT=1
-- cb_MENU_BACK=2
-- cb_MENU_HOME=3
-- resolution_x=1920
-- resolution_y=1080
-- min_frame_t=14285714
-- on_screen_metrics=low
-- music=off
-- music_period=15040
-- config_file=config.txt
-- map_unlock_code=CA646EC63000B200E800960100135000
-- map_id=13082023
-- map_file_version=10

-- This module is a launcher for the engine.  Users can change engine settings relative to those in a given configuration file 
-- through an interactive text prompt.  This is implemented by the launcher passing command line arguments to the engine that 
-- override the defaults in the configuration file.

module Main where

import System.IO
import System.Process
import BuildModel

main = do
  contents <- bracket (openFile "config.txt" ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\ncfg file size: " ++ show (length c)); return c)
  putStr "\nThanks for choosing to play Game :: Dangerous!"
  

