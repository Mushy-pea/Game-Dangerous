-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module Main where

import Prelude hiding ((!!))
import Index_wrapper
import System.IO
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Graphics.GL.Core33
import Graphics.UI.GLUT hiding (Object, Matrix, GLuint, GLchar, GLenum, GLsizei, GLushort, GLsizeiptr, GLfloat)
import Data.Bits
import Data.Word
import Data.List.Split
import Data.Matrix hiding ((!))
import Data.Binary hiding (get)
import qualified Data.Sequence as SEQ
import Data.IORef
import qualified Data.List as LS
import System.Environment
import Data.Coerce
import Unsafe.Coerce
import Data.Array.IArray
import Data.Array.Unboxed
import Control.Concurrent
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Either
import qualified Data.Foldable as FOLD
import Control.Exception
import System.Exit
import System.Random
import Build_model
import Decompress_map
import Game_logic
import Game_sound

main = do
  args <- getArgs
  if length args == 0 then do
    contents <- bracket (openFile "config.txt" ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nconfig file size: " ++ show (length contents)); return contents)
    open_window (listArray (0, 81) (splitOneOf "=\n" contents))
  else do
    contents <- bracket (openFile ((args, 1) !! 0) ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nconfig file size: " ++ show (length contents)); return contents)
    open_window (listArray (0, 81) (splitOneOf "=\n" contents))

-- This function initialises the GLUT runtime system, which in turn is used to initialise a window and OpenGL context.
open_window :: Array Int [Char] -> IO ()
open_window conf_reg =
  let cfg' = cfg conf_reg 0
      cb = \x -> head (cfg' x)
      key_set = listArray (0, 14) [cb "cb_PAUSE", cb "cb_FORWARD", cb "cb_STRAFE_RIGHT", cb "cb_BACK", cb "cb_STRAFE_LEFT", cb "cb_TURN_LEFT", cb "cb_TURN_RIGHT", cb "cb_JUMP", cb "cb_LIGHT_TORCH", cb "cb_SWITCH_VIEW", cb "cb_ROTATE_VIEW", cb "cb_FIRE", cb "cb_MENU_SELECT", cb "cb_MENU_BACK", cb "cb_MENU_HOME"]
  in do
  putStr ("\n\nGame :: Dangerous engine " ++ cfg' "version_and_platform_string")
  putStr "\nInitialising GLUT and OpenGL runtime environment..."
  initialize "game_dangerous.exe" []
  initialContextVersion $= (3, 3)
  screenRes <- newIORef (Size 0 0)
  if cfg' "resolution_x" == "auto" then do
    screen_res <- get screenSize
    initialWindowSize $= screen_res
    writeIORef screenRes screen_res
  else do
    initialWindowSize $= (Size (read (cfg' "resolution_x")) (read (cfg' "resolution_y")))
    writeIORef screenRes (Size (read (cfg' "resolution_x")) (read (cfg' "resolution_y")))
  initialDisplayMode $= [RGBAMode, WithAlphaComponent, WithDepthBuffer, DoubleBuffered]
  createWindow "Game :: Dangerous"
  actionOnWindowClose $= Exit
  displayCallback $= repaint_window
  control_ref <- newIORef 0
  keyboardCallback $= (Just (get_input control_ref key_set))
  contents <- bracket (openFile (cfg' "map_file") ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nmap file size: " ++ show (length contents)); return contents)
  screen_res <- readIORef screenRes
  setup_game contents conf_reg screen_res control_ref

-- This is the callback that GLUT calls when it detects a window repaint is necessary.  This should only happen when the window is first opened, the user moves or resizes the window, or it is
-- overlapped by another window.  For standard frame rendering, show_frame and run_menu repaint the rendered area of the window.
repaint_window :: IO ()
repaint_window = do
  swapBuffers

-- This is the callback that GLUT calls each time mainLoopEvent has been called and there is keyboard input in the window message queue.
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

-- This function initialises the OpenGL and OpenAL contexts.  It also decompresses the map file, manages the compilation of GLSL shaders, loading of 3D models, loading of the light map
-- and loading of sound effects.
setup_game :: [Char] -> Array Int [Char] -> Size -> IORef Int -> IO ()
setup_game comp_env_map conf_reg (Size w h) control_ref =
  let m0 = "mod_to_world"
      m1 = "world_to_clip"
      m2 = "world_to_mod"
      lm0 = "lmap_pos0"
      lm1 = "lmap_pos1"
      lm2 = "lmap_int0"
      lm3 = "lmap_int1"
      lm4 = "lmap_t0"
      lm5 = "lmap_t1"
      dl0 = "mobileLightIntensities"
      dl1 = "mobileLightPositions"
      dl2 = "numLights"
      proc_map' = proc_map (splitOn "\n~\n" comp_env_map) (read (((splitOn "\n~\n" comp_env_map), 3) !! 12)) (read (((splitOn "\n~\n" comp_env_map), 4) !! 13)) (read (((splitOn "\n~\n" comp_env_map), 5) !! 14))
      env_map = ".~.~.~.~" ++ fst (proc_map') ++ "~" ++ snd (proc_map') ++ (((splitOn "\n~\n" comp_env_map), 6) !! 10) ++ "~" ++ (((splitOn "\n~\n" comp_env_map), 7) !! 11) ++ "~" ++ (((splitOn "\n~\n" comp_env_map), 8) !! 12) ++ "~" ++ (((splitOn "\n~\n" comp_env_map), 9) !! 13) ++ "~" ++ (((splitOn "\n~\n" comp_env_map), 10) !! 14)
      cfg' = cfg conf_reg 0
      p_bind_limit = (read (((splitOn "\n~\n" comp_env_map), 11) !! 7)) - 1
      frustumScale0 = (read (cfg' "frustumScale1")) / (fromIntegral w / fromIntegral h)
  in do
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LEQUAL
  glDepthRange 0 1
  glEnable GL_DEPTH_CLAMP
  contents0 <- bracket (openFile (cfg' "shader_file") ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nshader file size: " ++ show (length contents)); return contents)
  p_gl_program <- mallocBytes (7 * gluint)
  make_gl_program (tail (splitOn "#" contents0)) p_gl_program 0
  uniform <- find_gl_uniform [m0, m1, m2, lm0, lm1, lm2, lm3, lm4, lm5, "t", "mode", m0, m1, m2, lm0, lm1, lm2, lm3, lm4, lm5, "t", "mode", "tex_unit0", m0, m1, m2, "worldTorchPos", "timer", "mode", m0, m1, m2, "worldTorchPos", "timer", "mode", "tex_unit0", "tt_matrix", "tex_unit0", "mode", m0, m1, m2, "normal_transf", lm0, lm1, lm2, lm3, lm4, lm5, "tex_unit0", "t", m0, m1, "tex_unit0", dl0, dl1, dl2, dl0, dl1, dl2, dl0, dl1, dl2, dl0, dl1, dl2] [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3] p_gl_program []
  gl_program0 <- peekElemOff p_gl_program 0; gl_program1 <- peekElemOff p_gl_program 1; gl_program2 <- peekElemOff p_gl_program 2; gl_program3 <- peekElemOff p_gl_program 3; gl_program4 <- peekElemOff p_gl_program 4; gl_program5 <- peekElemOff p_gl_program 5; gl_program6 <- peekElemOff p_gl_program 6
  glClearColor 0 0 0 0
  glClearDepth 1
  contents1 <- bracket (openFile ((cfg' "model_data_dir") ++ (((splitOn "\n~\n" comp_env_map), 12) !! 9)) ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nlight map file size: " ++ show (length contents)); return contents)
  p_lmap_pos0 <- callocBytes (glfloat * 300)
  p_lmap_pos1 <- callocBytes (glfloat * 300)
  p_lmap_int0 <- callocBytes (glfloat * 300)
  p_lmap_int1 <- callocBytes (glfloat * 300)
  p_lmap_t0 <- callocBytes (glfloat * 240)
  p_lmap_t1 <- callocBytes (glfloat * 240)
  load_array (take 300 (proc_floats (splitOn ", " (((splitOn "\n~\n" contents1), 13) !! 0)))) p_lmap_pos0 0
  load_array (take 300 (proc_floats (splitOn ", " (((splitOn "\n~\n" contents1), 14) !! 1)))) p_lmap_pos1 0
  load_array (take 300 (proc_floats (splitOn ", " (((splitOn "\n~\n" contents1), 15) !! 2)))) p_lmap_int0 0
  load_array (take 300 (proc_floats (splitOn ", " (((splitOn "\n~\n" contents1), 16) !! 3)))) p_lmap_int1 0
  load_array (take 240 (proc_floats (splitOn ", " (((splitOn "\n~\n" contents1), 17) !! 4)))) p_lmap_t0 0
  load_array (take 240 (proc_floats (splitOn ", " (((splitOn "\n~\n" contents1), 18) !! 5)))) p_lmap_t1 0
  glUseProgram gl_program0
  glUniform3fv (fromIntegral ((uniform, 19) !! 3)) 100 (castPtr p_lmap_pos0)
  glUniform3fv (fromIntegral ((uniform, 20) !! 4)) 100 (castPtr p_lmap_pos1)
  glUniform3fv (fromIntegral ((uniform, 21) !! 5)) 100 (castPtr p_lmap_int0)
  glUniform3fv (fromIntegral ((uniform, 22) !! 6)) 100 (castPtr p_lmap_int1)
  glUniform1fv (fromIntegral ((uniform, 23) !! 7)) 240 (castPtr p_lmap_t0)
  glUniform1fv (fromIntegral ((uniform, 24) !! 8)) 240 (castPtr p_lmap_t1)
  glUseProgram gl_program1
  glUniform1i (fromIntegral ((uniform, 25) !! 22)) 0
  glUniform3fv (fromIntegral ((uniform, 26) !! 14)) 100 (castPtr p_lmap_pos0)
  glUniform3fv (fromIntegral ((uniform, 27) !! 15)) 100 (castPtr p_lmap_pos1)
  glUniform3fv (fromIntegral ((uniform, 28) !! 16)) 100 (castPtr p_lmap_int0)
  glUniform3fv (fromIntegral ((uniform, 29) !! 17)) 100 (castPtr p_lmap_int1)
  glUniform1fv (fromIntegral ((uniform, 30) !! 18)) 240 (castPtr p_lmap_t0)
  glUniform1fv (fromIntegral ((uniform, 31) !! 19)) 240 (castPtr p_lmap_t1)
  glUseProgram gl_program3
  glUniform1i (fromIntegral ((uniform, 32) !! 35)) 0
  glUseProgram gl_program4
  glUniform1i (fromIntegral ((uniform, 33) !! 37)) 0
  glUseProgram gl_program5
  glUniform1i (fromIntegral ((uniform, 34) !! 49)) 0
  glUniform3fv (fromIntegral ((uniform, 35) !! 43)) 100 (castPtr p_lmap_pos0)
  glUniform3fv (fromIntegral ((uniform, 36) !! 44)) 100 (castPtr p_lmap_pos1)
  glUniform3fv (fromIntegral ((uniform, 37) !! 45)) 100 (castPtr p_lmap_int0)
  glUniform3fv (fromIntegral ((uniform, 38) !! 46)) 100 (castPtr p_lmap_int1)
  glUniform1fv (fromIntegral ((uniform, 39) !! 47)) 240 (castPtr p_lmap_t0)
  glUniform1fv (fromIntegral ((uniform, 40) !! 48)) 240 (castPtr p_lmap_t1)
  glUseProgram gl_program6
  glUniform1i (fromIntegral ((uniform, 41) !! 53)) 0
  putStr "\nCompiling OpenGL shader programs..."
  validate_prog gl_program0 0
  validate_prog gl_program1 1
  validate_prog gl_program2 2
  validate_prog gl_program3 3
  validate_prog gl_program4 4
  validate_prog gl_program5 5
  validate_prog gl_program6 6
  p_bind <- buffer_to_array (castPtr p_gl_program) (array (0, p_bind_limit) [(x, 0) | x <- [0..p_bind_limit]]) 0 (p_bind_limit - 6) 6
  putStr "\nLoading 3D models..."
  mod_bind <- callocBytes ((read (((splitOn "\n~\n" comp_env_map), 42) !! 7)) * gluint)
  load_mod_file (init (splitOn ", " (((splitOn "\n~\n" comp_env_map), 43) !! 8))) (cfg' "model_data_dir") mod_bind
  free p_gl_program; free p_lmap_pos0; free p_lmap_pos1; free p_lmap_int0; free p_lmap_int1; free p_lmap_t0; free p_lmap_t1
  p_bind_ <- buffer_to_array (castPtr mod_bind) p_bind 0 0 (p_bind_limit - 16)
  init_al_context
  contents2 <- bracket (openFile ((cfg' "sound_data_dir") ++ (last (splitOn ", " (((splitOn "\n~\n" comp_env_map), 44) !! 8)))) ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nsound map size: " ++ show (length contents)); return contents)
  sound_array <- init_al_effect0 (splitOneOf "\n " contents2) (cfg' "sound_data_dir") (array (0, 255) [(x, Source 0) | x <- [0..255]])
  start_game control_ref (listArray (0, 65) uniform) (p_bind_, p_bind_limit + 1) env_map conf_reg (-1) (read (cfg' "init_u"), read (cfg' "init_v"), read (cfg' "init_w"), read (cfg' "gravity"), read (cfg' "friction"), read (cfg' "run_power"), read (cfg' "jump_power")) def_save_state sound_array frustumScale0

-- The model file(s) that describe all 3D and 2D models referenced in the current map are loaded here.
load_mod_file :: [[Char]] -> [Char] -> Ptr GLuint -> IO ()
load_mod_file [] path p_bind = return ()
load_mod_file (x:xs) path p_bind = do
  h <- openFile (path ++ x) ReadMode
  mod_data <- hGetContents h
  if (((splitOn "~" mod_data), 45) !! 0) == [] then setup_object (load_object0 (splitOn "&" (((splitOn "~" mod_data), 46) !! 1))) (proc_marker (proc_ints (splitOn ", " (((splitOn "~" mod_data), 47) !! 2)))) (proc_floats (splitOn ", " (((splitOn "~" mod_data), 48) !! 3))) (proc_elements (splitOn ", " (((splitOn "~" mod_data), 49) !! 4))) [] p_bind
  else do
    bs_tex <- load_bitmap0 (splitOn ", " (((splitOn "~" mod_data), 50) !! 0)) (load_object0 (splitOn "&" (((splitOn "~" mod_data), 51) !! 1))) path [] 1
    setup_object (load_object0 (splitOn "&" (((splitOn "~" mod_data), 52) !! 1))) (proc_marker (proc_ints (splitOn ", " (((splitOn "~" mod_data), 53) !! 2)))) (proc_floats (splitOn ", " (((splitOn "~" mod_data), 54) !! 3))) (proc_elements (splitOn ", " (((splitOn "~" mod_data), 55) !! 4))) bs_tex p_bind
  hClose h
  load_mod_file xs path p_bind

-- Functions used by start_game as part of game initialisation.
select_metric_mode "none" = 0
select_metric_mode "low" = 1
select_metric_mode "high" = 2

select_verbose_mode "y" = True
select_verbose_mode "n" = False

gen_prob_seq :: RandomGen g => Int -> Int -> Int -> g -> UArray Int Int
gen_prob_seq i0 i1 i2 g = listArray (i0, i1) (drop i2 (randomRs (0, 99) g))

-- This function initialises the game logic thread each time a new game is started and handles user input from the main menu.
start_game :: IORef Int -> UArray Int Int32 -> (UArray Int Word32, Int) -> [Char] -> Array Int [Char] -> Int -> (Float, Float, Float, Float, Float, Float, Float) -> Save_state -> Array Int Source -> Float -> IO ()
start_game control_ref uniform p_bind c conf_reg mode (u, v, w, g, f, mag_r, mag_j) save_state sound_array frustumScale0 =
  let u_limit = (read (((splitOn "~" c), 56) !! 8))
      v_limit = (read (((splitOn "~" c), 57) !! 9))
      w_limit = (read (((splitOn "~" c), 58) !! 10))
      w_grid = check_map_layer (-3) 0 0 u_limit v_limit (make_array0 ((build_table0 (elems (build_table1 (splitOn ", " (((splitOn "~" c), 59) !! 7)) (empty_w_grid u_limit v_limit w_limit) 7500)) u_limit v_limit w_limit) ++ (sort_grid0 (splitOn "&" (((splitOn "~" c), 60) !! 4)))) u_limit v_limit w_limit) w_grid_flag
      f_grid = check_map_layer 0 0 0 ((div (u_limit + 1) 2) - 1) ((div (v_limit + 1) 2) - 1) (make_array1 (load_floor0 (splitOn "&" (((splitOn "~" c), 61) !! 5))) ((div (u_limit + 1) 2) - 1) ((div (v_limit + 1) 2) - 1) w_limit) f_grid_flag
      obj_grid = check_map_layer 0 0 0 u_limit v_limit (empty_obj_grid u_limit v_limit w_limit // load_obj_grid (splitOn ", " (((splitOn "~" c), 62) !! 6))) obj_grid_flag
      look_up_ = look_up [make_table 0 0, make_table 1 0, make_table 2 0, make_table 3 0]
      camera_to_clip = fromList 4 4 [frustumScale0, 0, 0, 0, 0, read (cfg' "frustumScale1"), 0, 0, 0, 0, ((zFar + zNear) / (zNear - zFar)), ((2 * zFar * zNear) / (zNear - zFar)), 0, 0, -1, 0]
      cfg' = cfg conf_reg 0
  in do
  if mode == -1 then do
    if cfg' "splash_image" /= "null" then do
      glDisable GL_DEPTH_TEST
      glBindVertexArray (unsafeCoerce ((fst p_bind) ! 1017))
      glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst p_bind) ! 1019))
      glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 3)))
      glUniform1i (fromIntegral (uniform ! 38)) 0
      p_tt_matrix <- mallocBytes (glfloat * 16)
      load_array [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1] p_tt_matrix 0
      glUniformMatrix4fv (fromIntegral (uniform ! 36)) 1 1 p_tt_matrix
      glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT zero_ptr
      swapBuffers
      free p_tt_matrix
      threadDelay 5000000
      glEnable GL_DEPTH_TEST
      start_game control_ref uniform p_bind c conf_reg 2 (u, v, w, g, f, mag_r, mag_j) save_state sound_array frustumScale0
    else start_game control_ref uniform p_bind c conf_reg 0 (u, v, w, g, f, mag_r, mag_j) save_state sound_array frustumScale0
  else if mode == 0 || mode == 1 then do
    p_mt_matrix <- mallocBytes (glfloat * 128)
    p_f_table0 <- callocBytes (int_ * 120000)
    p_f_table1 <- callocBytes (int_ * 37500)
    p_light_buffer <- mallocBytes (glfloat * 35)
    state_ref <- newEmptyMVar
    t_log <- newEmptyMVar
    r_gen <- getStdGen
    if mode == 0 then do
      tid <- forkIO (update_play (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = control_ref}) state_ref (ps0_init {pos_u = u, pos_v = v, pos_w = w, on_screen_metrics = select_metric_mode (cfg' "on_screen_metrics"), prob_seq = gen_prob_seq 0 239 (read (cfg' "prob_c")) r_gen}) (ps1_init {verbose_mode = select_verbose_mode (cfg' "verbose_mode")}) False (read (cfg' "min_frame_t")) (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up_ save_state sound_array 0 t_log (SEQ.empty) 60)
      result <- show_frame p_bind uniform (p_mt_matrix, p_light_buffer) (p_f_table0, p_f_table1) 0 0 0 0 0 state_ref w_grid f_grid obj_grid look_up_ camera_to_clip (array (0, 5) [(i, (0, [])) | i <- [0..5]])
      free p_mt_matrix
      free p_f_table0
      free p_f_table1
      free p_light_buffer
      killThread tid
      save_array_diff0 (is_set (snd result)) 0 ([], []) (wrapped_save_array_diff1 (gen_array_diff (-3) 0 0 u_limit v_limit w_grid (w_grid_ (snd result)) SEQ.empty)) (wrapped_save_array_diff1 (gen_array_diff 0 0 0 ((div (u_limit + 1) 2) - 1) ((div (v_limit + 1) 2) - 1) f_grid (f_grid_ (snd result)) SEQ.empty)) (wrapped_save_array_diff1 (gen_array_diff 0 0 0 u_limit v_limit obj_grid (obj_grid_ (snd result)) SEQ.empty)) (label_play_state_encoding (encode (s0_ (snd result)))) (label_play_state_encoding (encode (s1_ (snd result)))) conf_reg (s0_ (snd result))
      start_game control_ref uniform p_bind c conf_reg ((head (fst result)) + 1) (u, v, w, g, f, mag_r, mag_j) (snd result) sound_array frustumScale0
    else do
      tid <- forkIO (update_play (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = control_ref}) state_ref (s0_ save_state) (s1_ save_state) False (read (cfg' "min_frame_t")) (g, f, mag_r, mag_j) (w_grid_ save_state) (f_grid_ save_state) (obj_grid_ save_state) look_up_ save_state sound_array 0 t_log (SEQ.empty) 60)
      result <- show_frame p_bind uniform (p_mt_matrix, p_light_buffer) (p_f_table0, p_f_table1) 0 0 0 0 0 state_ref w_grid f_grid obj_grid look_up_ camera_to_clip (array (0, 5) [(i, (0, [])) | i <- [0..5]])
      free p_mt_matrix
      free p_f_table0
      free p_f_table1
      free p_light_buffer
      killThread tid
      save_array_diff0 (is_set (snd result)) 0 ([], []) (wrapped_save_array_diff1 (gen_array_diff (-3) 0 0 u_limit v_limit w_grid (w_grid_ (snd result)) SEQ.empty)) (wrapped_save_array_diff1 (gen_array_diff 0 0 0 ((div (u_limit + 1) 2) - 1) ((div (v_limit + 1) 2) - 1) f_grid (f_grid_ (snd result)) SEQ.empty)) (wrapped_save_array_diff1 (gen_array_diff 0 0 0 u_limit v_limit obj_grid (obj_grid_ (snd result)) SEQ.empty)) (label_play_state_encoding (encode (s0_ (snd result)))) (label_play_state_encoding (encode (s1_ (snd result)))) conf_reg (s0_ (snd result))
      start_game control_ref uniform p_bind c conf_reg ((head (fst result)) + 1) (u, v, w, g, f, mag_r, mag_j) (snd result) sound_array frustumScale0
  else if mode == 2 then do
    choice <- run_menu main_menu_text [] (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = control_ref}) (-0.75) (-0.75) 1 0 0 ps0_init
    if choice == 1 then start_game control_ref uniform p_bind c conf_reg 0 (u, v, w, g, f, mag_r, mag_j) save_state sound_array frustumScale0
    else if choice == 2 then do
      contents <- bracket (openFile "save_log.log" ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nsave_log file size: " ++ show (length contents)); return contents)
      state_choice <- run_menu (gen_load_menu (tail (splitOn "\n" contents)) [] 1) [] (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = control_ref}) (-0.75) (-0.75) 1 0 0 ps0_init
      loaded_state <- load_saved_game 0 (tail (splitOn "\n" contents)) [] 1 state_choice (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = control_ref}) w_grid f_grid obj_grid conf_reg
      if isNothing loaded_state == True then start_game control_ref uniform p_bind c conf_reg 2 (u, v, w, g, f, mag_r, mag_j) def_save_state sound_array frustumScale0
      else start_game control_ref uniform p_bind c conf_reg 1 (u, v, w, g, f, mag_r, mag_j) (fromJust loaded_state) sound_array frustumScale0
    else exitSuccess
  else if mode == 3 then do
    if is_set save_state == True then start_game control_ref uniform p_bind c conf_reg 1 (u, v, w, g, f, mag_r, mag_j) save_state sound_array frustumScale0
    else start_game control_ref uniform p_bind c conf_reg 0 (u, v, w, g, f, mag_r, mag_j) save_state sound_array frustumScale0
  else if mode == 4 then exitSuccess
  else if mode == 6 then do
    putStr "\nYou have completed the demo.  Nice one.  Check the project website later for details of further releases."
    exitSuccess
  else return ()

-- This function determines the content of the load game menu that allows the user to load a previous game state.
gen_load_menu :: [[Char]] -> [(Int, [Int])] -> Int -> [(Int, [Int])]
gen_load_menu [] acc c =
  if acc == [] then no_game_states_header
  else load_game_menu_header ++ acc
gen_load_menu ((y0:y1:y2:y3:y4:y5:y6:y7:y8:y9:y10:y11:y12:y13:y14:y15:y16:ys):xs) acc c =
  if y0 == '_' then gen_load_menu xs (acc ++ [(c, game_state_text ++ [c + 53] ++ game_time_text ++ [(read [y11] + 53), (read [y12] + 53), 69, (read [y13] + 53), (read [y14] + 53), 69, (read [y15] + 53), (read [y16] + 53)])]) (c + 1)
  else gen_load_menu xs acc c

-- Constants used to fix the types decoded from save game files.
def_obj_place_ = ((0, 0, 0), Nothing) :: ((Int, Int, Int), Maybe Obj_place)
def_f_grid_ = ((0, 0, 0), def_f_grid) :: ((Int, Int, Int), Floor_grid)
def_obj_grid_ = ((0, 0, 0), def_obj_grid) :: ((Int, Int, Int), (Int, [Int]))

-- These three functions deal with loading a saved game state and recreating the game state by updating a base map state.
decode_sequence :: Binary a => Int -> a -> LBS.ByteString -> SEQ.Seq a -> SEQ.Seq a
decode_sequence c def bs diff_seq =
  let result = decodeOrFail bs
      tested_result = if (isLeft result) == True then error ("\ndecode_sequence: " ++ third_ (fromLeft (LBS.empty, 0, "def") result) ++ "\nblock offset: " ++ show c)
                      else fromRight (LBS.empty, 0, def) result
  in
  if LBS.length bs == 0 then diff_seq
  else decode_sequence (c + 1) def (fst__ tested_result) (diff_seq SEQ.>< SEQ.singleton (third_ tested_result))

proc_w_grid_upd :: SEQ.Seq ((Int, Int, Int), Maybe Obj_place) -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)]
proc_w_grid_upd SEQ.Empty w_grid = []
proc_w_grid_upd (x SEQ.:<| xs) w_grid = (fst x, (w_grid ! (fst x)) {obj = snd x}) : proc_w_grid_upd xs w_grid

load_game_state_file :: Int -> LBS.ByteString -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> SEQ.Seq ((Int, Int, Int), Maybe Obj_place) -> SEQ.Seq ((Int, Int, Int), Floor_grid) -> SEQ.Seq ((Int, Int, Int), (Int, [Int])) -> Play_state0 -> Play_state1 -> Save_state
load_game_state_file c bs w_grid f_grid obj_grid w_grid_upd f_grid_upd obj_grid_upd s0 s1 =
  if c == 0 then load_game_state_file (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid (decode_sequence 0 def_obj_place_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty) f_grid_upd obj_grid_upd s0 s1
  else if c == 1 then load_game_state_file (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid w_grid_upd (decode_sequence 0 def_f_grid_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty) obj_grid_upd  s0 s1
  else if c == 2 then load_game_state_file (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid w_grid_upd f_grid_upd (decode_sequence 0 def_obj_grid_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)  s0 s1
  else if c == 3 then load_game_state_file (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid w_grid_upd f_grid_upd obj_grid_upd ((decode (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs))) :: Play_state0) s1
  else if c == 4 then load_game_state_file (c + 1) LBS.empty w_grid f_grid obj_grid w_grid_upd f_grid_upd obj_grid_upd s0 ((decode (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs))) :: Play_state1)
  else Save_state {is_set = False, w_grid_ = w_grid // (proc_w_grid_upd w_grid_upd w_grid), f_grid_ = f_grid // (FOLD.toList f_grid_upd), obj_grid_ = obj_grid // (FOLD.toList obj_grid_upd), s0_ = s0, s1_ = s1}

-- If an error occurs while attempting to open a save game file the user is informed through the menu system.
loader_error :: SomeException -> Io_box -> IO LBS.ByteString
loader_error x box = do
  run_menu error_opening_file_text [] box (-0.75) (-0.75) 1 0 0 ps0_init
  putStr ("\nload_saved_game: " ++ show x)
  return LBS.empty

-- This function is the entry point to the game state saving logic and handles user input from the load game menu.
load_saved_game :: Int -> [[Char]] -> [Char] -> Int -> Int -> Io_box -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> Array Int [Char] -> IO (Maybe Save_state)
load_saved_game 0 [] chosen_file c choice box w_grid f_grid obj_grid conf_reg = error "\nload_saved_game: encountered an unexpected log file structure."
load_saved_game 0 ((y0:y1:y2:y3:y4:y5:y6:y7:y8:y9:y10:y11:y12:y13:y14:y15:y16:ys):xs) chosen_file c choice box w_grid f_grid obj_grid conf_reg = do
  if choice == 7 then return Nothing
  else if c == choice then load_saved_game 1 [] [y1, y2, y3, y4, y5, y6, y7, y8, y9] c choice box w_grid f_grid obj_grid conf_reg
  else load_saved_game 0 xs chosen_file (c + 1) choice box w_grid f_grid obj_grid conf_reg
load_saved_game 1 [] chosen_file c choice box w_grid f_grid obj_grid conf_reg = do
  contents <- catch (do contents <- LBS.readFile ((cfg conf_reg 0 "game_save_path") ++ chosen_file); return contents) (\e -> loader_error e box)
  if LBS.length contents == 0 then return Nothing
  else return (Just (load_game_state_file 0 contents w_grid f_grid obj_grid SEQ.Empty SEQ.Empty SEQ.Empty ps0_init ps1_init))

-- Sequential saves of the same game produce a sequence of save game files up to a preset maximum.  The automation of this feature is done in the two functions below.
add_time_stamp :: [[Char]] -> Play_state0 -> Int -> Int -> [Char]
add_time_stamp [] s0 save_slot i = []
add_time_stamp ((y0:y1:y2:y3:y4:y5:y6:y7:y8:y9:y10:y11:y12:y13:y14:y15:y16:ys):xs) s0 save_slot i =
  if i == save_slot then ['_', y1, y2, y3, y4, y5, y6, y7, y8, y9, y10] ++ show_game_time (mod (fst__ (game_clock s0)) 1440000) [] False ++ "\n" ++ add_time_stamp xs s0 save_slot (i + 1)
  else [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16] ++ "\n" ++ add_time_stamp xs s0 save_slot (i + 1)

select_save_file :: [[Char]] -> Play_state0 -> Int -> ([Char], [Char])
select_save_file file_list s0 limit =
  let i = read ((file_list, 631) !! 0)
  in
  if i == limit then (take 9 (tail ((file_list, 632) !! limit)), "1\n" ++ init (add_time_stamp (tail file_list) s0 i 1))
  else (take 9 (tail ((file_list, 633) !! i)), show (i + 1) ++ "\n" ++ init (add_time_stamp (tail file_list) s0 i 1))

-- This class and the four other functions below deal with generating a save game file.
-- The Serialise_diff class is used so that the Obj_place type gets extracted from Wall_grid.  This is done because the rest of the Wall_grid structure is not exposed to the
-- GPLC interpreter and so cannot change during game play.
class Serialise_diff a where
  save_diff :: ((Int, Int, Int), a) -> LBS.ByteString

instance Serialise_diff Wall_grid where
  save_diff ((w, u, v), x) = LBS.append (encode (w, u, v)) (encode (obj x))

instance Serialise_diff Floor_grid where
  save_diff ((w, u, v), x) = LBS.append (encode (w, u, v)) (encode x)

instance Serialise_diff (Int, [Int]) where
  save_diff ((w, u, v), x) = LBS.append (encode (w, u, v)) (encode x)

save_array_diff0 :: Bool -> Int -> ([Char], [Char]) -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> Array Int [Char] -> Play_state0 -> IO ()
save_array_diff0 False mode (save_file, save_log) w_grid_bstring f_grid_bstring obj_grid_bstring s0_bstring s1_bstring conf_reg s0 = return ()
save_array_diff0 True mode (save_file, save_log) w_grid_bstring f_grid_bstring obj_grid_bstring s0_bstring s1_bstring conf_reg s0 = do
  if mode == 0 then do
    contents <- bracket (openFile "save_log.log" ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nsave_log file size: " ++ show (length contents)); return contents)
    save_array_diff0 True 1 (select_save_file (splitOn "\n" contents) s0 ((length (splitOn "\n" contents)) - 1)) w_grid_bstring f_grid_bstring obj_grid_bstring s0_bstring s1_bstring conf_reg s0
  else do
    h0 <- openFile "save_log.log" WriteMode
    hPutStr h0 save_log
    hClose h0
    LBS.writeFile ((cfg conf_reg 0 "game_save_path") ++ save_file) (LBS.append (LBS.append (LBS.append (LBS.append (LBS.drop 8 w_grid_bstring) (LBS.drop 8 f_grid_bstring)) (LBS.drop 8 obj_grid_bstring)) s0_bstring) s1_bstring)
    putStr ("\n\nGame saved as: " ++ (cfg conf_reg 0 "game_save_path") ++ save_file)

wrapped_save_array_diff1 :: Serialise_diff a => SEQ.Seq ((Int, Int, Int), a) -> LBS.ByteString
wrapped_save_array_diff1 x =
  if SEQ.length x == 0 then LBS.append (encode (0 :: Int)) (encode (0 :: Int))
  else save_array_diff1 x LBS.empty 0 ((SEQ.length x) - 1)

save_array_diff1 :: Serialise_diff a => SEQ.Seq ((Int, Int, Int), a) -> LBS.ByteString -> Int -> Int -> LBS.ByteString
save_array_diff1 diff_seq diff_bytestring i limit =
  if i > limit then LBS.append (encode i) (LBS.append (encode (fromIntegral (LBS.length diff_bytestring) :: Int)) diff_bytestring)
  else save_array_diff1 diff_seq (LBS.append diff_bytestring (save_diff (SEQ.index diff_seq i))) (i + 1) limit

label_play_state_encoding :: LBS.ByteString -> LBS.ByteString
label_play_state_encoding x = LBS.append (encode (fromIntegral (LBS.length x) :: Int)) x

-- Find the uniform locations of GLSL uniform variables.
find_gl_uniform :: [[Char]] -> [Int] -> Ptr GLuint -> [Int32] -> IO [Int32]
find_gl_uniform [] [] p_gl_program acc = return acc
find_gl_uniform (x:xs) (y:ys) p_gl_program acc = do
  query <- newCString x
  gl_program <- peekElemOff p_gl_program y
  uniform <- glGetUniformLocation gl_program query
  free query
  find_gl_uniform xs ys p_gl_program (acc ++ [fromIntegral uniform])

-- These four functions deal with the compilation of the GLSL shaders and linking of the shader program.
make_gl_program :: [[Char]] -> Ptr GLuint -> Int -> IO ()
make_gl_program [] p_prog i = return ()
make_gl_program (x0:x1:xs) p_prog i = do
  sc0 <- shader_code ["#" ++ x0] nullPtr 0
  sc1 <- shader_code ["#" ++ x1] nullPtr 0
  shader0 <- make_shader GL_VERTEX_SHADER sc0
  shader1 <- make_shader GL_FRAGMENT_SHADER sc1
  gl_program <- make_program [shader0, shader1] 0 0
  pokeElemOff p_prog i gl_program
  free sc0; free sc1
  make_gl_program xs p_prog (i + 1)

shader_code :: [[Char]] -> Ptr (Ptr GLchar) -> Int -> IO (Ptr (Ptr GLchar))
shader_code sources ptr 0 = do
  p0 <- mallocBytes (ptr_size * length sources)
  shader_code sources p0 1
shader_code [] ptr c = return ptr
shader_code (x:xs) ptr c = do
  p <- newCString x
  pokeByteOff ptr (ptr_size * (c - 1)) p
  shader_code xs ptr (c + 1)

make_shader :: GLenum -> Ptr (Ptr GLchar) -> IO GLuint
make_shader t_shader source = do
  shader <- glCreateShader t_shader
  glShaderSource shader 1 source nullPtr
  glCompileShader shader
  p0 <- mallocBytes (sizeOf glint)
  p1 <- mallocBytes (sizeOf glint)
  glGetShaderiv shader GL_COMPILE_STATUS p0
  status <- peek p0
  if status == 0 then do
    glGetShaderiv shader GL_INFO_LOG_LENGTH p1
    p2 <- mallocBytes 1024
    glGetShaderInfoLog shader 1024 nullPtr p2
    e <- peekCString p2
    putStr "\nGL shader compile error: "
    print e
    return shader
  else return shader

make_program :: [GLuint] -> GLuint -> Int -> IO GLuint
make_program shaders program 0 = do
  prog <- glCreateProgram
  make_program shaders prog 1
make_program [] program c = do
  glLinkProgram program
  return program
make_program (x:xs) program c = do
  glAttachShader program x
  make_program xs program c

-- Check for errors during GLSL program compilation.
validate_prog :: GLuint -> Int -> IO ()
validate_prog program n = do
  glValidateProgram program
  p0 <- mallocBytes (sizeOf glint)
  glGetProgramiv program GL_VALIDATE_STATUS p0
  e0 <- peek p0
  p1 <- mallocBytes 1024
  glGetProgramInfoLog program 1024 nullPtr p1
  e1 <- peekCString p1
  putStr ("\nValidating GLSL program " ++ show n ++ "...")
  putStr ("\ngl_program validate status: " ++ show e0)
  putStr "GL program info log: "
  print e1
  free p0; free p1

-- These functions load bitmap images used for textures.
set_pad :: Int -> Int
set_pad tex_w =
  if mod tex_w 4 == 0 then 0
  else 4 - mod tex_w 4

load_bitmap0 :: [[Char]] -> [Object] -> [Char] -> [BS.ByteString] -> Int -> IO [BS.ByteString]
load_bitmap0 [] _ path acc c = return acc
load_bitmap0 (x:xs) (y:ys) path acc c = do
  if num_tex y == 0 then load_bitmap0 (x:xs) ys path acc 1
  else do
    contents <- BS.readFile (path ++ x)
    if c == (num_tex y) then do
      load_bitmap0 xs ys path (acc ++ [load_bitmap1 contents (BS.empty) (fromIntegral (tex_w y)) (fromIntegral (tex_h y)) 0 0]) 1
    else do
      load_bitmap0 xs (y:ys) path (acc ++ [load_bitmap1 contents (BS.empty) (fromIntegral (tex_w y)) (fromIntegral (tex_h y)) 0 0]) (c + 1)

load_bitmap1 :: BS.ByteString -> BS.ByteString -> Int -> Int -> Int -> Int -> BS.ByteString
load_bitmap1 bs0 bs1 w h pad y =
  if y == h then bs1
  else load_bitmap1 (BS.drop (3 * w) bs0) (BS.append bs1 (BS.take ((3 * w) - pad) bs0)) w h pad (y + 1)

load_bitmap2 :: GLsizei -> GLsizei -> Ptr CChar -> IO ()
load_bitmap2 w h p_tex = do
  glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGB) w h 0 GL_RGB GL_UNSIGNED_BYTE p_tex

-- These two functions load vertex data and bind OpenGL vertex array objects and texture objects, which are used to render all environmental models.
setup_object :: [Object] -> [[Int]] -> [Float] -> [GLushort] -> [BS.ByteString] -> Ptr GLuint -> IO ()
setup_object [] _ vertex element bs_tex p_bind = return ()
setup_object (x:xs) (y:ys) vertex element bs_tex p_bind = do
  glGenVertexArrays 1 (plusPtr p_bind ((ident x) * gluint))
  vao <- peekElemOff p_bind (ident x)
  p0 <- mallocBytes gluint
  p1 <- mallocBytes gluint
  p2 <- mallocBytes (glfloat * (y, 63) !! 1)
  p3 <- mallocBytes (glushort * (y, 64) !! 3)
  glGenBuffers 1 p0
  glGenBuffers 1 p1
  a_buf <- peek p0
  e_buf <- peek p1
  load_array (take ((y, 65) !! 1) (drop ((y, 66) !! 0) vertex)) p2 0
  load_array (take ((y, 67) !! 3) (drop ((y, 68) !! 2) element)) p3 0
  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER a_buf
  glBufferData GL_ARRAY_BUFFER (unsafeCoerce (plusPtr zero_ptr (glfloat * (y, 69) !! 1)) :: GLsizeiptr) p2 GL_STREAM_DRAW
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER e_buf
  glBufferData GL_ELEMENT_ARRAY_BUFFER (unsafeCoerce (plusPtr zero_ptr (glushort * (y, 70) !! 3)) :: GLsizeiptr) p3 GL_STREAM_DRAW
  free p0; free p1; free p2; free p3
  glVertexAttribPointer 0 4 GL_FLOAT 0 0 zero_ptr
  if num_tex x > 0 then do
    glVertexAttribPointer 1 2 GL_FLOAT 0 0 (plusPtr zero_ptr ((att_offset x) * 4 * glfloat))
    bind_texture (take (num_tex x) bs_tex) (plusPtr p_bind ((ident x) * gluint + gluint)) (tex_w x) (tex_h x) 0
    glVertexAttribPointer 2 3 GL_FLOAT 0 0 (plusPtr zero_ptr ((att_offset x) * 6 * glfloat))
    glEnableVertexAttribArray 0
    glEnableVertexAttribArray 1
    glEnableVertexAttribArray 2
    setup_object xs ys vertex element (drop (num_tex x) bs_tex) p_bind
  else do
    glVertexAttribPointer 1 4 GL_FLOAT 0 0 (plusPtr zero_ptr ((att_offset x) * 4 * glfloat))
    glVertexAttribPointer 2 3 GL_FLOAT 0 0 (plusPtr zero_ptr ((att_offset x) * 8 * glfloat))
    glEnableVertexAttribArray 0
    glEnableVertexAttribArray 1
    glEnableVertexAttribArray 2
    setup_object xs ys vertex element bs_tex p_bind

bind_texture :: [BS.ByteString] -> Ptr GLuint -> GLsizei -> GLsizei -> Int -> IO ()
bind_texture [] p_bind w h offset = return ()
bind_texture (x:xs) p_bind w h offset = do
  glGenTextures 1 (plusPtr p_bind (offset * gluint))
  tex <- peek (plusPtr p_bind (offset * gluint))
  glBindTexture GL_TEXTURE_2D tex
  BS.useAsCString (BS.take (fromIntegral (w * h * 3)) (BS.reverse x)) (load_bitmap2 w h)
  glGenerateMipmap GL_TEXTURE_2D
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)
  bind_texture xs p_bind w h (offset + 1)

-- This function manages the rendering of all environmental models and in game messages.  It recurses once per frame rendered and is the central branching point of the rendering thread.
show_frame :: (UArray Int Word32, Int) -> UArray Int Int32 -> (Ptr GLfloat, Ptr GLfloat) -> (Ptr Int, Ptr Int) -> Float -> Float -> Float -> Int -> Int -> MVar (Play_state0, Array (Int, Int, Int) Wall_grid, Save_state) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> UArray (Int, Int) Float -> Matrix Float -> Array Int (Int, [Int]) -> IO ([Int], Save_state)
show_frame p_bind uniform (p_mt_matrix, p_light_buffer) filter_table u v w a a' state_ref w_grid f_grid obj_grid look_up camera_to_clip msg_queue =
  let survey0 = multi_survey (mod_angle a (-92)) 183 u v (truncate u) (truncate v) w_grid f_grid obj_grid look_up 2 0 [] []
      survey1 = multi_survey (mod_angle (mod_angle a' a) 222) 183 (fst view_circle') (snd view_circle') (truncate (fst view_circle')) (truncate (snd view_circle')) w_grid f_grid obj_grid look_up 2 0 [] []
      view_circle' = view_circle u v 2 (mod_angle a a') look_up
      world_to_clip0 = multStd camera_to_clip (world_to_camera (-u) (-v) (-w) a look_up)
      world_to_clip1 = multStd camera_to_clip (world_to_camera (- (fst view_circle')) (- (snd view_circle')) (-w) (mod_angle (mod_angle a' a) 314) look_up)
  in do
  p_state <- takeMVar state_ref
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  if view_mode (fst__ p_state) == 0 then load_array (toList world_to_clip0) (castPtr p_mt_matrix) 0
  else do
    load_array (toList world_to_clip1) (castPtr p_mt_matrix) 0
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 2)))
    glUniformMatrix4fv (coerce (uniform ! 40)) 1 1 (castPtr p_mt_matrix)
    glUniform1i (coerce (uniform ! 50)) (fromIntegral (mod (fst__ (game_clock (fst__ p_state))) 240))
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 1)))
    glUniformMatrix4fv (coerce (uniform ! 52)) 1 1 (castPtr p_mt_matrix)
    show_player uniform p_bind (plusPtr p_mt_matrix (glfloat * 80)) u v w a look_up (rend_mode (fst__ p_state))
  if rend_mode (fst__ p_state) == 0 then do
    load_array (fst (mobile_lights (fst__ p_state)) ++ snd (mobile_lights (fst__ p_state))) p_light_buffer 0
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 7)))
    if mobile_lights (fst__ p_state) == ([], []) then glUniform1i (coerce (uniform ! 56)) (fromIntegral 2)
    else do
      glUniform1i (coerce (uniform ! 56)) ((div (fromIntegral (length (fst (mobile_lights (fst__ p_state))))) 4) + 2)
      glUniform4fv (coerce (uniform ! 54)) (fromIntegral (div (length (fst (mobile_lights (fst__ p_state)))) 4)) p_light_buffer
      glUniform3fv (coerce (uniform ! 55)) (fromIntegral (div (length (snd (mobile_lights (fst__ p_state)))) 3)) (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (fst__ p_state)))))
    glUniform1i (coerce (uniform ! 9)) (fromIntegral (mod (fst__ (game_clock (fst__ p_state))) 240))
    glUniformMatrix4fv (coerce (uniform ! 1)) 1 1 (castPtr p_mt_matrix)
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 6)))
    if mobile_lights (fst__ p_state) == ([], []) then glUniform1i (coerce (uniform ! 59)) (fromIntegral 2)
    else do
      glUniform1i (coerce (uniform ! 59)) ((div (fromIntegral (length (fst (mobile_lights (fst__ p_state))))) 4) + 2)
      glUniform4fv (coerce (uniform ! 57)) (fromIntegral (div (length (fst (mobile_lights (fst__ p_state)))) 4)) p_light_buffer
      glUniform3fv (coerce (uniform ! 58)) (fromIntegral (div (length (snd (mobile_lights (fst__ p_state)))) 3)) (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (fst__ p_state)))))
    glUniform1i (coerce (uniform ! 20)) (fromIntegral (mod (fst__ (game_clock (fst__ p_state))) 240))
    glUniformMatrix4fv (coerce (uniform ! 12)) 1 1 (castPtr p_mt_matrix)
  else do
    load_array ([3, 3, 3, 1] ++ fst (mobile_lights (fst__ p_state)) ++ [coerce u, coerce v, coerce w] ++ snd (mobile_lights (fst__ p_state))) p_light_buffer 0
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 5)))
    glUniform1i (coerce (uniform ! 62)) ((div (fromIntegral (length (fst (mobile_lights (fst__ p_state))))) 4) + 1)
    glUniform4fv (coerce (uniform ! 60)) ((fromIntegral (div (length (fst (mobile_lights (fst__ p_state)))) 4)) + 1) p_light_buffer
    glUniform3fv (coerce (uniform ! 61)) ((fromIntegral (div (length (snd (mobile_lights (fst__ p_state)))) 3)) + 1) (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (fst__ p_state))) + 16))
    glUniformMatrix4fv (coerce (uniform ! 24)) 1 1 (castPtr p_mt_matrix)
    glUniform1i (coerce (uniform ! 27)) (fromIntegral (torch_t_limit (fst__ p_state) - (fst__ (game_clock (fst__ p_state)) - torch_t0 (fst__ p_state))))
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 4)))
    glUniform1i (coerce (uniform ! 65)) ((div (fromIntegral (length (fst (mobile_lights (fst__ p_state))))) 4) + 1)
    glUniform4fv (coerce (uniform ! 63)) ((fromIntegral (div (length (fst (mobile_lights (fst__ p_state)))) 4)) + 1) p_light_buffer
    glUniform3fv (coerce (uniform ! 64)) ((fromIntegral (div (length (snd (mobile_lights (fst__ p_state)))) 3)) + 1) (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (fst__ p_state))) + 16))
    glUniformMatrix4fv (coerce (uniform ! 30)) 1 1 (castPtr p_mt_matrix)
    glUniform1i (coerce (uniform ! 33)) (fromIntegral (torch_t_limit (fst__ p_state) - (fst__ (game_clock (fst__ p_state)) - torch_t0 (fst__ p_state))))
  glBindVertexArray (unsafeCoerce ((fst p_bind) ! 0))
  if view_mode (fst__ p_state) == 0 then do
    filtered_surv0 <- filter_surv (fst survey0) [] (fst filter_table) (third_ (game_clock (fst__ p_state)))
    filtered_surv1 <- filter_surv (snd survey0) [] (snd filter_table) (third_ (game_clock (fst__ p_state)))
    show_walls filtered_surv0 uniform p_bind (plusPtr p_mt_matrix (glfloat * 16)) u v w a look_up (rend_mode (fst__ p_state))
    show_object filtered_surv1 uniform p_bind (plusPtr p_mt_matrix (glfloat * 48)) u v w a look_up (rend_mode (fst__ p_state))
  else do
    filtered_surv0 <- filter_surv (fst survey1) [] (fst filter_table) (third_ (game_clock (fst__ p_state)))
    filtered_surv1 <- filter_surv (snd survey1) [] (snd filter_table) (third_ (game_clock (fst__ p_state)))
    show_walls filtered_surv0 uniform p_bind (plusPtr p_mt_matrix (glfloat * 16)) u v w a look_up (rend_mode (fst__ p_state))
    show_object filtered_surv1 uniform p_bind (plusPtr p_mt_matrix (glfloat * 48)) u v w a look_up (rend_mode (fst__ p_state))
  msg_residue <- handle_message0 (handle_message1 (message_ (fst__ p_state)) msg_queue 0 3) uniform p_bind 0
  if fst msg_residue == 1 then return ([1], third_ p_state)
  else if fst msg_residue == 2 then do
    threadDelay 5000000
    return ([2], third_ p_state)
  else if fst msg_residue == 3 then return ([3], third_ p_state)
  else if fst msg_residue > 3 then return (([0] ++ (snd (head (message_ (fst__ p_state))))), third_ p_state)
  else do
    swapBuffers
    show_frame p_bind uniform (p_mt_matrix, p_light_buffer) filter_table (pos_u (fst__ p_state)) (pos_v (fst__ p_state)) (pos_w (fst__ p_state)) (angle (fst__ p_state)) (view_angle (fst__ p_state)) state_ref (snd__ p_state) f_grid obj_grid look_up camera_to_clip (snd msg_residue)

--These two functions iterate through the message queue received from the game logic thread.  They manage the appearance and expiry of on screen messages and detect special event messages,
--such as are received when the user opts to return to the main menu.
handle_message1 :: [(Int, [Int])] -> Array Int (Int, [Int]) -> Int -> Int -> (Int, Array Int (Int, [Int]))
handle_message1 [] msg_queue i0 i1 = (0, msg_queue)
handle_message1 (x:xs) msg_queue i0 i1 =
  if fst x < 0 then (abs (fst x), msg_queue)
  else if head (snd x) == -1 then
    if fst (msg_queue ! i1) == 0 && i1 < 5 then handle_message1 xs (msg_queue // [(i1, x)]) i0 (i1 + 1)
    else if fst (msg_queue ! i1) == 0 && i1 == 5 then handle_message1 xs (msg_queue // [(i1, x)]) i0 3
    else if fst (msg_queue ! i1) > 0 && i1 < 5 then handle_message1 (x:xs) msg_queue i0 (i1 + 1)
    else handle_message1 xs (msg_queue // [(3, x)]) i0 4
  else
    if fst (msg_queue ! i0) == 0 && i0 < 2 then handle_message1 xs (msg_queue // [(i0, x)]) (i0 + 1) i1
    else if fst (msg_queue ! i0) == 0 && i0 == 2 then handle_message1 xs (msg_queue // [(i0, x)]) 0 i1
    else if fst (msg_queue ! i0) > 0 && i0 < 2 then handle_message1 (x:xs) msg_queue (i0 + 1) i1
    else handle_message1 xs (msg_queue // [(0, x)]) 1 i1

handle_message0 :: (Int, Array Int (Int, [Int])) -> UArray Int Int32 -> (UArray Int Word32, Int) -> Int -> IO (Int, Array Int (Int, [Int]))
handle_message0 msg_queue uniform p_bind i =
  let h_pos = \x -> if x < 3 then -0.96
                    else 0.64
      msg_queue_ = (snd msg_queue) ! i
  in do
  if fst msg_queue > 0 then return msg_queue
  else if i > 5 then return msg_queue
  else if fst ((snd msg_queue) ! i) > 0 then do
    show_text (tail (snd ((snd msg_queue) ! i))) 0 933 uniform p_bind (h_pos i) (0.9 - 0.05 * fromIntegral (mod i 3)) zero_ptr
    handle_message0 (0, (snd msg_queue) // [(i, ((fst msg_queue_) - 1, snd msg_queue_))]) uniform p_bind (i + 1)
  else handle_message0 msg_queue uniform p_bind (i + 1)

-- These three functions pass transformation matrices to the shaders and make the GL draw calls that render models.
show_walls :: [Wall_place] -> UArray Int Int32 -> (UArray Int Word32, Int) -> Ptr GLfloat -> Float -> Float -> Float -> Int -> UArray (Int, Int) Float -> Int -> IO ()
show_walls [] uniform p_bind p_mt_matrix u v w a look_up mode = return ()
show_walls (x:xs) uniform p_bind p_mt_matrix u v w a look_up mode = do
  if isNull x == False then do
    load_array (toList (model_to_world (translate_u x) (translate_v x) (translate_w x) 0 False look_up)) (castPtr p_mt_matrix) 0
    if texture_ x == 0 && mode == 0 then do
      glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 7)))
      glUniformMatrix4fv (coerce (uniform ! 0)) 1 1 p_mt_matrix
    else if texture_ x > 0 && texture_ x < (snd p_bind) && mode == 0 then do
      glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 6)))
      glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst p_bind) ! texture_ x))
      glUniformMatrix4fv (coerce (uniform ! 11)) 1 1 p_mt_matrix
    else if texture_ x == 0 && mode == 1 then do
      glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 5)))
      glUniformMatrix4fv (coerce (uniform ! 23)) 1 1 p_mt_matrix
    else if texture_ x > 0 && texture_ x < (snd p_bind) && mode == 1 then do
      glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 4)))
      glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst p_bind) ! texture_ x))
      glUniformMatrix4fv (coerce (uniform ! 29)) 1 1 p_mt_matrix
    else do
      putStr "\nshow_walls: Invalid wall_place texture reference in map..."
      show_walls xs uniform p_bind p_mt_matrix u v w a look_up mode
    if Build_model.rotate x < 2 then glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT (plusPtr zero_ptr (glushort * 36))
    else glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT zero_ptr
    show_walls xs uniform p_bind p_mt_matrix u v w a look_up mode
  else show_walls xs uniform p_bind p_mt_matrix u v w a look_up mode

show_object :: [Obj_place] -> UArray Int Int32 -> (UArray Int Word32, Int) -> Ptr GLfloat -> Float -> Float -> Float -> Int -> UArray (Int, Int) Float -> Int -> IO ()
show_object [] uniform p_bind p_mt_matrix u v w a look_up mode = return ()
show_object (x:xs) uniform p_bind p_mt_matrix u v w a look_up mode = do
  load_array (toList (model_to_world (u__ x) (v__ x) (w__ x) 0 False look_up)) (castPtr p_mt_matrix) 0
  if ident_ x < (snd p_bind) && texture__ x == 0 && mode == 0 then do
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 7)))
    glUniformMatrix4fv (coerce (uniform ! 0)) 1 1 p_mt_matrix
  else if (ident_ x) + (texture__ x) < snd p_bind && texture__ x > 0 && mode == 0 then do
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 6)))
    glUniformMatrix4fv (coerce (uniform ! 11)) 1 1 p_mt_matrix
    glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst p_bind) ! ((ident_ x) + (texture__ x))))
  else if ident_ x < (snd p_bind) && texture__ x == 0 && mode == 1 then do
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 5)))
    glUniformMatrix4fv (coerce (uniform ! 23)) 1 1 p_mt_matrix
  else if (ident_ x) + (texture__ x) < snd p_bind && texture__ x > 0 && mode == 1 then do
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 4)))
    glUniformMatrix4fv (coerce (uniform ! 29)) 1 1 p_mt_matrix
    glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst p_bind) ! ((ident_ x) + (texture__ x))))
  else do
    putStr "\nshow_object: Invalid obj_place object or texture reference in map..."
    show_object xs uniform p_bind p_mt_matrix u v w a look_up mode
  glBindVertexArray (unsafeCoerce ((fst p_bind) ! (ident_ x)))
  glDrawElements GL_TRIANGLES (coerce (num_elem x)) GL_UNSIGNED_SHORT zero_ptr
  show_object xs uniform p_bind p_mt_matrix u v w a look_up mode

show_player :: UArray Int Int32 -> (UArray Int Word32, Int) -> Ptr GLfloat -> Float -> Float -> Float -> Int -> UArray (Int, Int) Float -> Int -> IO ()
show_player uniform p_bind p_mt_matrix u v w a look_up mode = do
  load_array (toList (model_to_world u v w a True look_up)) (castPtr p_mt_matrix) 0
  load_array (toList (world_to_model u v w a True look_up)) (castPtr p_mt_matrix) 16
  load_array (toList (rotation_w a look_up)) (castPtr p_mt_matrix) 32
  if mode == 0 then do
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 2)))
    glUniformMatrix4fv (coerce (uniform ! 39)) 1 1 p_mt_matrix
    glUniformMatrix4fv (coerce (uniform ! 41)) 1 1 (plusPtr p_mt_matrix (glfloat * 16))
    glUniformMatrix4fv (coerce (uniform ! 42)) 1 1 (plusPtr p_mt_matrix (glfloat * 32))
  else do
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 1)))
    glUniformMatrix4fv (coerce (uniform ! 51)) 1 1 p_mt_matrix
  glBindVertexArray (unsafeCoerce ((fst p_bind) ! 1024))
  glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst p_bind) ! 1025))
  glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT zero_ptr
