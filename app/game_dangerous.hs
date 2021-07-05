-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude hiding ((!!))
import Index_wrapper0
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
import Encode_status

-- This function processes text from input files to return a result that is independent on whether the file has the Windows or Unix end of file format.
tailFile :: [Char] -> [Char]
tailFile contents =
  if last (splitOn "\n" contents) == [] then init contents
  else contents

main = do
  args <- getArgs
  if length args == 0 then do
    contents <- bracket (openFile "config.txt" ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\ncfg file size: " ++ show (length c)); return c)
    openWindow (listArray (0, 87) (splitOneOf "=\n" (tailFile contents)))
  else do
    contents <- bracket (openFile ((args, 1) !! 0) ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\ncfg file size: " ++ show (length c)); return c)
    openWindow (listArray (0, 87) (splitOneOf "=\n" (tailFile contents)))

-- This function initialises the GLUT runtime system, which in turn is used to initialise a window and OpenGL context.
openWindow :: Array Int [Char] -> IO ()
openWindow conf_reg =
  let cfg' = cfg conf_reg 0
      cb = \x -> head (cfg' x)
      key_set = listArray (0, 14) [cb "cb_PAUSE", cb "cb_FORWARD", cb "cb_STRAFE_RIGHT", cb "cb_BACK", cb "cb_STRAFE_LEFT", cb "cb_TURN_LEFT"
                                  , cb "cb_TURN_RIGHT", cb "cb_JUMP", cb "cb_LIGHT_TORCH", cb "cb_SWITCH_VIEW", cb "cb_ROTATE_VIEW", cb "cb_FIRE"
                                  , cb "cb_MENU_SELECT", cb "cb_MENU_BACK", cb "cb_MENU_HOME"]
      filePath = cfg' "map_file_path" ++ cfg' "map_file"
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
  displayCallback $= repaintWindow
  control_ref <- newIORef 0
  keyboardCallback $= (Just (getInput control_ref key_set))
  contents <- bracket (openFile filePath ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\nmap file size: " ++ show (length c)); return c)
  screen_res <- readIORef screenRes
  setupGame (tailFile contents) conf_reg screen_res control_ref

-- This is the callback that GLUT calls each time mainLoopEvent has been called and there is keyboard input in the window message queue.
getInput :: IORef Int -> Array Int Char -> Char -> Position -> IO ()
getInput ref key_set key pos = do
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

-- This is the callback that GLUT calls when it detects a window repaint is necessary.  This should only happen when the window is first opened, the user moves
-- or resizes the window, or it is overlapped by another window.  For standard frame rendering, showFrame and runMenu repaint the rendered area of the window.
repaintWindow :: IO ()
repaintWindow = return ()

-- This function initialises the OpenAL context, decompresses the map file, manages the compilation of GLSL shaders, loading of 3D models, loading of the
-- light map and loading of sound effects.
setupGame :: [Char] -> Array Int [Char] -> Size -> IORef Int -> IO ()
setupGame comp_env_map conf_reg (Size w h) control_ref =
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
      u_max = (read (((splitOn "\n~\n" comp_env_map), 3) !! 12))
      v_max = (read (((splitOn "\n~\n" comp_env_map), 4) !! 13))
      w_max = (read (((splitOn "\n~\n" comp_env_map), 5) !! 14))
      proc_map' = procMap (splitOn "\n~\n" comp_env_map) u_max v_max w_max
      pm'' = fst proc_map'
      pm''' = snd proc_map'
      mc = \i -> ((splitOn "\n~\n" comp_env_map), 637) !! i
      env_map = ".~.~.~.~" ++ pm'' ++ "~" ++ pm''' ++ (mc 10) ++ "~" ++ (mc 11) ++ "~" ++ (mc 12) ++ "~" ++ (mc 13) ++ "~" ++ (mc 14) ++ "~" ++ (mc 15)
      cfg' = cfg conf_reg 0
      p_bind_limit = (read (((splitOn "\n~\n" comp_env_map), 11) !! 7)) - 1
      frustumScale0 = (read (cfg' "frustumScale1")) / (fromIntegral w / fromIntegral h)
  in do
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LEQUAL
  glDepthRange 0 1
  glEnable GL_DEPTH_CLAMP
  contents0 <- bracket (openFile (cfg' "shader_file") ReadMode) (hClose)
                       (\h -> do c <- hGetContents h; putStr ("\nshader file size: " ++ show (length c)); return c)
  p_gl_program <- mallocBytes (7 * gluint)
  makeGlProgram (tail (splitOn "#" (tailFile contents0))) p_gl_program 0
  uniform <- findGlUniform [m0, m1, m2, lm0, lm1, lm2, lm3, lm4, lm5, "t", "mode", m0, m1, m2, lm0, lm1, lm2, lm3, lm4, lm5, "t", "mode", "tex_unit0", m0, m1
                           , m2, "worldTorchPos", "timer", "mode", m0, m1, m2, "worldTorchPos", "timer", "mode", "tex_unit0", "tt_matrix", "tex_unit0", "mode"
                           , m0, m1, m2, "normal_transf", lm0, lm1, lm2, lm3, lm4, lm5, "tex_unit0", "t", m0, m1, "tex_unit0", dl0, dl1, dl2, dl0, dl1, dl2
                           , dl0, dl1, dl2, dl0, dl1, dl2]
                           [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5
                           , 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3] p_gl_program []
  gl_program0 <- peekElemOff p_gl_program 0
  gl_program1 <- peekElemOff p_gl_program 1
  gl_program2 <- peekElemOff p_gl_program 2
  gl_program3 <- peekElemOff p_gl_program 3
  gl_program4 <- peekElemOff p_gl_program 4
  gl_program5 <- peekElemOff p_gl_program 5
  gl_program6 <- peekElemOff p_gl_program 6
  glClearColor 0 0 0 0
  glClearDepth 1
  contents1 <- bracket (openFile (cfg' "model_data_dir" ++ mc 9) ReadMode) (hClose)
                       (\h -> do c <- hGetContents h; putStr ("\nlight map file size: " ++ show (length c)); return c)
  p_lmap_pos0 <- callocBytes (glfloat * 300)
  p_lmap_pos1 <- callocBytes (glfloat * 300)
  p_lmap_int0 <- callocBytes (glfloat * 300)
  p_lmap_int1 <- callocBytes (glfloat * 300)
  p_lmap_t0 <- callocBytes (glfloat * 240)
  p_lmap_t1 <- callocBytes (glfloat * 240)
  loadArray (take 300 (procFloats (splitOn ", " (((splitOn "\n~\n" (tailFile contents1)), 13) !! 0)))) p_lmap_pos0 0
  loadArray (take 300 (procFloats (splitOn ", " (((splitOn "\n~\n" (tailFile contents1)), 14) !! 1)))) p_lmap_pos1 0
  loadArray (take 300 (procFloats (splitOn ", " (((splitOn "\n~\n" (tailFile contents1)), 15) !! 2)))) p_lmap_int0 0
  loadArray (take 300 (procFloats (splitOn ", " (((splitOn "\n~\n" (tailFile contents1)), 16) !! 3)))) p_lmap_int1 0
  loadArray (take 240 (procFloats (splitOn ", " (((splitOn "\n~\n" (tailFile contents1)), 17) !! 4)))) p_lmap_t0 0
  loadArray (take 240 (procFloats (splitOn ", " (((splitOn "\n~\n" (tailFile contents1)), 18) !! 5)))) p_lmap_t1 0
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
  validateProg gl_program0 0
  validateProg gl_program1 1
  validateProg gl_program2 2
  validateProg gl_program3 3
  validateProg gl_program4 4
  validateProg gl_program5 5
  validateProg gl_program6 6
  p_bind <- bufferToArray (castPtr p_gl_program) (array (0, p_bind_limit) [(x, 0) | x <- [0..p_bind_limit]]) 0 (p_bind_limit - 6) 6
  putStr "\nLoading 3D models..."
  mod_bind <- callocBytes ((read (((splitOn "\n~\n" comp_env_map), 42) !! 7)) * gluint)
  loadModFile (init (splitOn ", " (((splitOn "\n~\n" comp_env_map), 43) !! 8))) (cfg' "model_data_dir") mod_bind
  free p_gl_program; free p_lmap_pos0; free p_lmap_pos1; free p_lmap_int0; free p_lmap_int1; free p_lmap_t0; free p_lmap_t1
  p_bind_ <- bufferToArray (castPtr mod_bind) p_bind 0 0 (p_bind_limit - 16)
  initAlContext
  contents2 <- bracket (openFile ((cfg' "sound_data_dir") ++ (last (splitOn ", " (mc 8)))) ReadMode) (hClose)
                       (\h -> do c <- hGetContents h; putStr ("\nsound map size: " ++ show (length c)); return c)
  sound_array <- initAlEffect0 (splitOneOf "\n " (tailFile contents2)) (cfg' "sound_data_dir")
                               (array (0, (div (length (splitOneOf "\n " (tailFile contents2))) 2) - 1) [(x, Source 0) | x <- [0..(div (length (splitOneOf "\n " (tailFile contents2))) 2) - 1]])
  r_gen <- getStdGen
  startGame control_ref (listArray (0, 65) uniform) (p_bind_, p_bind_limit + 1) env_map conf_reg (-1) (read (cfg' "init_u")) (read (cfg' "init_v"))
            (read (cfg' "init_w")) (read (cfg' "gravity")) (read (cfg' "friction")) (read (cfg' "run_power")) (read (cfg' "jump_power"))
            def_save_state sound_array (cameraToClip frustumScale0 (read (cfg' "frustumScale1"))) r_gen

-- The model file(s) that describe all 3D and 2D models referenced in the current map are loaded here.
loadModFile :: [[Char]] -> [Char] -> Ptr GLuint -> IO ()
loadModFile [] path p_bind = return ()
loadModFile (x:xs) path p_bind =
  let md = \mod_data i -> ((splitOn "~" (tailFile mod_data)), 638) !! i
  in do
  h <- openFile (path ++ x) ReadMode
  mod_data <- hGetContents h
  if md mod_data 0 == [] then setupObject (loadObject0 (splitOn "&" (md mod_data 1))) (procMarker (procInts (splitOn ", " (md mod_data 2))))
                                          (procFloats (splitOn ", " (md mod_data 3))) (procElements (splitOn ", " (md mod_data 4))) [] p_bind
  else do
    bs_tex <- loadBitmap0 (splitOn ", " (((splitOn "~" (tailFile mod_data)), 50) !! 0)) (loadObject0 (splitOn "&" (((splitOn "~" (tailFile mod_data)), 51) !! 1))) path [] 1
    setupObject (loadObject0 (splitOn "&" (md mod_data 1))) (procMarker (procInts (splitOn ", " (md mod_data 2)))) (procFloats (splitOn ", " (md mod_data 3)))
                (procElements (splitOn ", " (md mod_data 4))) bs_tex p_bind
  hClose h
  loadModFile xs path p_bind

-- These two functions convert parameters passed through the engine's configuration file to the corresponding values in the Play_state0 and Play_state1
-- structures, respectively.
selectMetricMode "none" = 0
selectMetricMode "low" = 1
selectMetricMode "medium" = 2
selectMetricMode "high" = 3

selectVerboseMode "y" = True
selectVerboseMode "n" = False

-- This function generates the pseudorandom number sequence that is exposed to certain GPLC op - codes.
genProbSeq :: RandomGen g => Int -> Int -> Int -> g -> UArray Int Int
genProbSeq i0 i1 i2 g = listArray (i0, i1) (drop i2 (randomRs (0, 99) g))

-- The game state can be initialised within startGame in three different ways.  Namely, an unlocked or locked map being initialised to a base state
-- or a map being initialised using a saved game file.  This function is used by startGame to select the correct game state values to pass to
-- updatePlay depending on which mode is being used.
selectState :: Int -> [Char] -> a -> a -> a -> a
selectState 0 "unlocked" x y z = x
selectState 0 "locked" x y z = y
selectState 1 lock_flag x y z = z

-- When a locked map is used to initialise the game state the unlocking logic is applied here.
unlockWrapper :: [Char] -> Play_state0 -> Play_state1 -> (Play_state0, Play_state1)
unlockWrapper map_unlock_code s0 s1 =
  let bit_list = pad (decimalBinary (hexDecimal map_unlock_code 31) (2 ^ 127)) [] 127 0
      state_values = extractStateValues (listArray (0, 127) bit_list) s0 s1 0
  in
  if third_ state_values == True then (fst__ state_values, snd__ state_values)
  else error "\n\nInvalid map unlock code detected."

-- This function generates the camera to clip transformation matrix that will be passed to the shaders.
cameraToClip :: Float -> Float -> Matrix Float
cameraToClip frustumScale0 frustumScale1 =
  let y = \x -> fromList 4 4 x
  in
  y [frustumScale0, 0, 0, 0, 0, frustumScale1, 0, 0, 0, 0, ((zFar + zNear) / (zNear - zFar)), ((2 * zFar * zNear) / (zNear - zFar)), 0, 0, -1, 0]

-- This function initialises the game logic thread each time a new game is started and handles user input from the main menu.
startGame :: RandomGen g => IORef Int -> UArray Int Int32 -> (UArray Int Word32, Int) -> [Char] -> Array Int [Char] -> Int -> Float -> Float -> Float -> Float
             -> Float -> Float -> Float -> Game_state -> Array Int Source -> Matrix Float -> g -> IO ()
startGame control_ref uniform p_bind c conf_reg mode u v w g f mag_r mag_j save_state sound_array camera_to_clip r_gen =
  let u_limit = (read (((splitOn "~" c), 56) !! 8))
      v_limit = (read (((splitOn "~" c), 57) !! 9))
      w_limit = (read (((splitOn "~" c), 58) !! 10))
      fd = \limit -> (div (limit + 1) 2) - 1
      buildTable1_ = buildTable1 (splitOn ", " (((splitOn "~" c), 59) !! 7)) (emptyWGrid u_limit v_limit w_limit) 7500
      buildTable0_ = buildTable0 (elems buildTable1_) u_limit v_limit w_limit
      w_grid = checkMapLayer (-3) 0 0 u_limit v_limit
               (makeArray0 (buildTable0_ ++ (sortGrid0 (splitOn "&" (((splitOn "~" c), 60) !! 4)))) u_limit v_limit w_limit)
               w_grid_flag
      f_grid = checkMapLayer 0 0 0 (fd u_limit) (fd v_limit)
                             (makeArray1 (loadFloor0 (splitOn "&" (((splitOn "~" c), 61) !! 5))) (fd u_limit) (fd v_limit) w_limit) f_grid_flag
      obj_grid = checkMapLayer 0 0 0 u_limit v_limit (emptyObjGrid u_limit v_limit w_limit // loadObjGrid (splitOn ", " (((splitOn "~" c), 62) !! 6)))
                               obj_grid_flag
      look_up_ = lookUp [makeTable 0 0, makeTable 1 0, makeTable 2 0, makeTable 3 0]
      cfg' = cfg conf_reg 0
      setup_music = if cfg' "music" == "off" then 0
                    else read (cfg' "music_period")
      s0 = ps0_init {pos_u = u, pos_v = v, pos_w = w, on_screen_metrics = selectMetricMode (cfg' "on_screen_metrics"),
                     prob_seq = genProbSeq 0 239 (read (cfg' "prob_c")) r_gen}
      s1 = ps1_init {verbose_mode = selectVerboseMode (cfg' "verbose_mode")}
      unlocked_state = unlockWrapper (cfg' "map_unlock_code") s0 s1
      lock_flag = ((splitOn "~" c), 635) !! 11
  in do
  if mode == -1 then do
    if cfg' "splash_image" == "on" then do
      glDisable GL_DEPTH_TEST
      glBindVertexArray (unsafeCoerce ((fst p_bind) ! 1026))
      glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst p_bind) ! 1028))
      glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 3)))
      glUniform1i (fromIntegral (uniform ! 38)) 0
      p_tt_matrix <- mallocBytes (glfloat * 16)
      loadArray [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1] p_tt_matrix 0
      glUniformMatrix4fv (fromIntegral (uniform ! 36)) 1 1 p_tt_matrix
      glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT zero_ptr
      swapBuffers
      free p_tt_matrix
      threadDelay 5000000
      glEnable GL_DEPTH_TEST
      startGame control_ref uniform p_bind c conf_reg 2 u v w g f mag_r mag_j save_state sound_array camera_to_clip r_gen
    else startGame control_ref uniform p_bind c conf_reg 0 u v w g f mag_r mag_j save_state sound_array camera_to_clip r_gen
  else if mode < 2 then do
    p_mt_matrix <- mallocBytes (glfloat * 128)
    p_f_table0 <- callocBytes (int_ * 120000)
    p_f_table1 <- callocBytes (int_ * 37500)
    p_light_buffer <- mallocBytes (glfloat * 35)
    state_ref <- newEmptyMVar
    t_log <- newEmptyMVar
    tid <- forkIO (updatePlay (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = control_ref}) state_ref
                              (selectState mode lock_flag s0 (fst unlocked_state)
                                           ((s0_ save_state) {on_screen_metrics = selectMetricMode (cfg' "on_screen_metrics")}))
                              (selectState mode lock_flag s1 (snd unlocked_state) ((s1_ save_state) {verbose_mode = selectVerboseMode (cfg' "verbose_mode")}))
                              False (read (cfg' "min_frame_t")) (g, f, mag_r, mag_j)
                              (selectState mode lock_flag w_grid w_grid (w_grid_ save_state)) (selectState mode lock_flag f_grid f_grid (f_grid_ save_state))
                              (selectState mode lock_flag obj_grid obj_grid (obj_grid_ save_state)) look_up_ save_state (sound_array, setup_music)
                              0 t_log (SEQ.empty) 60)
    result <- showFrame p_bind uniform (p_mt_matrix, p_light_buffer) (p_f_table0, p_f_table1) 0 0 0 0 0 state_ref w_grid f_grid obj_grid look_up_
                        camera_to_clip (array (0, 5) [(i, (0, [])) | i <- [0..5]]) (SEQ.Empty)
    free p_mt_matrix
    free p_f_table0
    free p_f_table1
    free p_light_buffer
    killThread tid
    h <- openFile "benchmark_log.txt" WriteMode
    hPutStr h (showFrameRecords (third_ result) 0 ((SEQ.length (third_ result)) - 1))
    hClose h
    startGame control_ref uniform p_bind c conf_reg ((head (fst__ result)) + 1) u v w g f mag_r mag_j (snd__ result) sound_array camera_to_clip r_gen
  else if mode == 2 then do
    choice <- runMenu mainMenuText [] (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = control_ref}) (-0.75) (-0.75) 1 0 0 ps0_init 1
    if choice == 1 then startGame control_ref uniform p_bind c conf_reg 0 u v w g f mag_r mag_j save_state sound_array camera_to_clip r_gen
    else if choice == 2 then do
      contents <- bracket (openFile "save_log.log" ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\nsave_log size: " ++ show (length c)); return c)
      state_choice <- runMenu (genLoadMenu (tail (splitOn "\n" (tailFile contents))) [] 1) [] (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = control_ref})
                              (-0.75) (-0.75) 1 0 0 ps0_init 1
      loaded_state <- loadSavedGame 0 (tail (splitOn "\n" (tailFile contents))) [] 1 state_choice (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = control_ref})
                                    w_grid f_grid obj_grid conf_reg
      if isNothing loaded_state == True then startGame control_ref uniform p_bind c conf_reg 2 u v w g f mag_r mag_j def_save_state sound_array
                                                       camera_to_clip r_gen
      else startGame control_ref uniform p_bind c conf_reg 1 u v w g f mag_r mag_j (fromJust loaded_state) sound_array camera_to_clip r_gen
    else exitSuccess
  else if mode == 3 then do
    if is_set save_state == True then startGame control_ref uniform p_bind c conf_reg 1 u v w g f mag_r mag_j save_state sound_array camera_to_clip r_gen
    else startGame control_ref uniform p_bind c conf_reg 0 u v w g f mag_r mag_j save_state sound_array camera_to_clip r_gen
  else if mode == 4 then exitSuccess
  else if mode == 5 then do
    saveArrayDiff0 0 ([], []) (wrappedSaveArrayDiff1 (genArrayDiff (-3) 0 0 u_limit v_limit w_grid (w_grid_ save_state) SEQ.empty))
                   (wrappedSaveArrayDiff1 (genArrayDiff 0 0 0 ((div (u_limit + 1) 2) - 1) ((div (v_limit + 1) 2) - 1) f_grid (f_grid_ save_state) SEQ.empty))
                   (wrappedSaveArrayDiff1 (genArrayDiff 0 0 0 u_limit v_limit obj_grid (obj_grid_ save_state) SEQ.empty))
                   (labelPlayStateEncoding (encode (s0_ save_state))) (labelPlayStateEncoding (encode (s1_ save_state))) conf_reg (s0_ save_state)
    startGame control_ref uniform p_bind c conf_reg 1 u v w g f mag_r mag_j save_state sound_array camera_to_clip r_gen
  else if mode == 6 then do
    updConfigFile conf_reg (map_transit_string save_state) False
    h <- openFile "save_log.log" WriteMode
    hPutStr h def_save_log
    hClose h
    putStr ("\n\nConfiguration file updated due to map transit event.")
    exitSuccess
  else return ()

-- Used to update the engine's configuration file when a map transit event occurs, such that the targetted map will be loaded the next time the engine is run.
updConfigFile :: Array Int [Char] -> ([Char], [Char]) -> Bool -> IO ()
updConfigFile conf_reg (map_file, map_unlock_code) ready_flag = do
  if ready_flag == False then updConfigFile (updateCfg conf_reg "map_file" map_file 0) (map_file, map_unlock_code) True
  else do
    h <- openFile (cfg conf_reg 0 "config_file") WriteMode
    hPutStr h (init (writeCfg (updateCfg conf_reg "map_unlock_code" map_unlock_code 0) 0))
    hClose h

-- This function determines the content of the load game menu that allows the user to load a previous game state.
genLoadMenu :: [[Char]] -> [(Int, [Int])] -> Int -> [(Int, [Int])]
genLoadMenu [] acc c =
  if acc == [] then no_game_states_header
  else load_game_menu_header ++ acc
genLoadMenu ((y0:y1:y2:y3:y4:y5:y6:y7:y8:y9:y10:y11:y12:y13:y14:y15:y16:ys):xs) acc c =
  let state_choice = [(c, game_state_text ++ [c + 53] ++ game_time_text ++ [(read [y11] + 53), (read [y12] + 53), 69, (read [y13] + 53), (read [y14] + 53), 69
                     , (read [y15] + 53), (read [y16] + 53)])]
  in
  if y0 == '_' then genLoadMenu xs (acc ++ state_choice) (c + 1)
  else genLoadMenu xs acc c

-- Constants used to fix the types decoded from save game files.
def_obj_place_ = ((0, 0, 0), Nothing) :: ((Int, Int, Int), Maybe Obj_place)
def_f_grid_ = ((0, 0, 0), def_f_grid) :: ((Int, Int, Int), Floor_grid)
def_obj_grid_ = ((0, 0, 0), def_obj_grid) :: ((Int, Int, Int), (Int, [Int]))

-- These three functions deal with loading a saved game state and recreating the game state by updating a base map state.
decodeSequence :: Binary a => Int -> a -> LBS.ByteString -> SEQ.Seq a -> SEQ.Seq a
decodeSequence c def bs diff_seq =
  let result = decodeOrFail bs
      tested_result = if (isLeft result) == True then error ("\ndecode_sequence: " ++ third_ (fromLeft (LBS.empty, 0, "def") result) ++ "\noffset: " ++ show c)
                      else fromRight (LBS.empty, 0, def) result
  in
  if LBS.length bs == 0 then diff_seq
  else decodeSequence (c + 1) def (fst__ tested_result) (diff_seq SEQ.>< SEQ.singleton (third_ tested_result))

procWGridUpd :: SEQ.Seq ((Int, Int, Int), Maybe Obj_place) -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)]
procWGridUpd SEQ.Empty w_grid = []
procWGridUpd (x SEQ.:<| xs) w_grid = (fst x, (w_grid ! (fst x)) {obj = snd x}) : procWGridUpd xs w_grid

loadGameStateFile :: Int -> LBS.ByteString -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int])
                     -> SEQ.Seq ((Int, Int, Int), Maybe Obj_place) -> SEQ.Seq ((Int, Int, Int), Floor_grid) -> SEQ.Seq ((Int, Int, Int), (Int, [Int]))
                     -> Play_state0 -> Play_state1 -> Game_state
loadGameStateFile c bs w_grid f_grid obj_grid w_grid_upd f_grid_upd obj_grid_upd s0 s1 =
  if c == 0 then loadGameStateFile (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid
                                   (decodeSequence 0 def_obj_place_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
                                   f_grid_upd obj_grid_upd s0 s1
  else if c == 1 then loadGameStateFile (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid w_grid_upd
                                        (decodeSequence 0 def_f_grid_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
                                        obj_grid_upd  s0 s1
  else if c == 2 then loadGameStateFile (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid w_grid_upd f_grid_upd
                                        (decodeSequence 0 def_obj_grid_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
                                        s0 s1
  else if c == 3 then loadGameStateFile (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid w_grid_upd f_grid_upd
                                        obj_grid_upd ((decode (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs))) :: Play_state0) s1
  else if c == 4 then loadGameStateFile (c + 1) LBS.empty w_grid f_grid obj_grid w_grid_upd f_grid_upd obj_grid_upd s0
                                        ((decode (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs))) :: Play_state1)
  else Game_state {is_set = False, w_grid_ = w_grid // (procWGridUpd w_grid_upd w_grid), f_grid_ = f_grid // (FOLD.toList f_grid_upd),
                   obj_grid_ = obj_grid // (FOLD.toList obj_grid_upd), s0_ = s0, s1_ = s1}

-- If an error occurs while attempting to open a save game file the user is informed through the menu system.
loaderError :: SomeException -> Io_box -> IO LBS.ByteString
loaderError x box = do
  runMenu error_opening_file_text [] box (-0.75) (-0.75) 1 0 0 ps0_init 1
  putStr ("\nload_saved_game: " ++ show x)
  return LBS.empty

-- This function is the entry point to the game state saving logic and handles user input from the load game menu.
loadSavedGame :: Int -> [[Char]] -> [Char] -> Int -> Int -> Io_box -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
                 -> Array (Int, Int, Int) (Int, [Int]) -> Array Int [Char] -> IO (Maybe Game_state)
loadSavedGame 0 [] chosen_file c choice box w_grid f_grid obj_grid conf_reg = error "\nload_saved_game: encountered an unexpected log file structure."
loadSavedGame 0 ((y0:y1:y2:y3:y4:y5:y6:y7:y8:y9:y10:y11:y12:y13:y14:y15:y16:ys):xs) chosen_file c choice box w_grid f_grid obj_grid conf_reg = do
  if choice == 7 then return Nothing
  else if c == choice then loadSavedGame 1 [] [y1, y2, y3, y4, y5, y6, y7, y8, y9] c choice box w_grid f_grid obj_grid conf_reg
  else loadSavedGame 0 xs chosen_file (c + 1) choice box w_grid f_grid obj_grid conf_reg
loadSavedGame 1 [] chosen_file c choice box w_grid f_grid obj_grid conf_reg = do
  contents <- catch (do contents <- LBS.readFile ((cfg conf_reg 0 "game_save_path") ++ chosen_file); return contents) (\e -> loaderError e box)
  if LBS.length contents == 0 then return Nothing
  else return (Just (loadGameStateFile 0 contents w_grid f_grid obj_grid SEQ.Empty SEQ.Empty SEQ.Empty ps0_init ps1_init))

-- Sequential saves of the same game produce a sequence of save game files up to a preset maximum.
-- The automation of this feature is done in the two functions below.
addTimeStamp :: [[Char]] -> Play_state0 -> Int -> Int -> [Char]
addTimeStamp [] s0 save_slot i = []
addTimeStamp ((y0:y1:y2:y3:y4:y5:y6:y7:y8:y9:y10:y11:y12:y13:y14:y15:y16:ys):xs) s0 save_slot i =
  let file_name = ['_', y1, y2, y3, y4, y5, y6, y7, y8, y9, y10]
  in
  if i == save_slot then file_name ++ showGameTime (mod (fst__ (gameClock s0)) 1440000) [] False ++ "\n" ++ addTimeStamp xs s0 save_slot (i + 1)
  else [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16] ++ "\n" ++ addTimeStamp xs s0 save_slot (i + 1)

selectSaveFile :: [[Char]] -> Play_state0 -> Int -> ([Char], [Char])
selectSaveFile file_list s0 limit =
  let i = read ((file_list, 631) !! 0)
  in
  if i == limit then (take 9 (tail ((file_list, 632) !! limit)), "1\n" ++ init (addTimeStamp (tail file_list) s0 i 1))
  else (take 9 (tail ((file_list, 633) !! i)), show (i + 1) ++ "\n" ++ init (addTimeStamp (tail file_list) s0 i 1))

-- This class and the four other functions below deal with generating a save game file.
-- The Serialise_diff class is used so that the Obj_place type gets extracted from Wall_grid.  This is done because the rest of the Wall_grid structure is
-- not exposed to the GPLC interpreter and so cannot change during game play.
class Serialise_diff a where
  save_diff :: ((Int, Int, Int), a) -> LBS.ByteString

instance Serialise_diff Wall_grid where
  save_diff ((w, u, v), x) = LBS.append (encode (w, u, v)) (encode (obj x))

instance Serialise_diff Floor_grid where
  save_diff ((w, u, v), x) = LBS.append (encode (w, u, v)) (encode x)

instance Serialise_diff (Int, [Int]) where
  save_diff ((w, u, v), x) = LBS.append (encode (w, u, v)) (encode x)

saveArrayDiff0 :: Int -> ([Char], [Char]) -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> Array Int [Char]
                  -> Play_state0 -> IO ()
saveArrayDiff0 mode (save_file, save_log) w_grid_bstring f_grid_bstring obj_grid_bstring s0_bstring s1_bstring conf_reg s0 =
  let block0 = LBS.append (LBS.drop 8 w_grid_bstring)
      block1 = LBS.append (LBS.drop 8 f_grid_bstring)
      block2 = LBS.append (LBS.drop 8 obj_grid_bstring)
      block3 = LBS.append s0_bstring
  in
  if mode == 0 then do
    contents <- bracket (openFile "save_log.log" ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\nsave_log size: " ++ show (length c)); return c)
    saveArrayDiff0 1 (selectSaveFile (splitOn "\n" (tailFile contents)) s0 ((length (splitOn "\n" (tailFile contents))) - 1)) w_grid_bstring f_grid_bstring obj_grid_bstring
                   s0_bstring s1_bstring conf_reg s0
  else do
    h0 <- openFile "save_log.log" WriteMode
    hPutStr h0 save_log
    hClose h0
    LBS.writeFile (cfg conf_reg 0 "game_save_path" ++ save_file) (block0 $ block1 $ block2 $ block3 $ s1_bstring)
    putStr ("\n\nGame saved as: " ++ (cfg conf_reg 0 "game_save_path") ++ save_file)

wrappedSaveArrayDiff1 :: Serialise_diff a => SEQ.Seq ((Int, Int, Int), a) -> LBS.ByteString
wrappedSaveArrayDiff1 x =
  if SEQ.length x == 0 then LBS.append (encode (0 :: Int)) (encode (0 :: Int))
  else saveArrayDiff1 x LBS.empty 0 ((SEQ.length x) - 1)

saveArrayDiff1 :: Serialise_diff a => SEQ.Seq ((Int, Int, Int), a) -> LBS.ByteString -> Int -> Int -> LBS.ByteString
saveArrayDiff1 diff_seq diff_bytestring i limit =
  if i > limit then LBS.append (encode i) (LBS.append (encode (fromIntegral (LBS.length diff_bytestring) :: Int)) diff_bytestring)
  else saveArrayDiff1 diff_seq (LBS.append diff_bytestring (save_diff (SEQ.index diff_seq i))) (i + 1) limit

labelPlayStateEncoding :: LBS.ByteString -> LBS.ByteString
labelPlayStateEncoding x = LBS.append (encode (fromIntegral (LBS.length x) :: Int)) x

-- Find the uniform locations of GLSL uniform variables.
findGlUniform :: [[Char]] -> [Int] -> Ptr GLuint -> [Int32] -> IO [Int32]
findGlUniform [] [] p_gl_program acc = return acc
findGlUniform (x:xs) (y:ys) p_gl_program acc = do
  query <- newCString x
  gl_program <- peekElemOff p_gl_program y
  uniform <- glGetUniformLocation gl_program query
  free query
  findGlUniform xs ys p_gl_program (acc ++ [fromIntegral uniform])

-- These four functions deal with the compilation of the GLSL shaders and linking of the shader program.
makeGlProgram :: [[Char]] -> Ptr GLuint -> Int -> IO ()
makeGlProgram [] p_prog i = return ()
makeGlProgram (x0:x1:xs) p_prog i = do
  sc0 <- shaderCode ["#" ++ x0] nullPtr 0
  sc1 <- shaderCode ["#" ++ x1] nullPtr 0
  shader0 <- makeShader GL_VERTEX_SHADER sc0
  shader1 <- makeShader GL_FRAGMENT_SHADER sc1
  gl_program <- makeProgram [shader0, shader1] 0 0
  pokeElemOff p_prog i gl_program
  free sc0; free sc1
  makeGlProgram xs p_prog (i + 1)

shaderCode :: [[Char]] -> Ptr (Ptr GLchar) -> Int -> IO (Ptr (Ptr GLchar))
shaderCode sources ptr 0 = do
  p0 <- mallocBytes (ptr_size * length sources)
  shaderCode sources p0 1
shaderCode [] ptr c = return ptr
shaderCode (x:xs) ptr c = do
  p <- newCString x
  pokeByteOff ptr (ptr_size * (c - 1)) p
  shaderCode xs ptr (c + 1)

makeShader :: GLenum -> Ptr (Ptr GLchar) -> IO GLuint
makeShader t_shader source = do
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

makeProgram :: [GLuint] -> GLuint -> Int -> IO GLuint
makeProgram shaders program 0 = do
  prog <- glCreateProgram
  makeProgram shaders prog 1
makeProgram [] program c = do
  glLinkProgram program
  return program
makeProgram (x:xs) program c = do
  glAttachShader program x
  makeProgram xs program c

-- Check for errors during GLSL program compilation.
validateProg :: GLuint -> Int -> IO ()
validateProg program n = do
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
setPad :: Int -> Int
setPad tex_w =
  if mod tex_w 4 == 0 then 0
  else 4 - mod tex_w 4

loadBitmap0 :: [[Char]] -> [Object] -> [Char] -> [BS.ByteString] -> Int -> IO [BS.ByteString]
loadBitmap0 [] _ path acc c = return acc
loadBitmap0 (x:xs) (y:ys) path acc c = do
  if num_tex y == 0 then loadBitmap0 (x:xs) ys path acc 1
  else do
    contents <- BS.readFile (path ++ x)
    if c == (num_tex y) then do
      loadBitmap0 xs ys path (acc ++ [loadBitmap1 contents (BS.empty) (fromIntegral (tex_w y)) (fromIntegral (tex_h y)) 0 0]) 1
    else do
      loadBitmap0 xs (y:ys) path (acc ++ [loadBitmap1 contents (BS.empty) (fromIntegral (tex_w y)) (fromIntegral (tex_h y)) 0 0]) (c + 1)

loadBitmap1 :: BS.ByteString -> BS.ByteString -> Int -> Int -> Int -> Int -> BS.ByteString
loadBitmap1 bs0 bs1 w h pad y =
  if y == h then bs1
  else loadBitmap1 (BS.drop (3 * w) bs0) (BS.append bs1 (BS.take ((3 * w) - pad) bs0)) w h pad (y + 1)

loadBitmap2 :: GLsizei -> GLsizei -> Ptr CChar -> IO ()
loadBitmap2 w h p_tex = do
  glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGB) w h 0 GL_RGB GL_UNSIGNED_BYTE p_tex

-- These two functions load vertex data and bind OpenGL vertex array objects and texture objects, which are used to render all environmental models.
setupObject :: [Object] -> [[Int]] -> [Float] -> [GLushort] -> [BS.ByteString] -> Ptr GLuint -> IO ()
setupObject [] _ vertex element bs_tex p_bind = return ()
setupObject (x:xs) (y:ys) vertex element bs_tex p_bind = do
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
  loadArray (take ((y, 65) !! 1) (drop ((y, 66) !! 0) vertex)) p2 0
  loadArray (take ((y, 67) !! 3) (drop ((y, 68) !! 2) element)) p3 0
  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER a_buf
  glBufferData GL_ARRAY_BUFFER (unsafeCoerce (plusPtr zero_ptr (glfloat * (y, 69) !! 1)) :: GLsizeiptr) p2 GL_STREAM_DRAW
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER e_buf
  glBufferData GL_ELEMENT_ARRAY_BUFFER (unsafeCoerce (plusPtr zero_ptr (glushort * (y, 70) !! 3)) :: GLsizeiptr) p3 GL_STREAM_DRAW
  free p0; free p1; free p2; free p3
  glVertexAttribPointer 0 4 GL_FLOAT 0 0 zero_ptr
  if num_tex x > 0 then do
    glVertexAttribPointer 1 2 GL_FLOAT 0 0 (plusPtr zero_ptr ((att_offset x) * 4 * glfloat))
    bindTexture (take (num_tex x) bs_tex) (plusPtr p_bind ((ident x) * gluint + gluint)) (tex_w x) (tex_h x) 0
    glVertexAttribPointer 2 3 GL_FLOAT 0 0 (plusPtr zero_ptr ((att_offset x) * 6 * glfloat))
    glEnableVertexAttribArray 0
    glEnableVertexAttribArray 1
    glEnableVertexAttribArray 2
    setupObject xs ys vertex element (drop (num_tex x) bs_tex) p_bind
  else do
    glVertexAttribPointer 1 4 GL_FLOAT 0 0 (plusPtr zero_ptr ((att_offset x) * 4 * glfloat))
    glVertexAttribPointer 2 3 GL_FLOAT 0 0 (plusPtr zero_ptr ((att_offset x) * 8 * glfloat))
    glEnableVertexAttribArray 0
    glEnableVertexAttribArray 1
    glEnableVertexAttribArray 2
    setupObject xs ys vertex element bs_tex p_bind

bindTexture :: [BS.ByteString] -> Ptr GLuint -> GLsizei -> GLsizei -> Int -> IO ()
bindTexture [] p_bind w h offset = return ()
bindTexture (x:xs) p_bind w h offset = do
  glGenTextures 1 (plusPtr p_bind (offset * gluint))
  tex <- peek (plusPtr p_bind (offset * gluint))
  glBindTexture GL_TEXTURE_2D tex
  BS.useAsCString (BS.take (fromIntegral (w * h * 3)) (BS.reverse x)) (loadBitmap2 w h)
  glGenerateMipmap GL_TEXTURE_2D
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)
  bindTexture xs p_bind w h (offset + 1)

-- This function determines the number of vectors in either component of the mobile_lights field of Play_state0, which is used in certain calls
-- to OpenGL functions in showFrame.
detBufferLen :: Integral a => Play_state0 -> Int -> a -> a
detBufferLen s0 mode component_size =
  if mode == 0 then div (fromIntegral (length (fst (mobile_lights s0)))) component_size
  else div (fromIntegral (length (snd (mobile_lights s0)))) component_size

-- This function manages the rendering of all environmental models and in game messages.
-- It recurses once per frame rendered and is the central branching point of the rendering thread.
showFrame :: (UArray Int Word32, Int) -> UArray Int Int32 -> (Ptr GLfloat, Ptr GLfloat) -> (Ptr Int, Ptr Int) -> Float -> Float -> Float -> Int -> Int
             -> MVar (Play_state0, Array (Int, Int, Int) Wall_grid, Game_state) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
             -> Array (Int, Int, Int) (Int, [Int]) -> UArray (Int, Int) Float -> Matrix Float -> Array Int (Int, [Int]) -> SEQ.Seq Frame_record
             -> IO ([Int], Game_state, SEQ.Seq Frame_record)
showFrame p_bind uniform (p_mt_matrix, p_light_buffer) filter_table u v w a a' state_ref w_grid f_grid obj_grid lookUp camera_to_clip msg_queue frame_seq =
  let survey0 = multiSurvey (modAngle a (-92)) 183 u v (truncate u) (truncate v) w_grid f_grid obj_grid lookUp 2 0 [] []
      survey1 = multiSurvey (modAngle (modAngle a' a) 222) 183 (fst view_circle') (snd view_circle') (truncate (fst view_circle')) (truncate (snd view_circle'))
                            w_grid f_grid obj_grid lookUp 2 0 [] []
      view_circle' = viewCircle u v 2 (modAngle a a') lookUp
      world_to_clip0 = multStd camera_to_clip (worldToCamera (-u) (-v) (-w) a lookUp)
      world_to_clip1 = multStd camera_to_clip (worldToCamera (- (fst view_circle')) (- (snd view_circle')) (-w) (modAngle (modAngle a' a) 314) lookUp)
  in do
  p_state <- takeMVar state_ref
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  if view_mode (fst__ p_state) == 0 then loadArray (toList world_to_clip0) (castPtr p_mt_matrix) 0
  else do
    loadArray (toList world_to_clip1) (castPtr p_mt_matrix) 0
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 2)))
    glUniformMatrix4fv (coerce (uniform ! 40)) 1 1 (castPtr p_mt_matrix)
    glUniform1i (coerce (uniform ! 50)) (fromIntegral (mod (fst__ (gameClock (fst__ p_state))) 240))
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 1)))
    glUniformMatrix4fv (coerce (uniform ! 52)) 1 1 (castPtr p_mt_matrix)
    showPlayer uniform p_bind (plusPtr p_mt_matrix (glfloat * 80)) u v w a lookUp (rend_mode (fst__ p_state))
  if rend_mode (fst__ p_state) == 0 then do
    loadArray (fst (mobile_lights (fst__ p_state)) ++ snd (mobile_lights (fst__ p_state))) p_light_buffer 0
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 7)))
    if mobile_lights (fst__ p_state) == ([], []) then glUniform1i (coerce (uniform ! 56)) (fromIntegral 2)
    else do
      glUniform1i (coerce (uniform ! 56)) ((detBufferLen (fst__ p_state) 0 4) + 2)
      glUniform4fv (coerce (uniform ! 54)) (detBufferLen (fst__ p_state) 0 4) p_light_buffer
      glUniform3fv (coerce (uniform ! 55)) (detBufferLen (fst__ p_state) 0 3) (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (fst__ p_state)))))
    glUniform1i (coerce (uniform ! 9)) (fromIntegral (mod (fst__ (gameClock (fst__ p_state))) 240))
    glUniformMatrix4fv (coerce (uniform ! 1)) 1 1 (castPtr p_mt_matrix)
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 6)))
    if mobile_lights (fst__ p_state) == ([], []) then glUniform1i (coerce (uniform ! 59)) (fromIntegral 2)
    else do
      glUniform1i (coerce (uniform ! 59)) ((detBufferLen (fst__ p_state) 0 4) + 2)
      glUniform4fv (coerce (uniform ! 57)) (detBufferLen (fst__ p_state) 0 4) p_light_buffer
      glUniform3fv (coerce (uniform ! 58)) (detBufferLen (fst__ p_state) 0 3) (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (fst__ p_state)))))
    glUniform1i (coerce (uniform ! 20)) (fromIntegral (mod (fst__ (gameClock (fst__ p_state))) 240))
    glUniformMatrix4fv (coerce (uniform ! 12)) 1 1 (castPtr p_mt_matrix)
  else do
    loadArray ([3, 3, 3, 1] ++ fst (mobile_lights (fst__ p_state)) ++ [coerce u, coerce v, coerce w] ++ snd (mobile_lights (fst__ p_state))) p_light_buffer 0
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 5)))
    glUniform1i (coerce (uniform ! 62)) ((detBufferLen (fst__ p_state) 0 4) + 1)
    glUniform4fv (coerce (uniform ! 60)) ((detBufferLen (fst__ p_state) 0 4) + 1) p_light_buffer
    glUniform3fv (coerce (uniform ! 61)) ((detBufferLen (fst__ p_state) 0 3) + 1)
                 (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (fst__ p_state))) + 16))
    glUniformMatrix4fv (coerce (uniform ! 24)) 1 1 (castPtr p_mt_matrix)
    glUniform1i (coerce (uniform ! 27)) (fromIntegral (torch_t_limit (fst__ p_state) - (fst__ (gameClock (fst__ p_state)) - torch_t0 (fst__ p_state))))
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 4)))
    glUniform1i (coerce (uniform ! 65)) ((detBufferLen (fst__ p_state) 0 4) + 1)
    glUniform4fv (coerce (uniform ! 63)) ((detBufferLen (fst__ p_state) 0 4) + 1) p_light_buffer
    glUniform3fv (coerce (uniform ! 64)) ((detBufferLen (fst__ p_state) 0 3) + 1)
                 (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (fst__ p_state))) + 16))
    glUniformMatrix4fv (coerce (uniform ! 30)) 1 1 (castPtr p_mt_matrix)
    glUniform1i (coerce (uniform ! 33)) (fromIntegral (torch_t_limit (fst__ p_state) - (fst__ (gameClock (fst__ p_state)) - torch_t0 (fst__ p_state))))
  glBindVertexArray (unsafeCoerce ((fst p_bind) ! 0))
  if view_mode (fst__ p_state) == 0 then do
    filtered_surv0 <- filterSurv 0 (fst survey0) [] (fst filter_table) (third_ (gameClock (fst__ p_state)))
    filtered_surv1 <- filterSurv 1 (snd survey0) [] (snd filter_table) (third_ (gameClock (fst__ p_state)))
    showWalls filtered_surv0 uniform p_bind (plusPtr p_mt_matrix (glfloat * 16)) u v w a lookUp (rend_mode (fst__ p_state))
    showObject (ceiling_model : filtered_surv1) uniform p_bind (plusPtr p_mt_matrix (glfloat * 48)) u v w a lookUp (rend_mode (fst__ p_state))
  else do
    filtered_surv0 <- filterSurv 0 (fst survey1) [] (fst filter_table) (third_ (gameClock (fst__ p_state)))
    filtered_surv1 <- filterSurv 1 (snd survey1) [] (snd filter_table) (third_ (gameClock (fst__ p_state)))
    showWalls filtered_surv0 uniform p_bind (plusPtr p_mt_matrix (glfloat * 16)) u v w a lookUp (rend_mode (fst__ p_state))
    showObject (ceiling_model : filtered_surv1) uniform p_bind (plusPtr p_mt_matrix (glfloat * 48)) u v w a lookUp (rend_mode (fst__ p_state))
  msg_residue <- handleMessage0 (handleMessage1 (message_ (fst__ p_state)) msg_queue 0 3) uniform p_bind 0
  if fst msg_residue == 1 || fst msg_residue == 3 || fst msg_residue == 4 || fst msg_residue == 5 then return ([fst msg_residue], third_ p_state, frame_seq)
  else if fst msg_residue == 2 then do
    threadDelay 5000000
    return ([2], third_ p_state, frame_seq)
  else if SEQ.length frame_seq == 10800 then return ([1], third_ p_state, frame_seq)
  else do
    swapBuffers
    showFrame p_bind uniform (p_mt_matrix, p_light_buffer) filter_table (pos_u (fst__ p_state)) (pos_v (fst__ p_state)) (pos_w (fst__ p_state))
              (angle (fst__ p_state)) (view_angle (fst__ p_state)) state_ref (snd__ p_state) f_grid obj_grid lookUp camera_to_clip (snd msg_residue)
              (frame_seq SEQ.>< SEQ.singleton Frame_record {pos_u_r = pos_u (fst__ p_state), pos_v_r = pos_v (fst__ p_state), pos_w_r = pos_w (fst__ p_state),
              view_mode_r = view_mode (fst__ p_state), control_key_r = control_key (fst__ p_state)})

-- These two functions iterate through the message queue received from the game logic thread.  They manage the appearance and expiry of on screen messages
-- and detect special event messages, such as are received when the user opts to return to the main menu.
handleMessage1 :: [(Int, [Int])] -> Array Int (Int, [Int]) -> Int -> Int -> (Int, Array Int (Int, [Int]))
handleMessage1 [] msg_queue i0 i1 = (0, msg_queue)
handleMessage1 (x:xs) msg_queue i0 i1 =
  if fst x < 0 then (abs (fst x), msg_queue)
  else if head (snd x) == -1 then
    if fst (msg_queue ! i1) == 0 && i1 < 5 then handleMessage1 xs (msg_queue // [(i1, x)]) i0 (i1 + 1)
    else if fst (msg_queue ! i1) == 0 && i1 == 5 then handleMessage1 xs (msg_queue // [(i1, x)]) i0 3
    else if fst (msg_queue ! i1) > 0 && i1 < 5 then handleMessage1 (x:xs) msg_queue i0 (i1 + 1)
    else handleMessage1 xs (msg_queue // [(3, x)]) i0 4
  else
    if fst (msg_queue ! i0) == 0 && i0 < 2 then handleMessage1 xs (msg_queue // [(i0, x)]) (i0 + 1) i1
    else if fst (msg_queue ! i0) == 0 && i0 == 2 then handleMessage1 xs (msg_queue // [(i0, x)]) 0 i1
    else if fst (msg_queue ! i0) > 0 && i0 < 2 then handleMessage1 (x:xs) msg_queue (i0 + 1) i1
    else handleMessage1 xs (msg_queue // [(0, x)]) 1 i1

handleMessage0 :: (Int, Array Int (Int, [Int])) -> UArray Int Int32 -> (UArray Int Word32, Int) -> Int -> IO (Int, Array Int (Int, [Int]))
handleMessage0 msg_queue uniform p_bind i =
  let h_pos = \x -> if x < 3 then -0.96
                    else 0.64
      msg_queue_ = (snd msg_queue) ! i
  in do
  if fst msg_queue > 0 then return msg_queue
  else if i > 5 then return msg_queue
  else if fst ((snd msg_queue) ! i) > 0 then do
    showText (tail (snd ((snd msg_queue) ! i))) 0 933 uniform p_bind (h_pos i) (0.9 - 0.05 * fromIntegral (mod i 3)) zero_ptr
    handleMessage0 (0, (snd msg_queue) // [(i, ((fst msg_queue_) - 1, snd msg_queue_))]) uniform p_bind (i + 1)
  else handleMessage0 msg_queue uniform p_bind (i + 1)

-- These three functions pass transformation matrices to the shaders and make the GL draw calls that render models.
showWalls :: [Wall_place] -> UArray Int Int32 -> (UArray Int Word32, Int) -> Ptr GLfloat -> Float -> Float -> Float -> Int -> UArray (Int, Int) Float -> Int
             -> IO ()
showWalls [] uniform p_bind p_mt_matrix u v w a lookUp mode = return ()
showWalls (x:xs) uniform p_bind p_mt_matrix u v w a lookUp mode = do
  if isNull x == False then do
    loadArray (toList (modelToWorld (translate_u x) (translate_v x) (translate_w x) 0 False lookUp)) (castPtr p_mt_matrix) 0
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
      showWalls xs uniform p_bind p_mt_matrix u v w a lookUp mode
    if Build_model.rotate x < 2 then glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT (plusPtr zero_ptr (glushort * 36))
    else glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT zero_ptr
    showWalls xs uniform p_bind p_mt_matrix u v w a lookUp mode
  else showWalls xs uniform p_bind p_mt_matrix u v w a lookUp mode

showObject :: [Obj_place] -> UArray Int Int32 -> (UArray Int Word32, Int) -> Ptr GLfloat -> Float -> Float -> Float -> Int -> UArray (Int, Int) Float -> Int
              -> IO ()
showObject [] uniform p_bind p_mt_matrix u v w a lookUp mode = return ()
showObject (x:xs) uniform p_bind p_mt_matrix u v w a lookUp mode = do
  loadArray (toList (modelToWorld (u__ x) (v__ x) (w__ x) 0 False lookUp)) (castPtr p_mt_matrix) 0
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
    showObject xs uniform p_bind p_mt_matrix u v w a lookUp mode
  glBindVertexArray (unsafeCoerce ((fst p_bind) ! (ident_ x)))
  glDrawElements GL_TRIANGLES (coerce (num_elem x)) GL_UNSIGNED_SHORT zero_ptr
  showObject xs uniform p_bind p_mt_matrix u v w a lookUp mode

showPlayer :: UArray Int Int32 -> (UArray Int Word32, Int) -> Ptr GLfloat -> Float -> Float -> Float -> Int -> UArray (Int, Int) Float -> Int -> IO ()
showPlayer uniform p_bind p_mt_matrix u v w a lookUp mode = do
  loadArray (toList (modelToWorld u v w a True lookUp)) (castPtr p_mt_matrix) 0
  loadArray (toList (worldToModel u v w a True lookUp)) (castPtr p_mt_matrix) 16
  loadArray (toList (rotationW a lookUp)) (castPtr p_mt_matrix) 32
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


