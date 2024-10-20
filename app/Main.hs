-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude hiding ((!!))
import IndexWrapper0
import System.IO
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Graphics.GL.Core33
import Graphics.UI.GLUT hiding (Object, Matrix, GLuint, GLchar, GLenum, GLsizei, GLushort, GLsizeiptr, GLfloat, None)
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
import OpenMap
import DecompressMap
import BuildModel
import GameLogic
import GameSound
import SaveGame

patchConfReg :: [[Char]] -> Array Int [Char] -> Array Int [Char]
patchConfReg [] conf_reg = conf_reg
patchConfReg (x0:x1:xs) conf_reg =
  patchConfReg xs (updateCfg conf_reg (drop 2 x0) x1 0)

main = do
  args <- getArgs
  if length args == 0 then do
    contents <- bracket (openFile "config.txt" ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\ncfg file size: " ++ show (length c)); return c)
    openWindow (listArray (0, 97) (splitOneOf "=\n" (tailFile contents)))
  else if length args == 1 then do
    contents <- bracket (openFile ((args, 1) !! 0) ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\ncfg file size: " ++ show (length c)); return c)
    openWindow (listArray (0, 97) (splitOneOf "=\n" (tailFile contents)))
  else if length args > 2 && (args, 655) !! 1 == "--debugSet" then do
    contents <- bracket (openFile ((args, 656) !! 0) ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\ncfg file size: " ++ show (length c)); return c)
    openWindow (listArray (0, 97 - 2 + length args) (splitOneOf "=\n" (tailFile contents) ++ drop 2 args))
  else do
    contents <- bracket (openFile ((args, 657) !! 0) ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\ncfg file size: " ++ show (length c)); return c)
    openWindow (patchConfReg (drop 1 args) (listArray (0, 97) (splitOneOf "=\n" (tailFile contents))))

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
  putStr ("\n\nGame :: Dangerous engine starting.  Version and platform: " ++ cfg' "version_and_platform_string")
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
  setupGame_ conf_reg (tailFile contents) screen_res control_ref

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

-- This was added to implement auto alignment of the ray caster field of view with the frustum field of view.
-- 4 : 3 and 16 : 9 aspect ratios are supported.
setupGame_ :: Array Int [Char] -> [Char] -> Size -> IORef Int -> IO ()
setupGame_ conf_reg comp_map_text (Size w h) control_ref
  | fromIntegral w / fromIntegral h > 1.77 = do
    putStr ("\nResolution: " ++ show w ++ " * " ++ show h)
    putStr ("\nField of view: " ++ field_of_view 190)
    setupGame (conf_reg'' (conf_reg' conf_reg (-95)) 190) comp_map_text (Size w h) control_ref
  | otherwise = do
    putStr ("\nResolution: " ++ show w ++ " * " ++ show h)
    putStr ("\nField of view: " ++ field_of_view 160)
    setupGame (conf_reg'' (conf_reg' conf_reg (-80)) 160) comp_map_text (Size w h) control_ref
  where field_of_view = \angular_size -> take 5 (show ((angular_size / 629) * 360)) ++ " degrees"
        conf_reg' = \conf_reg survey_start -> updateCfg conf_reg "survey_start" (show survey_start) 0
        conf_reg'' = \conf_reg survey_size -> updateCfg conf_reg "survey_size" (show survey_size) 0

-- This function initialises the OpenAL context, decompresses the map file, manages the compilation of GLSL shaders, loading of 3D models, loading of the
-- light map and loading of sound effects.
setupGame :: Array Int [Char] -> [Char] -> Size -> IORef Int -> IO ()
setupGame conf_reg comp_map_text (Size w h) control_ref =
  let cfg' = cfg conf_reg 0
      m0 = "mod_to_world"
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
      u_max = read (((splitOn "\n~\n" comp_map_text), 3) !! 12)
      v_max = read (((splitOn "\n~\n" comp_map_text), 4) !! 13)
      w_max = read (((splitOn "\n~\n" comp_map_text), 5) !! 14)
      proc_map' = procMap (splitOn "\n~\n" comp_map_text) u_max v_max w_max
      pm'' = fst proc_map'
      pm''' = snd proc_map'
      mc = \i -> ((splitOn "\n~\n" comp_map_text), 637) !! i
      map_text = ".~.~.~.~" ++ pm'' ++ "~" ++ pm''' ++ (mc 10) ++ "~" ++ (mc 11) ++ "~" ++ (mc 12) ++ "~" ++ (mc 13) ++ "~" ++ (mc 14) ++ "~" ++ (mc 15)
      p_bind_limit = (read (((splitOn "\n~\n" comp_map_text), 11) !! 7)) - 1
      frustumScale0 = (read (cfg' "frustumScale1")) / (fromIntegral w / fromIntegral h)
      physics = GamePhysics {u = read (cfg' "init_u"), v = read (cfg' "init_v"), w = read (cfg' "init_w"),
                             gravity = read (cfg' "gravity"), friction = read (cfg' "friction"), mag_run = read (cfg' "run_power"),
                             mag_jump = read (cfg' "jump_power")}
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
  mod_bind <- callocBytes ((read (((splitOn "\n~\n" comp_map_text), 42) !! 7)) * gluint)
  loadModFile (init (splitOn ", " (((splitOn "\n~\n" comp_map_text), 43) !! 8))) (cfg' "model_data_dir") mod_bind
  free p_gl_program; free p_lmap_pos0; free p_lmap_pos1; free p_lmap_int0; free p_lmap_int1; free p_lmap_t0; free p_lmap_t1
  p_bind_ <- bufferToArray (castPtr mod_bind) p_bind 0 0 (p_bind_limit - 16)
  initAlContext
  contents2 <- bracket (openFile ((cfg' "sound_data_dir") ++ (last (splitOn ", " (mc 8)))) ReadMode) (hClose)
                       (\h -> do c <- hGetContents h; putStr ("\nsound map size: " ++ show (length c)); return c)
  sound_array <- initAlEffect0 (splitOneOf "\n " (tailFile contents2)) (cfg' "sound_data_dir")
                               (array (0, (div (length (splitOneOf "\n " (tailFile contents2))) 2) - 1) [(x, Source 0) | x <- [0..(div (length (splitOneOf "\n " (tailFile contents2))) 2) - 1]])
  r_gen <- getStdGen
  startGame NewGame physics control_ref (listArray (0, 65) uniform) (p_bind_, p_bind_limit + 1) map_text conf_reg
            sound_array (cameraToClip frustumScale0 (read (cfg' "frustumScale1"))) r_gen

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

-- This function generates the camera to clip transformation matrix that will be passed to the shaders.
cameraToClip :: Float -> Float -> Matrix Float
cameraToClip frustumScale0 frustumScale1 =
  let y = \x -> fromList 4 4 x
  in
  y [frustumScale0, 0, 0, 0, 0, frustumScale1, 0, 0, 0, 0, ((zFar + zNear) / (zNear - zFar)), ((2 * zFar * zNear) / (zNear - zFar)), 0, 0, -1, 0]

setCurrentMap :: [Char] -> Int
setCurrentMap map_file = read [(map_file, 659) !! 3] :: Int

-- This function initialises the game logic thread each time a new game is started and handles user input from the main menu.
startGame :: RandomGen g => EventContext -> GamePhysics -> IORef Int -> UArray Int Int32 -> (UArray Int Word32, Int) -> [Char] -> Array Int [Char]
             -> Array Int Source -> Matrix Float -> g -> IO ()
startGame context physics control_ref uniform p_bind map_text conf_reg sound_array camera_to_clip r_gen
  | context == NewGame || context == LoadGame = do
    putStr ("\nEngine event: " ++ show context)
    p_mt_matrix <- mallocBytes (glfloat * 128)
    p_f_table0 <- callocBytes (int_ * 120000)
    p_f_table1 <- callocBytes (int_ * 37500)
    p_light_buffer <- mallocBytes (glfloat * 35)
    state_ref <- newEmptyMVar
    t_log <- newEmptyMVar
    result_ <- newEmptyMVar
    if context == NewGame then do
      tid <- forkIO (updatePlayWrapper0 (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = Just control_ref}) state_ref
                                        game_state False (read (cfg' "min_frame_t")) physics
                                        look_up_ (sound_array, setup_music) 0 t_log (SEQ.empty) 60)
      result <- showFrame p_bind uniform (p_mt_matrix, p_light_buffer) (p_f_table0, p_f_table1) 0 0 0 0 0 state_ref w_grid f_grid obj_grid look_up_
                          camera_to_clip (array (0, 5) [(i, (0, [])) | i <- [0..5]]) conf_reg
      killThread tid
      putMVar result_ result
    else do
      contents <- bracket (openFile "save_log.log" ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\nsave_log size: " ++ show (length c)); return c)
      state_choice <- runMenu (read (cfg' "current_save")) (genLoadMenu (tail (splitOn "\n" (tailFile contents))) [] 1) []
                              (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = Just control_ref}) (-0.75) (-0.75) 1 0 0 ps0_init 1
      loaded_state <- loadSavedGame 0 (tail (splitOn "\n" (tailFile contents))) [] 1 state_choice
                                      (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = Just control_ref})
                                      w_grid f_grid obj_grid conf_reg load_transform (cfg' "map_file")
      if isNothing loaded_state then do
        free p_mt_matrix
        free p_f_table0
        free p_f_table1
        free p_light_buffer
        startGame ReturnMainMenu physics control_ref uniform p_bind map_text conf_reg sound_array camera_to_clip r_gen
      else if isNothing loaded_state == False && currentMap (s0_ (fromJust loaded_state)) == setCurrentMap (cfg' "map_file") then do
        tid <- forkIO (updatePlayWrapper0 (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = Just control_ref}) state_ref
                                          (fromJust loaded_state) False (read (cfg' "min_frame_t")) physics
                                          look_up_ (sound_array, setup_music) 0 t_log (SEQ.empty) 60)
        result <- showFrame p_bind uniform (p_mt_matrix, p_light_buffer) (p_f_table0, p_f_table1) 0 0 0 0 0 state_ref w_grid f_grid obj_grid look_up_
                            camera_to_clip (array (0, 5) [(i, (0, [])) | i <- [0..5]]) conf_reg
        killThread tid
        putMVar result_ result
      else do
        putStr ("\nMap file required: " ++ show (currentMap (s0_ (fromJust loaded_state))) ++ "\n")
        exitSuccess
    free p_mt_matrix
    free p_f_table0
    free p_f_table1
    free p_light_buffer
    result <- takeMVar result_
    if event_context result == SaveGame then do
      putStr "\nEngine event: SaveGame"
      save_index <- saveGame (genArrayDiff (-3) 0 0 u_limit v_limit save_transform w_grid (w_grid_ result) SEQ.empty) (serialiseWGridDiffs (w_grid_save result))
                             (genArrayDiff 0 0 0 ((div (u_limit + 1) 2) - 1) ((div (v_limit + 1) 2) - 1) save_transform f_grid (f_grid_ result) SEQ.empty)
                             (serialiseFGridDiffs (f_grid_save result))
                             (genArrayDiff 0 0 0 u_limit v_limit save_transform obj_grid (obj_grid_ result) SEQ.empty) (serialiseObjGridDiffs (obj_grid_save result))
                             (s0_ result) (s1_ result) conf_reg
      if currentMap (s0_ result) == setCurrentMap (cfg' "map_file") then
        startGame LoadGame physics control_ref uniform p_bind map_text (updateCfg conf_reg "current_save" (show save_index) 0) sound_array camera_to_clip r_gen
      else do
        putStr ("\nMap file required: " ++ show (currentMap (s0_ result)) ++ "\n")
        exitSuccess
    else startGame (event_context result) physics control_ref uniform p_bind map_text conf_reg sound_array camera_to_clip r_gen
  | context == ReturnMainMenu = do
    putStr ("\nEngine event: " ++ show context)
    choice <- runMenu (-1) mainMenuText [] (Io_box {uniform_ = uniform, p_bind_ = p_bind, control_ = Just control_ref}) (-0.75) (-0.75) 1 0 0 ps0_init 1
    if choice == 1 then startGame NewGame physics control_ref uniform p_bind map_text conf_reg sound_array camera_to_clip r_gen
    else if choice == 2 then
      startGame LoadGame physics control_ref uniform p_bind map_text (updateCfg conf_reg "current_save" "-1" 0) sound_array camera_to_clip r_gen
    else exitSuccess
  | context == ExitGame = do
    putStr ("\nEngine event: " ++ show context)
    exitSuccess
  | otherwise = error ("\nInvalid engine event within startGame: " ++ show context)
  where cfg' = cfg conf_reg 0
        u_limit = (read (((splitOn "~" map_text), 56) !! 8))
        v_limit = (read (((splitOn "~" map_text), 57) !! 9))
        w_limit = (read (((splitOn "~" map_text), 58) !! 10))
        look_up_ = lookUp [makeTable 0 0, makeTable 1 0, makeTable 2 0, makeTable 3 0]
        setup_music = if cfg' "music" == "off" then 0
                      else read (cfg' "music_period")
        map = openMap 0 map_text u_limit v_limit w_limit conf_reg
        w_grid = fst__ map
        f_grid = snd__ map
        obj_grid = third_ map
        s0 = ps0_init {pos_u = u physics, pos_v = v physics, pos_w = w physics, on_screen_metrics = selectMetricMode (cfg' "on_screen_metrics"),
                       prob_seq = genProbSeq 0 239 (read (cfg' "prob_c")) r_gen, currentMap = setCurrentMap (cfg' "map_file")}
        s1 = if cfg' "verbose_mode" == "n" || cfg' "verbose_mode" == "y" then ps1_init {verbose_mode = cfg' "verbose_mode"}
             else ps1_init {verbose_mode = "filter", debugSet = array (0, snd (bounds conf_reg) - 92) [(i, conf_reg ! (i + 92)) | i <- [0..snd (bounds conf_reg) - 92]]}
        game_state = Game_state {event_context = None, w_grid_ = w_grid, f_grid_ = f_grid, obj_grid_ = obj_grid, s0_ = s0, s1_ = s1,
                                 w_grid_save = emptyWGridDiffContainer, f_grid_save = emptyFGridDiffContainer,
                                 obj_grid_save = emptyObjGridDiffContainer}
        save_transform = detMapTransform (cfg' "map_file") "save" u_limit v_limit
        load_transform = detMapTransform (cfg' "map_file") "load" u_limit v_limit

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

-- Find the uniform locations of GLSL uniform variables.
findGlUniform :: [[Char]] -> [Int] -> Ptr GLuint -> [Int32] -> IO [Int32]
findGlUniform [] [] p_gl_program acc = return acc
findGlUniform (x:xs) (y:ys) p_gl_program acc = do
  query <- newCString x
  gl_program <- peekElemOff p_gl_program y
  uniform <- glGetUniformLocation gl_program query
  free query
  findGlUniform xs ys p_gl_program (acc ++ [fromIntegral uniform])

-- These four functions deal with the compilation of the GLSL shaders and linking of the shader programs.
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
             -> MVar Game_state -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
             -> Array (Int, Int, Int) Obj_grid -> UArray (Int, Int) Float -> Matrix Float -> Array Int (Int, [Int])
             -> Array Int [Char] -> IO Game_state
showFrame p_bind uniform (p_mt_matrix, p_light_buffer) filter_table u v w a a' state_ref w_grid f_grid obj_grid lookUp camera_to_clip msg_queue
          conf_reg =
  let survey_start = read (cfg' "survey_start")
      survey_size = read (cfg' "survey_size")
      survey0 = multiSurvey (modAngle a survey_start) survey_size u v (truncate u) (truncate v) w_grid f_grid obj_grid lookUp 2 0 [] []
      survey1 = multiSurvey (modAngle (modAngle a' a) (survey_start + 314)) survey_size (fst view_circle') (snd view_circle')
                            (truncate (fst view_circle')) (truncate (snd view_circle')) w_grid f_grid obj_grid lookUp 2 0 [] []
      view_circle' = viewCircle u v 2 (modAngle a a') lookUp
      world_to_clip0 = multStd camera_to_clip (worldToCamera (-u) (-v) (-w) a lookUp)
      world_to_clip1 = multStd camera_to_clip (worldToCamera (- (fst view_circle')) (- (snd view_circle')) (-w) (modAngle (modAngle a' a) 314) lookUp)
      cfg' = cfg conf_reg 0
  in do
  game_state <- takeMVar state_ref
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  if view_mode (s0_ game_state) == 0 then loadArray (toList world_to_clip0) (castPtr p_mt_matrix) 0
  else do
    loadArray (toList world_to_clip1) (castPtr p_mt_matrix) 0
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 2)))
    glUniformMatrix4fv (coerce (uniform ! 40)) 1 1 (castPtr p_mt_matrix)
    glUniform1i (coerce (uniform ! 50)) (fromIntegral (mod (fst__ (gameClock (s0_ game_state))) 240))
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 1)))
    glUniformMatrix4fv (coerce (uniform ! 52)) 1 1 (castPtr p_mt_matrix)
    showPlayer uniform p_bind (plusPtr p_mt_matrix (glfloat * 80)) u v w a lookUp (rend_mode (s0_ game_state))
  if rend_mode (s0_ game_state) == 0 then do
    loadArray (fst (mobile_lights (s0_ game_state)) ++ snd (mobile_lights (s0_ game_state))) p_light_buffer 0
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 7)))
    if mobile_lights (s0_ game_state) == ([], []) then glUniform1i (coerce (uniform ! 56)) (fromIntegral 2)
    else do
      glUniform1i (coerce (uniform ! 56)) ((detBufferLen (s0_ game_state) 0 4) + 2)
      glUniform4fv (coerce (uniform ! 54)) (detBufferLen (s0_ game_state) 0 4) p_light_buffer
      glUniform3fv (coerce (uniform ! 55)) (detBufferLen (s0_ game_state) 0 3) (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (s0_ game_state)))))
    glUniform1i (coerce (uniform ! 9)) (fromIntegral (mod (fst__ (gameClock (s0_ game_state))) 240))
    glUniformMatrix4fv (coerce (uniform ! 1)) 1 1 (castPtr p_mt_matrix)
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 6)))
    if mobile_lights (s0_ game_state) == ([], []) then glUniform1i (coerce (uniform ! 59)) (fromIntegral 2)
    else do
      glUniform1i (coerce (uniform ! 59)) ((detBufferLen (s0_ game_state) 0 4) + 2)
      glUniform4fv (coerce (uniform ! 57)) (detBufferLen (s0_ game_state) 0 4) p_light_buffer
      glUniform3fv (coerce (uniform ! 58)) (detBufferLen (s0_ game_state) 0 3) (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (s0_ game_state)))))
    glUniform1i (coerce (uniform ! 20)) (fromIntegral (mod (fst__ (gameClock (s0_ game_state))) 240))
    glUniformMatrix4fv (coerce (uniform ! 12)) 1 1 (castPtr p_mt_matrix)
  else do
    loadArray ([3, 3, 3, 1] ++ fst (mobile_lights (s0_ game_state)) ++ [coerce u, coerce v, coerce w] ++ snd (mobile_lights (s0_ game_state))) p_light_buffer 0
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 5)))
    glUniform1i (coerce (uniform ! 62)) ((detBufferLen (s0_ game_state) 0 4) + 1)
    glUniform4fv (coerce (uniform ! 60)) ((detBufferLen (s0_ game_state) 0 4) + 1) p_light_buffer
    glUniform3fv (coerce (uniform ! 61)) ((detBufferLen (s0_ game_state) 0 3) + 1)
                 (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (s0_ game_state))) + 16))
    glUniformMatrix4fv (coerce (uniform ! 24)) 1 1 (castPtr p_mt_matrix)
    glUniform1i (coerce (uniform ! 27)) (fromIntegral (torch_t_limit (s0_ game_state) - (fst__ (gameClock (s0_ game_state)) - torch_t0 (s0_ game_state))))
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 4)))
    glUniform1i (coerce (uniform ! 65)) ((detBufferLen (s0_ game_state) 0 4) + 1)
    glUniform4fv (coerce (uniform ! 63)) ((detBufferLen (s0_ game_state) 0 4) + 1) p_light_buffer
    glUniform3fv (coerce (uniform ! 64)) ((detBufferLen (s0_ game_state) 0 3) + 1)
                 (plusPtr p_light_buffer (glfloat * length (fst (mobile_lights (s0_ game_state))) + 16))
    glUniformMatrix4fv (coerce (uniform ! 30)) 1 1 (castPtr p_mt_matrix)
    glUniform1i (coerce (uniform ! 33)) (fromIntegral (torch_t_limit (s0_ game_state) - (fst__ (gameClock (s0_ game_state)) - torch_t0 (s0_ game_state))))
  glBindVertexArray (unsafeCoerce ((fst p_bind) ! 0))
  if view_mode (s0_ game_state) == 0 then do
    filtered_surv0 <- filterSurv 0 (fst survey0) [] (fst filter_table) (third_ (gameClock (s0_ game_state)))
    filtered_surv1 <- filterSurv 1 (snd survey0) [] (snd filter_table) (third_ (gameClock (s0_ game_state)))
    showWalls filtered_surv0 uniform p_bind (plusPtr p_mt_matrix (glfloat * 16)) u v w a lookUp (rend_mode (s0_ game_state))
    showObject (ceiling_model : filtered_surv1) uniform p_bind (plusPtr p_mt_matrix (glfloat * 48)) u v w a lookUp (rend_mode (s0_ game_state))
  else do
    filtered_surv0 <- filterSurv 0 (fst survey1) [] (fst filter_table) (third_ (gameClock (s0_ game_state)))
    filtered_surv1 <- filterSurv 1 (snd survey1) [] (snd filter_table) (third_ (gameClock (s0_ game_state)))
    showWalls filtered_surv0 uniform p_bind (plusPtr p_mt_matrix (glfloat * 16)) u v w a lookUp (rend_mode (s0_ game_state))
    showObject (ceiling_model : filtered_surv1) uniform p_bind (plusPtr p_mt_matrix (glfloat * 48)) u v w a lookUp (rend_mode (s0_ game_state))
  msg_residue <- handleMessage0 (handleMessage1 (message_ (s0_ game_state)) msg_queue 0 3) uniform p_bind 0
  if event_context game_state == PlayerDied then do
    threadDelay 5000000
    return game_state {event_context = ReturnMainMenu}
  else if event_context game_state == None then do
    swapBuffers
    showFrame p_bind uniform (p_mt_matrix, p_light_buffer) filter_table (pos_u (s0_ game_state)) (pos_v (s0_ game_state)) (pos_w (s0_ game_state))
              (angle (s0_ game_state)) (view_angle (s0_ game_state)) state_ref (w_grid_ game_state) f_grid obj_grid lookUp camera_to_clip (snd msg_residue)
              conf_reg
  else return game_state

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
    if BuildModel.rotate x < 2 then glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT (plusPtr zero_ptr (glushort * 36))
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


