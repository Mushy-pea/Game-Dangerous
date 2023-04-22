-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- The Game :: Dangerous map development server, designed to be used with the forthcoming map
-- development client.

module Main where

import System.IO
import System.Environment
import Control.Exception
import Data.List.Split
import Data.Maybe
import Data.Array.IArray
import BuildModel hiding (Game_state, w_grid_, f_grid_, obj_grid_)
import DecompressMap
import CompressMap
import OpenMap
import HandleInput
import CompileGPLC
import PreprocessMap

loadMap :: [Char] -> Int -> Int -> Int -> (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) Obj_grid)
loadMap comp_map_text u_max v_max w_max =
  let proc_map' = procMap (splitOn "\n~\n" comp_map_text) u_max v_max w_max
      pm'' = fst proc_map'
      pm''' = snd proc_map'
      mc = \i -> (splitOn "\n~\n" comp_map_text) !! i
      map_text = ".~.~.~.~" ++ pm'' ++ "~" ++ pm''' ++ mc 10 ++ "~" ++ mc 11 ++ "~" ++ mc 12 ++ "~" ++ mc 13 ++ "~" ++ mc 14 ++ "~" ++ mc 15
  in openMap map_text u_max v_max w_max

main = do
  args <- getArgs
  putStr "\nGame :: Dangerous map development server version 0.5.0.0"
  putStr "\nLoading map file..."
  comp_map_text <- bracket (openFile ((args !! 0) ++ (args !! 1)) ReadMode) hClose
                   (\h -> do c <- hGetContents h; putStr ("\nmap file size: " ++ show (length c)); return c)
  handleInput Nothing comp_map_text (args !! 0) []

handleInput :: Maybe Server_state -> [Char] -> [Char] -> [[Char]] -> IO ()
handleInput server_state comp_map_text base_dir command
  | isNothing server_state = do
    prog_set <- bracket (openFile (base_dir ++ "GPLC_Programs.txt") ReadMode) hClose
                        (\h -> do c <- hGetContents h; putStr ("\nprogram list file size: " ++ show (length c)); return c)
    gplc_programs <- loadGplcPrograms (splitOn "\n" prog_set)
                                      "C:\\Users\\steve\\code\\GD\\GPLC-scripts-and-maps\\GPLC_Programs\\"
                                      (array (0, length (splitOn "\n" prog_set) - 1) [(i, empty_gplc_program) | i <- [0..length (splitOn "\n" prog_set) - 1]])
                                      0
    handleInput (Just Server_state {w_grid_ = fst__ new_game_state,
                                    f_grid_ = snd__ new_game_state,
                                    obj_grid_ = third_ new_game_state,
                                    gplcPrograms = gplc_programs})
                comp_map_text base_dir command
  | command == [] = do
      putStr "\nReady for request: "
      request <- getLine
      handleInput server_state comp_map_text base_dir (splitOn " " request)
  | head command == "exit" = putStr "\nExit command received ... closing server."
  | head command == "save" = do
    template <- bracket (openFile (base_dir ++ command !! 1) ReadMode) hClose
                        (\h -> do c <- hGetContents h; putStr ("\ntemplate file size: " ++ show (length c)); return c)
    h <- openFile (base_dir ++ command !! 2) WriteMode
    hPutStr h (encoded_wall_grid ++ encoded_floor_grid ++ template ++ encoded_obj_grid ++ encoded_sub_wall_grid ++ footer)
    hClose h
  | otherwise = do
      result <- applyCommand (fromJust server_state) command
      putStr ("\nresult: " ++ snd result)
      handleInput (Just (fst result)) comp_map_text base_dir []
  where u_max = read ((splitOn "\n~\n" comp_map_text) !! 12)
        v_max = read ((splitOn "\n~\n" comp_map_text) !! 13)
        w_max = read ((splitOn "\n~\n" comp_map_text) !! 14)
        new_game_state = loadMap comp_map_text u_max v_max w_max
        w_bd = bounds (w_grid_ (fromJust server_state))
        f_bd = bounds (f_grid_ (fromJust server_state))
        server_state' = applyAugmentations (fromJust server_state) (snd__ (snd w_bd), third_ (snd w_bd)) (snd__ (snd f_bd), third_ (snd f_bd))
        encoded_wall_grid = encodeWallGrid (w_grid_ server_state') 0 0 0 (snd__ (snd w_bd)) (third_ (snd w_bd)) []
        encoded_floor_grid = encodeFloorGrid (f_grid_ server_state') 0 0 0 (snd__ (snd f_bd)) (third_ (snd f_bd)) []
        encoded_obj_grid = encodeObjGrid (obj_grid_ server_state') 0 0 0 (snd__ (snd w_bd)) (third_ (snd w_bd)) []
        encoded_sub_wall_grid = encodeSubWallGrid (w_grid_ server_state') (-1) 0 0 (snd__ (snd w_bd)) (third_ (snd w_bd)) True []
        footer = show (snd__ (snd w_bd)) ++ "\n~\n" ++ show (third_ (snd w_bd)) ++ "\n~\n2\n~\nunlocked"

-- All GPLC programs referred to in GPLC_Programs.txt are compiled (if they are valid) at server start time.
loadGplcPrograms :: [[Char]] -> [Char] -> Array Int GPLC_program -> Int -> IO (Array Int GPLC_program)
loadGplcPrograms [] base_dir prog_array i = return prog_array
loadGplcPrograms (x:xs) base_dir prog_array i =
  let source_file = (base_dir ++ x ++ ".gplc")
  in do
  source <- bracket (openFile source_file ReadMode) hClose
                    (\h -> do c <- hGetContents h; putStr ("\nprogram file size: " ++ show (length c)); return c)
  putStr ("\nCompiling GPLC program " ++ x ++ " from source file " ++ source_file)
  loadGplcPrograms xs base_dir (prog_array // [(i, compileProgram x ((splitOn "\n~\n" source) !! 1))]) (i + 1)
  

applyCommand :: Server_state -> [[Char]] -> IO (Server_state, [Char])
applyCommand server_state command =
  let next_server_state = interpretCommand command baseNode server_state
  in return next_server_state

