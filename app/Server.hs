-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- The Main module for the map development server, designed to be used with the map development client.

module Main where

import System.IO
import System.Environment
import System.Process
import Control.Exception
import Data.List.Split
import Data.Maybe
import Data.Array.IArray
import Control.Concurrent
import BuildModel hiding (Game_state, w_grid_, f_grid_, obj_grid_)
import DecompressMap
import CompressMap
import OpenMap
import HandleInput
import PreprocessMap

loadMap :: [Char] -> Int -> Int -> Int -> (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) Obj_grid)
loadMap comp_map_text u_max v_max w_max =
  let proc_map' = procMap (splitOn "\n~\n" comp_map_text) u_max v_max w_max
      pm'' = fst proc_map'
      pm''' = snd proc_map'
      mc = \i -> (splitOn "\n~\n" comp_map_text) !! i
      map_text = ".~.~.~.~" ++ pm'' ++ "~" ++ pm''' ++ mc 10 ++ "~" ++ mc 11 ++ "~" ++ mc 12 ++ "~" ++ mc 13 ++ "~" ++ mc 14 ++ "~" ++ mc 15
  in openMap 1 map_text u_max v_max w_max

main = do
  args <- getArgs
  cfg_file <- bracket (openFile (args !! 0) ReadMode) (hClose)
                      (\h -> do c <- hGetContents h; putStr ("\ncfg file size: " ++ show (length c)); return c)
  initServer (listArray (0, 89) (splitOneOf "=\n" (tailFile cfg_file))) args

initServer :: Array Int [Char] -> [[Char]] -> IO ()
initServer conf_reg args =
  let cfg' = cfg conf_reg 0
  in do
  putStr ("\n\nGame :: Dangerous map development server starting.  Version and platform: " ++ cfg' "version_and_platform_string")
  putStr "\nLoading map file..."
  comp_map_text <- bracket (openFile ((args !! 1) ++ (args !! 2)) ReadMode) hClose
                   (\h -> do c <- hGetContents h; putStr ("\nmap file size: " ++ show (length c)); return c)
  input_ref <- newEmptyMVar
  console_output <- newEmptyMVar
  network_output <- newEmptyMVar
  if (splitOn " " (cfg' "version_and_platform_string")) !! 0 == "Windows" then do
    putStr "\nPlease use the \"exit\" command when you want to shut down the server."
    (h_in, h_out, _, _) <- createProcess (shell "node .\\node_server\\server.js") {std_in = CreatePipe, std_out = CreatePipe}
    forkIO (consoleInterface input_ref console_output)
    forkIO (networkInterface input_ref network_output (fromJust h_in) (fromJust h_out))
    handleInput Nothing (tailFile comp_map_text) [(args !! 1), "GPLC_Programs\\"] [] (fromJust h_in) input_ref console_output network_output 0
  else if (splitOn " " (cfg' "version_and_platform_string")) !! 0 == "Linux" then do
    putStr "\nPlease use Ctrl + C when you want to shut down the server."
    (h_in, h_out, _, _) <- createProcess (shell "node ./node_server/server.js") {std_in = CreatePipe, std_out = CreatePipe}
    forkIO (consoleInterface input_ref console_output)
    forkIO (networkInterface input_ref network_output (fromJust h_in) (fromJust h_out))
    handleInput Nothing (tailFile comp_map_text) [(args !! 1), "GPLC_Programs/"] [] (fromJust h_in) input_ref console_output network_output 0
  else error ("Unsupported platform found in version_and_platform_string")

handleInput :: Maybe Server_state -> [Char] -> [[Char]] -> [[Char]] -> Handle -> MVar (Int, [Char]) -> MVar [Char] -> MVar [Char]
               -> Int -> IO ()
handleInput server_state comp_map_text asset_path command h_in input_ref console_output network_output output_mode
  | isNothing server_state = do
    prog_set <- bracket (openFile ((asset_path !! 0) ++ (asset_path !! 1) ++ "GPLC_Programs.txt") ReadMode) hClose
                        (\h -> do c <- hGetContents h; putStr ("\nprogram list file size: " ++ show (length c)); return c)
    gplc_programs <-
      loadGplcPrograms (splitOn "\n" (tf prog_set))
                       ((asset_path !! 0) ++ (asset_path !! 1))
                       (array (0, length (splitOn "\n" (tf prog_set)) - 1) [(i, empty_gplc_program) | i <- [0..length (splitOn "\n" (tf prog_set)) - 1]])
                                      0
    handleInput (Just Server_state {w_grid_ = fst__ new_game_state,
                                    f_grid_ = snd__ new_game_state,
                                    obj_grid_ = third_ new_game_state,
                                    gplcPrograms = gplc_programs})
                comp_map_text asset_path command h_in input_ref console_output network_output 0
  | command == [] = do
    input <- takeMVar input_ref
    putStr ("\ncommand received: " ++ show (splitOn " " (snd input)))
    handleInput server_state comp_map_text asset_path (splitOn " " (snd input)) h_in input_ref console_output network_output (fst input)
  | head command == "exit" = do
    putStr "\nExit command received ... closing server."
    hPutStrLn h_in "exit"
    hFlush h_in
  | head command == "save" = do
    template <- bracket (openFile ((asset_path !! 0) ++ command !! 1) ReadMode) (hClose)
                        (\h -> do c <- hGetContents h; putStr ("\ntemplate file size: " ++ show (length c)); return c)
    bracket (openFile ((asset_path !! 0) ++ command !! 2) WriteMode) (hClose)
            (\h -> hPutStr h (encoded_wall_grid ++ encoded_floor_grid ++ template ++ encoded_obj_grid ++ encoded_sub_wall_grid ++ footer))
    if output_mode == 0 then putMVar console_output "\nMap file saved."
    else putMVar network_output "Map file saved."
    handleInput server_state comp_map_text asset_path [] h_in input_ref console_output network_output 0
  | otherwise = do
    result <- applyCommand (fromJust server_state) command
    if output_mode == 0 then putMVar console_output (snd result)
    else putMVar network_output (snd result)
    handleInput (Just (fst result)) comp_map_text asset_path [] h_in input_ref console_output network_output 0
  where tf = tailFile
        u_max = read ((splitOn "\n~\n" comp_map_text) !! 12)
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

-- These two functions each run on their own GHC thread and thereby allow the server to respectively receive requests 
-- through its console and network interfaces, which can then be handled asynchronously.
consoleInterface :: MVar (Int, [Char]) -> MVar [Char] -> IO ()
consoleInterface input_ref output_ref = do
  putStr "\nrequest: "
  request <- getLine
  putMVar input_ref (0, request)
  response <- takeMVar output_ref
  putStr response
  consoleInterface input_ref output_ref

networkInterface :: MVar (Int, [Char]) -> MVar [Char] -> Handle -> Handle -> IO ()
networkInterface input_ref output_ref h_in h_out = do
  request <- hGetLine h_out
  putMVar input_ref (1, request)
  response <- takeMVar output_ref
  hPutStrLn h_in (filter (/= '\n') response)
  hFlush h_in
  networkInterface input_ref output_ref h_in h_out

unpackErrors :: [[Char]] -> [Char] -> [Char]
unpackErrors [] acc = acc
unpackErrors (x:xs) acc = unpackErrors xs (acc ++ "\n" ++ x)

-- All GPLC programs referred to in GPLC_Programs.txt are compiled (if they are valid) at server start time.
loadGplcPrograms :: [[Char]] -> [Char] -> Array Int GPLC_program -> Int -> IO (Array Int GPLC_program)
loadGplcPrograms [] base_dir prog_array i = return prog_array
loadGplcPrograms (x:xs) base_dir prog_array i =
  let source_file = (base_dir ++ x ++ ".gplc")
  in do
  source <- bracket (openFile source_file ReadMode) hClose
                    (\h -> do c <- hGetContents h; putStr ("\nprogram file size: " ++ show (length c)); return c)
  putStr ("\nCompiling GPLC program [" ++ x ++ "] from source file: " ++ source_file)
  compiled_program <- compileProgram x ((splitOn "\n~\n" source) !! 1)
  if errors compiled_program /= [] then
    putStr ("\ncompileProgram: Compilation of GPLC program [" ++ x ++ "] failed with the following errors.\n"
            ++ unpackErrors (errors compiled_program) [])
  else putStr ("\ncompileProgram: Success.  Bytecode was generated for GPLC program [" ++ x ++ "].")
  loadGplcPrograms xs base_dir (prog_array // [(i, compiled_program)]) (i + 1)

applyCommand :: Server_state -> [[Char]] -> IO (Server_state, [Char])
applyCommand server_state command =
  let next_server_state = interpretCommand command baseNode server_state
  in return next_server_state

