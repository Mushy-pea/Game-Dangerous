-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This module is a launcher for the engine.  Users can change engine settings relative to those in a given configuration file 
-- through an interactive text prompt.  This is implemented by the launcher passing command line arguments to the engine that 
-- override the defaults in the configuration file.

module Main where

import System.IO
import System.Environment
import System.Process
import Control.Exception
import Data.Maybe
import Data.Array.IArray
import Data.List.Split
import BuildModel

cycleSaveIndex :: Int -> Int
cycleSaveIndex (-1) = 1
cycleSaveIndex 1 = 2
cycleSaveIndex 2 = 3
cycleSaveIndex 3 = 4
cycleSaveIndex 4 = 5
cycleSaveIndex 5 = 6
cycleSaveIndex 6 = 1

main = do
  args <- getArgs
  cfg_file <- bracket (openFile (args !! 0) ReadMode) (hClose)
                      (\h -> do c <- hGetContents h; putStr ("\ncfg file size: " ++ show (length c)); return c)
  startEngine (listArray (0, 97) (splitOneOf "=\n" (tailFile cfg_file))) (read (args !! 1)) (read (args !! 2))

startEngine :: Array Int [Char] -> Int -> Int -> IO ()
startEngine conf_reg map_index save_index =
  let cfg' = cfg conf_reg 0
      arg0 = "--map_file map" ++ show map_index ++ ".dan"
      arg1 = "--current_save " ++ show save_index
  in do
  if (splitOn " " (cfg' "version_and_platform_string")) !! 0 == "Windows" then do
    putStr ("\nStarting engine with command line: " ++ "Game-Dangerous-exe " ++ cfg' "config_file" ++ " " ++ arg0 ++ " " ++ arg1)
    (h_in, h_out, _, ph) <- createProcess (shell ("Game-Dangerous-exe " ++ cfg' "config_file" ++ " " ++ arg0 ++ " " ++ arg1)) {std_in = CreatePipe, std_out = CreatePipe}
    next_map <- getSignalTrace (fromJust h_out) ph
    if next_map == 0 then return ()
    else startEngine conf_reg next_map (cycleSaveIndex save_index)
  else if (splitOn " " (cfg' "version_and_platform_string")) !! 0 == "Linux" then do
    putStr ("\nStarting engine with command line: " ++ "./Game-Dangerous-exe " ++ cfg' "config_file" ++ " " ++ arg0 ++ " " ++ arg1)
    (h_in, h_out, _, ph) <- createProcess (shell ("./Game-Dangerous-exe " ++ cfg' "config_file" ++ " " ++ arg0 ++ " " ++ arg1)) {std_in = CreatePipe, std_out = CreatePipe}
    next_map <- getSignalTrace (fromJust h_out) ph
    if next_map == 0 then return ()
    else do
      putStr ("\nsave_index: " ++ show save_index)
      startEngine conf_reg next_map (cycleSaveIndex save_index)
  else
    putStr ("\nUnsupported platform specified in configuration file.")

getSignalTrace :: Handle -> ProcessHandle -> IO Int
getSignalTrace h_out ph = do
  line <- hGetLine h_out
  exit_code <- getProcessExitCode ph
  if take 19 line == "Map file required: " then return (read [line !! 19])
  else if isJust exit_code then return 0
  else do
    putStrLn line
    getSignalTrace h_out ph

