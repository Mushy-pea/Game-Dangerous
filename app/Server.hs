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
import BuildModel
import DecompressMap
import CompressMap
import OpenMap
import HandleInput

loadMap :: [Char] -> (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) (Int, [Int]))
loadMap comp_map_text =
  let u_max = read ((splitOn "\n~\n" comp_map_text) !! 12)
      v_max = read ((splitOn "\n~\n" comp_map_text) !! 13)
      w_max = read ((splitOn "\n~\n" comp_map_text) !! 14)
      proc_map' = procMap (splitOn "\n~\n" comp_map_text) u_max v_max w_max
      pm'' = fst proc_map'
      pm''' = snd proc_map'
      mc = \i -> (splitOn "\n~\n" comp_map_text) !! i
      map_text = ".~.~.~.~" ++ pm'' ++ "~" ++ pm''' ++ mc 10 ++ "~" ++ mc 11 ++ "~" ++ mc 12 ++ "~" ++ mc 13 ++ "~" ++ mc 14 ++ "~" ++ mc 15
  in openMap map_text u_max v_max w_max

main = do
  args <- getArgs
  putStr "\nGame :: Dangerous map development server version 0.5.0.0"
  putStr "\nLoading map file..."
  comp_map_text <- bracket (openFile (args !! 0) ReadMode) hClose (\h -> do c <- hGetContents h; putStr ("\nmap file size: " ++ show (length c)); return c)
  handleInput Nothing comp_map_text []

handleInput :: Maybe Game_state -> [Char] -> [[Char]] -> IO ()
handleInput game_state comp_map_text command
  | isNothing game_state = handleInput (Just Game_state {is_set = True,
                                                         w_grid_ = fst__ new_game_state,
                                                         f_grid_ = snd__ new_game_state,
                                                         obj_grid_ = third_ new_game_state,
                                                         s0_ = ps0_init,
                                                         s1_ = ps1_init,
                                                         map_transit_string = ([], [])})
                                       comp_map_text command
  | command == [] = do
      putStr "\nReady for request: "
      request <- getLine
      handleInput game_state comp_map_text (splitOn " " request)
  | head command == "exit" = putStr "\nExit command received ... closing server."
  | head command == "save" = do
    h <- openFile (command !! 1) WriteMode
    hPutStr h (encoded_wall_grid ++ "\n" ++ encoded_floor_grid)
    hClose h
  | otherwise = do
      result <- applyCommand (fromJust game_state) command
      putStr ("\nresult: " ++ snd result)
      handleInput (Just (fst result)) comp_map_text []
  where new_game_state = loadMap comp_map_text
        w_bd = bounds (w_grid_ (fromJust game_state))
        f_bd = bounds (f_grid_ (fromJust game_state))
        encoded_wall_grid = encodeWallGrid (w_grid_ (fromJust game_state)) 0 0 0 (snd__ (snd w_bd)) (third_ (snd w_bd)) []
        encoded_floor_grid = encodeFloorGrid (f_grid_ (fromJust game_state)) 0 0 0 (snd__ (snd f_bd)) (third_ (snd f_bd)) []

applyCommand :: Game_state -> [[Char]] -> IO (Game_state, [Char])
applyCommand game_state command =
  let next_game_state = interpretCommand command baseNode game_state
  in return next_game_state


