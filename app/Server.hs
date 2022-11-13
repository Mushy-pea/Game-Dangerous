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
  | head command == "show" = do
      if command !! 1 == "Wall_grid" then do
        if w < fst__ (fst w_bd) || w > fst__ (snd w_bd) || u < snd__ (fst w_bd) || u > snd__ (snd w_bd) || v < third_ (fst w_bd) || v > third_ (snd w_bd) then do
          putStr "\nInvalid w_grid index."
          handleInput game_state comp_map_text []
        else putStr ("\n" ++ show ((w_grid_ (fromJust game_state)) ! (w, u, v)))
      else if command !! 1 == "Floor_grid" then do
        if w < fst__ (fst f_bd) || w > fst__ (snd f_bd) || u < snd__ (fst f_bd) || u > snd__ (snd f_bd) || v < third_ (fst f_bd) || v > third_ (snd f_bd) then do
          putStr "\nInvalid f_grid index."
          handleInput game_state comp_map_text []
        else putStr ("\n" ++ show ((f_grid_ (fromJust game_state)) ! (w, u, v)))
      else if command !! 1 == "Obj_grid" then do
        if w < fst__ (fst w_bd) || w > fst__ (snd w_bd) || u < snd__ (fst w_bd) || u > snd__ (snd w_bd) || v < third_ (fst w_bd) || v > third_ (snd w_bd) then do
          putStr "\nInvalid obj_grid index."
          handleInput game_state comp_map_text []
        else putStr ("\n" ++ show ((obj_grid_ (fromJust game_state)) ! (w, u, v)))
      else putStr "\nInvalid parameter passed to show command."
      handleInput game_state comp_map_text []
  | otherwise = do
      result <- applyCommand (fromJust game_state) command
      putStr ("\nresult: " ++ snd result)
      handleInput (Just (fst result)) comp_map_text []
  where new_game_state = loadMap comp_map_text
        w = read (command !! 2)
        u = read (command !! 3)
        v = read (command !! 4)
        w_bd = bounds (w_grid_ (fromJust game_state))
        f_bd = bounds (f_grid_ (fromJust game_state))

applyCommand :: Game_state -> [[Char]] -> IO (Game_state, [Char])
applyCommand game_state command =
  let next_game_state = interpretCommand command baseNode game_state
  in return next_game_state

