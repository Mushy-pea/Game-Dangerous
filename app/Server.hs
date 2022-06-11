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
import HandleInput

loadMap :: [Char] -> (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) (Int, [Int]))
loadMap map =
  let u_max = read ((splitOn "\n~\n" map) !! 12)
      v_max = read ((splitOn "\n~\n" map) !! 13)
      w_max = read ((splitOn "\n~\n" map) !! 14)
      proc_map' = procMap (splitOn "\n~\n" map) u_max v_max w_max
      pm'' = fst proc_map'
      pm''' = snd proc_map'
      mc = \i -> (splitOn "\n~\n" map) !! i
      env_map = ".~.~.~.~" ++ pm'' ++ "~" ++ pm''' ++ mc 10 ++ "~" ++ mc 11 ++ "~" ++ mc 12 ++ "~" ++ mc 13 ++ "~" ++ mc 14 ++ "~" ++ mc 15
      u_limit = read ((splitOn "~" map) !! 8)
      v_limit = read ((splitOn "~" map) !! 9)
      w_limit = read ((splitOn "~" map) !! 10)
      fd = \limit -> div (limit + 1) 2 - 1
      buildTable1_ = buildTable1 (splitOn ", " ((splitOn "~" map) !! 7)) (emptyWGrid u_limit v_limit w_limit) 7500
      buildTable0_ = buildTable0 (elems buildTable1_) u_limit v_limit w_limit
      w_grid = checkMapLayer (-3) 0 0 u_limit v_limit
               (makeArray0 (buildTable0_ ++ sortGrid0 (splitOn "&" ((splitOn "~" map) !! 4))) u_limit v_limit w_limit)
               w_grid_flag
      f_grid = checkMapLayer 0 0 0 (fd u_limit) (fd v_limit)
                             (makeArray1 (loadFloor0 (splitOn "&" ((splitOn "~" map) !! 5))) (fd u_limit) (fd v_limit) w_limit) f_grid_flag
      obj_grid = checkMapLayer 0 0 0 u_limit v_limit (emptyObjGrid u_limit v_limit w_limit // loadObjGrid (splitOn ", " ((splitOn "~" map) !! 6)))
                               obj_grid_flag
      in (w_grid, f_grid, obj_grid)

main = do
  args <- getArgs
  putStr "\nGame :: Dangerous map development server version 0.5.0.0"
  putStr "\nLoading map file..."
  mapFile <- bracket (openFile (args !! 0) ReadMode) hClose (\h -> do c <- hGetContents h; putStr ("\nmap file size: " ++ show (length c)); return c)
  handleInput Nothing mapFile

handleInput :: Maybe Game_state -> [Char] -> IO ()
handleInput gameState mapFile =
  let newGameState = loadMap mapFile
  in do
  if isNothing gameState then handleInput (Just Game_state {is_set = True,
                                                            w_grid_ = fst__ newGameState,
                                                            f_grid_ = snd__ newGameState,
                                                            obj_grid_ = third_ newGameState,
                                                            s0_ = ps0_init,
                                                            s1_ = ps1_init,
                                                            map_transit_string = ([], [])})
                                          mapFile
  else do
    putStr "\nReady for request: "
    request <- getLine
    if request == "exit" then putStr "Exit command received ... closing server."
    else do
      result <- applyCommand (fromJust gameState) (splitOn " " request)
      putStr ("\nresult: " ++ snd result)
      handleInput (Just (fst result)) mapFile

applyCommand :: Game_state -> [[Char]] -> IO (Game_state, [Char])
applyCommand gameState request =
  let nextGameState = interpretCommand request base_node gameState
  in return nextGameState

