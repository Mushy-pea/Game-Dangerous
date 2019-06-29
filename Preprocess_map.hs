-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This is a map development tool that applies wall padding and augments maps with navigation data used by NPCs.

module Main where

import System.IO
import Data.Array.IArray
import Decompress_map

data Wall_grid = Wall_grid {u1 :: Bool, u2 :: Bool, v1 :: Bool, v2 :: Bool}

init_w_grid :: [[Char]] -> Array (Int, Int, Int) Wall_grid -> Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) Wall_grid
init_w_grid [] w_grid w u v u_limit v_limit = w_grid
init_w_grid (x:xs) w_grid w u v u_limit v_limit =
  let wall_setup_ = wall_setup (head x)
      w_grid' = w_grid // [((w, u, v), Wall_grid {u1 = wall_setup_ !! 0, u2 = wall_setup_ !! 1, v1 = wall_setup_ !! 2, v2 = wall_setup_ !! 3})]
  in
  if u == u_limit && v == v_limit then init_w_grid xs w_grid' (w + 1) 0 0 u_limit v_limit
  else if u == u_limit then init_w_grid xs w_grid' w 0 (v + 1) u_limit v_limit
  else init_w_grid xs w_grid' w (u + 1) v u_limit v_limit

pad_walls :: Wall_grid -> Int
pad_walls voxel =
  if u1 voxel == True || u2 voxel == True || v1 voxel == True || v2 voxel == True then 4
  else 0

init_obj_grid :: Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) Int
init_obj_grid w_grid obj_grid w u v u_limit v_limit =
  let obj_grid' = obj_grid // [((w, u, v), pad_walls (w_grid ! (w, u, v)))]
  in
  if w == 2 && u == u_limit & v == v_limit then obj_grid'
  else if u == u_limit & v == v_limit then init_obj_grid w_grid obj_grid' (w + 1) 0 0 u_limit v_limit
  else if u == u_limit then init_obj_grid w_grid obj_grid' w 0 (v + 1) u_limit v_limit
  else init_obj_grid w_grid obj_grid' w (u + 1) v u_limit v_limit

main = do
  