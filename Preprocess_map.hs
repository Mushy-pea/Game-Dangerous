-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This is a map development tool that applies wall padding and augments maps with navigation data used by non - player characters (NPCs).  Wall padding is currently done
-- by Decompress_map.pad_walls but this will be removed as it is added to this tool.  The navigation data added is used by the engine to initialise
-- the local_up_ramp and local_down_ramp fields of Floor_grid, which indicate to the NPC code the ramp to approach when a target
-- is on another level.

module Main where

import System.IO
import Data.Array.IArray
import Decompress_map

-- This is a simplified analogue of the Wall_grid type defined in Build_model, as less information is needed in this context.
data Wall_grid = Wall_grid {u1 :: Bool, u2 :: Bool, v1 :: Bool, v2 :: Bool}

-- Initialise the simplified Wall_grid array from map file input.
init_w_grid :: [[Char]] -> Array (Int, Int, Int) Wall_grid -> Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) Wall_grid
init_w_grid [] w_grid w u v u_limit v_limit = w_grid
init_w_grid (x:xs) w_grid w u v u_limit v_limit =
  let wall_setup_ = wall_setup (head x)
      w_grid' = w_grid // [((w, u, v), Wall_grid {u1 = wall_setup_ !! 0, u2 = wall_setup_ !! 1, v1 = wall_setup_ !! 2, v2 = wall_setup_ !! 3})]
  in
  if u == u_limit && v == v_limit then init_w_grid xs w_grid' (w + 1) 0 0 u_limit v_limit
  else if u == u_limit then init_w_grid xs w_grid' w 0 (v + 1) u_limit v_limit
  else init_w_grid xs w_grid' w (u + 1) v u_limit v_limit

-- These two functions initialise a simplified analogue of Obj_grid by applying pad_walls to each element of the Wall_grid and Obj_grid arrays.
pad_walls :: Wall_grid -> Int
pad_walls voxel =
  if u1 voxel == True || u2 voxel == True || v1 voxel == True || v2 voxel == True then 4
  else 0

init_obj_grid :: Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) Int
init_obj_grid w_grid obj_grid w u v u_limit v_limit =
  let obj_grid' = obj_grid // [((w, u, v), pad_walls (w_grid ! (w, u, v)))]
  in
  if w == 2 && u == u_limit && v == v_limit then obj_grid'
  else if u == u_limit && v == v_limit then init_obj_grid w_grid obj_grid' (w + 1) 0 0 u_limit v_limit
  else if u == u_limit then init_obj_grid w_grid obj_grid' w 0 (v + 1) u_limit v_limit
  else init_obj_grid w_grid obj_grid' w (u + 1) v u_limit v_limit

-- These two functions construct a list of the positions of ramps within the map.
load_floor1 :: Char -> Int
load_floor1 'b' = 2
load_floor1 'd' = 2
load_floor1 'c' = 1
load_floor1 'e' = 1
load_floor1 _ = 0

load_floor0 :: [[Char]] -> Int -> Int -> Int -> Int -> Int -> [(Int, Int, Int)]
load_floor0 [] w u v u_limit v_limit = []
load_floor0 (x0:x1:x2:x3:x4:xs) w u v u_limit v_limit =
  let floor_type = load_floor1 (head x0)
      ramp_position = if floor_type == 0 then []
                      else [(floor_type, u, v)]
  in
  if u == u_limit && v == v_limit then ramp_position ++ load_floor0 xs (w + 1) 0 0 u_limit v_limit
  else if u == u_limit then ramp_position ++ load_floor0 xs w 0 (v + 1) u_limit v_limit
  else ramp_position ++ load_floor0 xs w (u + 1) v u_limit v_limit

sim_flood2 :: [(Int, Int, Int)] -> (Int, Int) -> Int
sim_flood2 [] (u, v) = 0
sim_flood2 ((t, a, b):xs) (u, v) =
  if (a, b) == (u, v) then t
  else sim_flood2 xs (u, v)

sim_flood1 :: Array (Int, Int, Int) Int -> (Int, Int, Int) -> Int -> Int -> [(Int, Int, Int)]
sim_flood1 obj_grid (w, u, v) u_limit v_limit =
  let pos_u = if obj_grid ! (w, u + 1, v) > 0 then []
              else [(w, u + 1, v)]
      pos_v = if obj_grid ! (w, u, v + 1) > 0 then []
              else [(w, u, v + 1)]
      neg_u = if obj_grid ! (w, u - 1, v) > 0 then []
              else [(w, u - 1, v)]
      neg_v = if obj_grid ! (w, u, v - 1) > 0 then []
              else [(w, u, v - 1)]
      pos_uv = if obj_grid ! (w, u + 1, v + 1) > 0 then []
               else [(w, u + 1, v + 1)]
      pos_v_neg_u = if obj_grid ! (w, u - 1, v + 1) > 0 then []
                    else [(w, u - 1, v + 1)]
      neg_uv = if obj_grid ! (w, u - 1, v - 1) > 0 then []
                    else [(w, u - 1, v - 1)]
      pos_u_neg_v = if obj_grid ! (w, u + 1, v - 1) then []
                    else [(w, u + 1, v - 1)]
  in
  if u == u_limit || v == v_limit then error ("\nEdge of map reached at (" ++ show w ++ ", " ++ show u ++ ", " ++ show v ++ ").")
  else pos_u ++ pos_v ++ neg_u ++ neg_v ++ pos_uv ++ pos_v_neg_u ++ neg_uv ++ pos_u_neg_v

sim_flood0 :: Array (Int, Int, Int) Int -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> ((Int, Int), (Int, Int))
sim_flood0 obj_grid [] _ up_ramp down_ramp = (head up_ramp, head down_ramp)
sim_flood0 obj_grid (x:xs) (y:ys) up_ramp down_ramp =
  if 

main = do
  