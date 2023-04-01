-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- When the server saves a map it is augmented with wall padding and navigation data used by non - player characters (NPCs).
-- The navigation data added is used by the engine to initialise the local_up_ramp and local_down_ramp fields of Floor_grid,
-- which indicate to the NPC code the ramp to approach when a target is on another level.  The process also verifies if the
-- map is escapable or not, which it is required not to be.  These two tasks are achieved by simulating a finite element field
-- flooding the map geometry described by Obj_grid by expanding in every direction it is able to, while constrained by the rule
-- that it can't enter a voxel with object type > 0.

module PreprocessMap where

import Data.Array.IArray
import Data.List.Split
import BuildModel
import DecompressMap

single_fill [] (x:xs) = (x:xs)
single_fill (x:xs) [] = (x:xs)
single_fill (x:xs) (y:ys) = (x:xs)
single_fill [] [] = []

head_ [] = (0, 0)
head_ ls = head ls

-- This function modifies the collision properties of Obj_grid such that walls are effectively padded with voxels that can't be entered.
-- This is so that the models for the player and NPCs don't intersect the walls.
padWalls :: Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) Obj_grid -> Int -> Int -> Int -> Int -> Int
               -> Array (Int, Int, Int) Obj_grid
padWalls w_grid f_grid obj_grid w u v u_limit v_limit =
  let wall_voxel = w_grid ! (w, u, v)
      floor_surface = surface (f_grid ! (w, div u 2, div v 2))
      prog_present = if program (obj_grid ! (w, u, v)) == [] then False
                     else True
      wall_adjacent = u1 wall_voxel || u2 wall_voxel || v1 wall_voxel || v2 wall_voxel
      obj_grid' = if (floor_surface == Open || floor_surface == Flat) && prog_present == False && wall_adjacent then
        obj_grid // [((w, u, v), Obj_grid {objType = 4, program = [], programName = []})]
                  else obj_grid
  in
  if w == 2 && u == u_limit && v == v_limit then obj_grid'
  else if u == u_limit && v == v_limit then padWalls w_grid f_grid obj_grid' (w + 1) 0 0 u_limit v_limit
  else if u == u_limit then padWalls w_grid f_grid obj_grid' w 0 (v + 1) u_limit v_limit
  else padWalls w_grid f_grid obj_grid' w (u + 1) v u_limit v_limit

-- This function constructs a list of the locations of all the ramps within the map.
buildRampSet :: Array (Int, Int, Int) Floor_grid -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int, Int)], [(Int, Int, Int)], [(Int, Int, Int)])
                 -> [(Int, Int, Int)]
buildRampSet f_grid w u v u_limit v_limit acc =
  let surface_ = surface (f_grid ! (w, u, v))
      is_ramp = if surface_ == Open || surface_ == Flat then False
                else True
      ramp_update = \level0 level1 level2 -> if is_ramp == False then (level0, level1, level2)
                                             else
                                               if w == 0 then ((2, u, v) : level0, (1, u, v) : level1, level2)
                                               else (level0, (2, u, v) : level1, (1, u, v) : level2)
  in
  if w == 1 && u == u_limit && v == v_limit then fst__ acc ++ [(3, 0, 0)] ++ snd__ acc ++ [(3, 0, 0)] ++ third_ acc
  else if u == u_limit && v == v_limit then buildRampSet f_grid (w + 1) 0 0 u_limit v_limit (ramp_update (fst__ acc) (snd__ acc) (third_ acc))
  else if v == v_limit then buildRampSet f_grid w (u + 1) 0 u_limit v_limit (ramp_update (fst__ acc) (snd__ acc) (third_ acc))
  else buildRampSet f_grid w u (v + 1) u_limit v_limit (ramp_update (fst__ acc) (snd__ acc) (third_ acc))

-- The version of obj_grid updated within simFlood0 isn't returned and is only used internally as part of the flood simulation.
objGridUpd :: [(Int, Int, Int)] -> [((Int, Int, Int), Obj_grid)]
objGridUpd [] = []
objGridUpd ((w, u, v):xs) = ((w, u, v), Obj_grid {objType = 4, program = [], programName = []}) : objGridUpd xs

-- These two functions check each voxel encountered in the current iteration to see if it is colocated with a ramp.
checkVoxel1 :: [(Int, Int, Int)] -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
checkVoxel1 [] (u, v) = ([], [])
checkVoxel1 ((t, a, b):xs) (u, v) =
  if (a, b) == (div u 2, div v 2) then
    if t == 2 then ([(a, b)], [])
    else ([], [(a, b)])
  else checkVoxel1 xs (u, v)

checkVoxel0 :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
checkVoxel0 [] ramp_set up_ramp down_ramp = (up_ramp, down_ramp)
checkVoxel0 ((w, u, v):xs) ramp_set up_ramp down_ramp =
  let check_voxel1_ = checkVoxel1 ramp_set (u, v)
  in checkVoxel0 xs ramp_set (single_fill up_ramp (fst check_voxel1_)) (single_fill down_ramp (snd check_voxel1_))

-- These two functions compute each iteration of the flood simulation.
simFlood1 :: Array (Int, Int, Int) Obj_grid -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Int -> Int -> [(Int, Int, Int)]
simFlood1 obj_grid [] acc u_limit v_limit = acc
simFlood1 obj_grid ((w, u, v):xs) acc u_limit v_limit =
  let pos_u = if objType (obj_grid ! (w, u + 1, v)) > 0 then []
              else [(w, u + 1, v)]
      pos_v = if objType (obj_grid ! (w, u, v + 1)) > 0 then []
              else [(w, u, v + 1)]
      neg_u = if objType (obj_grid ! (w, u - 1, v)) > 0 then []
              else [(w, u - 1, v)]
      neg_v = if objType (obj_grid ! (w, u, v - 1)) > 0 then []
              else [(w, u, v - 1)]
      pos_uv = if objType (obj_grid ! (w, u + 1, v + 1)) > 0 then []
               else [(w, u + 1, v + 1)]
      pos_v_neg_u = if objType (obj_grid ! (w, u - 1, v + 1)) > 0 then []
                    else [(w, u - 1, v + 1)]
      neg_uv = if objType (obj_grid ! (w, u - 1, v - 1)) > 0 then []
                    else [(w, u - 1, v - 1)]
      pos_u_neg_v = if objType (obj_grid ! (w, u + 1, v - 1)) > 0 then []
                    else [(w, u + 1, v - 1)]
      unique_add = \new set -> if new == [] then set
                               else new ++ filter (/= head new) set
  in
  if u == u_limit || v == v_limit then error ("\nEdge of map reached at (" ++ show w ++ ", " ++ show u ++ ", " ++ show v ++ ").")
  else simFlood1 obj_grid xs ((unique_add pos_u) $
                              (unique_add pos_v) $
                              (unique_add neg_u) $
                              (unique_add neg_v) $
                              (unique_add pos_uv) $
                              (unique_add pos_v_neg_u) $
                              (unique_add neg_uv) $
                              (unique_add pos_u_neg_v acc)) u_limit v_limit

simFlood0 :: Array (Int, Int, Int) Obj_grid -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int
             -> ((Int, Int), (Int, Int))
simFlood0 obj_grid current_set ramp_set up_ramp down_ramp u_limit v_limit =
  let sim_flood1_ = simFlood1 obj_grid current_set [] u_limit v_limit
      ramps_found = checkVoxel0 sim_flood1_ ramp_set [] []
  in
  if sim_flood1_ == [] then (head_ up_ramp, head_ down_ramp)
  else simFlood0 (obj_grid // objGridUpd sim_flood1_) sim_flood1_ ramp_set (single_fill up_ramp (fst ramps_found)) (single_fill down_ramp (snd ramps_found))
                 u_limit v_limit

-- The flood simulation is applied to each element of Obj_grid where object type is initially 0, which is managed by this function.
-- note: Obj_grid elements are to be interpreted as (object type, [GPLC program]).
findRamps :: Array (Int, Int, Int) Obj_grid -> Array (Int, Int, Int) ((Int, Int), (Int, Int)) -> [[(Int, Int, Int)]] -> Int -> Int -> Int -> Int -> Int
              -> Array (Int, Int, Int) ((Int, Int), (Int, Int))
findRamps obj_grid ramp_map ramp_set w u v u_limit v_limit =
  let ramp_map' = if objType (obj_grid ! (w, u, v)) == 0 then ramp_map // [((w, u, v), simFlood0 obj_grid [(w, u, v)] (ramp_set !! w) [] [] u_limit v_limit)]
                  else ramp_map
  in
  if w == 2 && u == u_limit && v == v_limit then ramp_map'
  else if u == u_limit && v == v_limit then findRamps obj_grid ramp_map' ramp_set (w + 1) 0 0 u_limit v_limit
  else if u == u_limit then findRamps obj_grid ramp_map' ramp_set w 0 (v + 1) u_limit v_limit
  else findRamps obj_grid ramp_map' ramp_set w (u + 1) v u_limit v_limit

-- These two functions sample the navigation data generated by the flood simulation into the Floor_grid array,
-- which represents a lower resolution voxel map than that represented by the Obj_grid array.
sampleVoxel :: [((Int, Int), (Int, Int))] -> ((Int, Int), (Int, Int))
sampleVoxel [] = ((0, 0), (0, 0))
sampleVoxel (x:xs) =
  if x /= ((0, 0), (0, 0)) then x
  else sampleVoxel xs

augmentFloorGrid :: Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) ((Int, Int), (Int, Int)) -> Int -> Int -> Int -> Int -> Int
                    -> Array (Int, Int, Int) Floor_grid
augmentFloorGrid f_grid ramp_map w u v f_u_limit f_v_limit =
  let w_u = 2 * u
      w_v = 2 * v
      sampled_voxel = sampleVoxel [ramp_map ! (w, w_u, w_v), ramp_map ! (w, w_u, w_v + 1), ramp_map ! (w, w_u + 1, w_v), ramp_map ! (w, w_u + 1, w_v + 1)]
      f_grid' = f_grid // [((w, u, v), (f_grid ! (w, u, v)) {local_up_ramp = fst (sampled_voxel), local_down_ramp = snd (sampled_voxel)})]
  in
  if w == 2 && u == f_u_limit && v == f_v_limit then f_grid'
  else if u == f_u_limit && v == f_v_limit then augmentFloorGrid f_grid' ramp_map (w + 1) 0 0 f_u_limit f_v_limit
  else if v == f_v_limit then augmentFloorGrid f_grid' ramp_map w (u + 1) 0 f_u_limit f_v_limit
  else augmentFloorGrid f_grid' ramp_map w u (v + 1) f_u_limit f_v_limit

-- This is the intended entry point function for the module.
applyAugmentations :: Game_state -> (Int, Int) -> (Int, Int) -> Game_state
applyAugmentations game_state (w_u_limit, w_v_limit) (f_u_limit, f_v_limit) =
  let obj_grid' = padWalls (w_grid_ game_state) (f_grid_ game_state) (obj_grid_ game_state) 0 0 0 w_u_limit w_v_limit
      ramp_set = splitOn [(3, 0, 0)] (buildRampSet (f_grid_ game_state) 0 0 0 f_u_limit f_v_limit ([], [], []))
      new_ramp_map = array ((0, 0, 0), (2, w_u_limit, w_v_limit)) [((w, u, v), ((0, 0), (0, 0))) | w <- [0..2], u <- [0..w_u_limit], v <- [0..w_v_limit]]
      ramp_map = findRamps obj_grid' new_ramp_map ramp_set 0 0 0 w_u_limit w_v_limit
      f_grid' = augmentFloorGrid (f_grid_ game_state) ramp_map 0 0 0 f_u_limit f_v_limit
  in game_state {f_grid_ = f_grid', obj_grid_ = obj_grid'}

