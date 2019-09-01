-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This is a map development tool that applies wall padding and augments maps with navigation data used by non - player characters (NPCs).
-- For an explanation of wall padding see the now disabled function Decompress_map.pad_walls.  The navigation data added is used by the engine to initialise
-- the local_up_ramp and local_down_ramp fields of Floor_grid, which indicate to the NPC code the ramp to approach when a target
-- is on another level.  The process also verifies if the map is escapable or not, which it is required not to be.
-- These tasks are achieved by simulating a finite element field flooding the map geometry described by Obj_grid by expanding in every direction it is able to,
-- while constrained by the rule that it can't enter a voxel with object type > 0.

module Main where

import System.IO
import System.IO.Unsafe
import System.Environment
import Data.Array.IArray
import Data.List.Split
import Decompress_map hiding (pad_walls)

-- This is a simplified analogue of the Wall_grid type defined in Build_model, as less information is needed in this context.
data Wall_grid = Wall_grid {u1 :: Bool, u2 :: Bool, v1 :: Bool, v2 :: Bool}

def_w_grid = Wall_grid {u1 = False, u2 = False, v1 = False, v2 = False}

single_fill [] (x:xs) = (x:xs)
single_fill (x:xs) [] = (x:xs)
single_fill (x:xs) (y:ys) = (x:xs)
single_fill [] [] = []

head_ [] = (0, 0)
head_ ls = head ls

fst__ (a, b, c) = a
snd__ (a, b, c) = b
third_ (a, b, c) = c

-- Initialise the simplified Wall_grid array from map file input.
init_w_grid :: [[Char]] -> Array (Int, Int, Int) Wall_grid -> Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) Wall_grid
init_w_grid [] w_grid w u v u_limit v_limit = w_grid
init_w_grid (x:xs) w_grid w u v u_limit v_limit =
  let wall_setup_ = wall_setup (head x)
      w_grid' = w_grid // [((w, u, v), Wall_grid {u1 = wall_setup_ !! 0, u2 = wall_setup_ !! 1, v1 = wall_setup_ !! 2, v2 = wall_setup_ !! 3})]
  in
  if w == 3 then w_grid
  else if u == u_limit && v == v_limit then init_w_grid xs w_grid' (w + 1) 0 0 u_limit v_limit
  else if v == v_limit then init_w_grid xs w_grid' w (u + 1) 0 u_limit v_limit
  else init_w_grid xs w_grid' w u (v + 1) u_limit v_limit

-- These two functions initialise a simplified analogue of Obj_grid by applying pad_walls to each element of the Wall_grid and Obj_grid arrays.
pad_walls :: Wall_grid -> Int
pad_walls voxel =
  if u1 voxel == True || u2 voxel == True || v1 voxel == True || v2 voxel == True then 4
  else 0

init_obj_grid :: Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Bool -> Array (Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) Int
init_obj_grid w_grid f_grid obj_grid w u v u_limit v_limit =
  let obj_grid' = if f_grid ! (w, div u 2, div v 2) == False then obj_grid // [((w, u, v), pad_walls (w_grid ! (w, u, v)))]
                  else obj_grid
  in
  if w == 2 && u == u_limit && v == v_limit then obj_grid'
  else if u == u_limit && v == v_limit then init_obj_grid w_grid f_grid obj_grid' (w + 1) 0 0 u_limit v_limit
  else if u == u_limit then init_obj_grid w_grid f_grid obj_grid' w 0 (v + 1) u_limit v_limit
  else init_obj_grid w_grid f_grid obj_grid' w (u + 1) v u_limit v_limit

-- These two functions construct a list of the positions of ramps within the map.
load_floor1 :: Char -> Bool
load_floor1 'b' = True
load_floor1 'd' = True
load_floor1 'c' = True
load_floor1 'e' = True
load_floor1 _ = False

load_floor0 :: [[Char]] -> ([(Int, Int, Int)], [(Int, Int, Int)], [(Int, Int, Int)]) -> Array (Int, Int, Int) Bool -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int, Int)], Array (Int, Int, Int) Bool)
load_floor0 (x0:x1:x2:x3:x4:xs) acc f_grid w u v u_limit v_limit =
  let is_ramp = load_floor1 (head x0)
      ramp_update = \level0 level1 level2 -> if is_ramp == False then (level0, level1, level2)
                                             else
                                               if w == 0 then ((2, u, v) : level0, (1, u, v) : level1, level2)
                                               else (level0, (2, u, v) : level1, (1, u, v) : level2)
      f_grid' = f_grid // [((w, u, v), is_ramp)]
  in
  if w == 1 && u == u_limit && v == v_limit then (fst__ acc ++ [(3, 0, 0)] ++ snd__ acc ++ [(3, 0, 0)] ++ third_ acc, f_grid)
  else if u == u_limit && v == v_limit then load_floor0 xs (ramp_update (fst__ acc) (snd__ acc) (third_ acc)) f_grid' (w + 1) 0 0 u_limit v_limit
  else if v == v_limit then load_floor0 xs (ramp_update (fst__ acc) (snd__ acc) (third_ acc)) f_grid' w (u + 1) 0 u_limit v_limit
  else load_floor0 xs (ramp_update (fst__ acc) (snd__ acc) (third_ acc)) f_grid' w u (v + 1) u_limit v_limit

obj_grid_upd :: [(Int, Int, Int)] -> [((Int, Int, Int), Int)]
obj_grid_upd [] = []
obj_grid_upd ((w, u, v):xs) = ((w, u, v), 4) : obj_grid_upd xs

-- These two functions check each voxel encountered in the current iteration to see if it is colocated with a ramp.
check_voxel1 :: [(Int, Int, Int)] -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
check_voxel1 [] (u, v) = ([], [])
check_voxel1 ((t, a, b):xs) (u, v) =
  if (a, b) == (div u 2, div v 2) then
    if t == 2 then ([(a, b)], [])
    else ([], [(a, b)])
  else check_voxel1 xs (u, v)

check_voxel0 :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
check_voxel0 [] ramp_set up_ramp down_ramp = (up_ramp, down_ramp)
check_voxel0 ((w, u, v):xs) ramp_set up_ramp down_ramp =
  let check_voxel1_ = check_voxel1 ramp_set (u, v)
  in check_voxel0 xs ramp_set (single_fill up_ramp (fst check_voxel1_)) (single_fill down_ramp (snd check_voxel1_))

-- These two functions compute each iteration of the flood simulation.
sim_flood1 :: Array (Int, Int, Int) Int -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Int -> Int -> [(Int, Int, Int)]
sim_flood1 obj_grid [] acc u_limit v_limit = acc
sim_flood1 obj_grid ((w, u, v):xs) acc u_limit v_limit =
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
      pos_u_neg_v = if obj_grid ! (w, u + 1, v - 1) > 0 then []
                    else [(w, u + 1, v - 1)]
      unique_add = \new set -> if new == [] then set
                                else new ++ filter (/= head new) set
  in
  if u == u_limit || v == v_limit then error ("\nEdge of map reached at (" ++ show w ++ ", " ++ show u ++ ", " ++ show v ++ ").")
  else sim_flood1 obj_grid xs ((unique_add pos_u) $ (unique_add pos_v) $ (unique_add neg_u) $ (unique_add neg_v) $ (unique_add pos_uv) $ (unique_add pos_v_neg_u) $ (unique_add neg_uv) $ (unique_add pos_u_neg_v acc)) u_limit v_limit

sim_flood0 :: Array (Int, Int, Int) Int -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> ((Int, Int), (Int, Int))
sim_flood0 obj_grid current_set ramp_set up_ramp down_ramp u_limit v_limit =
  let sim_flood1_ = sim_flood1 obj_grid current_set [] u_limit v_limit
      ramps_found = check_voxel0 sim_flood1_ ramp_set [] []
  in
  if sim_flood1_ == [] then (head_ up_ramp, head_ down_ramp)
  else sim_flood0 (obj_grid // obj_grid_upd sim_flood1_) sim_flood1_ ramp_set (single_fill up_ramp (fst ramps_found)) (single_fill down_ramp (snd ramps_found)) u_limit v_limit

-- The flood simulation is applied to each element of Obj_grid where object type is initially 0, which is managed by this function.
augment_map :: Array (Int, Int, Int) Int -> Array (Int, Int, Int) ((Int, Int), (Int, Int)) -> [[(Int, Int, Int)]] -> Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) ((Int, Int), (Int, Int))
augment_map obj_grid ramp_map ramp_set w u v u_limit v_limit =
  let ramp_map' = if obj_grid ! (w, u, v) == 0 then ramp_map // [((w, u, v), sim_flood0 obj_grid [(w, u, v)] (ramp_set !! w) [] [] u_limit v_limit)]
                  else ramp_map
  in
  if w == 2 && u == u_limit && v == v_limit then ramp_map'
  else if u == u_limit && v == v_limit then augment_map obj_grid ramp_map' ramp_set (w + 1) 0 0 u_limit v_limit
  else if u == u_limit then augment_map obj_grid ramp_map' ramp_set w 0 (v + 1) u_limit v_limit
  else augment_map obj_grid ramp_map' ramp_set w (u + 1) v u_limit v_limit

-- These three functions transform the augmented versions of Floor_grid and Obj_grid to text output, which the engine uses to initialise these arrays at map load time.
sample_voxel :: [((Int, Int), (Int, Int))] -> ((Int, Int), (Int, Int))
sample_voxel [] = ((0, 0), (0, 0))
sample_voxel (x:xs) =
  if x /= ((0, 0), (0, 0)) then x
  else sample_voxel xs

f_grid_to_text :: [[Char]] -> Array (Int, Int, Int) ((Int, Int), (Int, Int)) -> Int -> Int -> Int -> Int -> Int -> [Char]
f_grid_to_text (x0:x1:x2:x3:x4:xs) ramp_map w u v u_limit v_limit =
  let this_voxel = sample_voxel [ramp_map ! (w, u, v), ramp_map ! (w, u, v + 1), ramp_map ! (w, u + 1, v), ramp_map ! (w, u + 1, v + 1)]
      text_out = x0 ++ " " ++ show (fst (fst this_voxel)) ++ " " ++ show (snd (fst this_voxel)) ++ " " ++ show (fst (snd this_voxel)) ++ " " ++ show (snd (snd this_voxel))
  in
  if w == 2 && v > v_limit && u == u_limit - 1 then []
  else if v > v_limit && u == u_limit - 1 then "\n~\n" ++ f_grid_to_text (x0:x1:x2:x3:x4:xs) ramp_map (w + 1) 0 0 u_limit v_limit
  else if v > v_limit then "\n" ++ f_grid_to_text (x0:x1:x2:x3:x4:xs) ramp_map w (u + 2) 0 u_limit v_limit
  else if v == v_limit - 1 then text_out ++ f_grid_to_text xs ramp_map w u (v + 2) u_limit v_limit
  else text_out ++ " " ++ f_grid_to_text xs ramp_map w u (v + 2) u_limit v_limit
f_grid_to_text _ ramp_map w u v u_limit v_limit = []

obj_grid_to_text :: Array (Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Int -> [Char]
obj_grid_to_text obj_grid w u v u_limit v_limit =
  let padding = if obj_grid ! (w, u, v) == 4 then show w ++ ", " ++ show u ++ ", " ++ show v ++ ", 4, 0, "
                else []
  in
  if w == 2 && u == u_limit && v == v_limit then padding
  else if u == u_limit && v == v_limit then padding ++ obj_grid_to_text obj_grid (w + 1) 0 0 u_limit v_limit
  else if v == v_limit then padding ++ obj_grid_to_text obj_grid w (u + 1) 0 u_limit v_limit
  else padding ++ obj_grid_to_text obj_grid w u (v + 1) u_limit v_limit

main = do
  args <- getArgs
  h0 <- openFile (args !! 0) ReadMode  
  contents <- hGetContents h0
  run_augmentation contents (args !! 1)
  hClose h0

run_augmentation :: [Char] -> [Char] -> IO ()
run_augmentation input_map file_path =
  let w_grid_text = splitOneOf " \n" (concat (take 3 (splitOn "\n~\n" input_map)))
      f_grid_text = splitOneOf " \n" (concat (take 3 (drop 3 (splitOn "\n~\n" input_map))))
      w_grid = init_w_grid w_grid_text (array ((0, 0, 0), (2, u_limit, v_limit)) [((w, u, v), def_w_grid) | w <- [0..2], u <- [0..u_limit], v <- [0..v_limit]]) 0 0 0 u_limit v_limit
      f_grid_plus = load_floor0 f_grid_text ([], [], []) (array ((0, 0, 0), (2, (div (u_limit + 1) 2) - 1, (div (v_limit + 1) 2) - 1)) [((w, u, v), False) | w <- [0..2], u <- [0..(div (u_limit + 1) 2) - 1], v <- [0..(div (v_limit + 1) 2) - 1]]) 0 0 0 ((div (u_limit + 1) 2) - 1) ((div (v_limit + 1) 2) - 1)
      obj_grid = init_obj_grid w_grid (snd f_grid_plus) (array ((0, 0, 0), (2, u_limit, v_limit)) [((w, u, v), 0) | w <- [0..2], u <- [0..u_limit], v <- [0..v_limit]]) 0 0 0 u_limit v_limit
      ramp_set = splitOn [(3, 0, 0)] (fst f_grid_plus)
      u_limit = read ((splitOn "\n~\n" input_map) !! 6)
      v_limit = read ((splitOn "\n~\n" input_map) !! 7)
      new_ramp_map = array ((0, 0, 0), (2, u_limit, v_limit)) [((w, u, v), ((0, 0), (0, 0))) | w <- [0..2], u <- [0..u_limit], v <- [0..v_limit]]
  in do
  h1 <- openFile file_path WriteMode
  hPutStr h1 (f_grid_to_text f_grid_text (augment_map obj_grid new_ramp_map ramp_set 0 0 0 u_limit v_limit) 0 0 0 u_limit v_limit)
  hPutStr h1 ("\n~\n" ++ obj_grid_to_text obj_grid 0 0 0 u_limit v_limit)
  hClose h1
