module Decompress_map where

import Data.Maybe
import Data.List.Split
import Control.Exception
import Build_model

-- These functions transform the characters in each geometry block in the map file into a form the engine can interpret
filter0 :: Char -> Char
filter0 x =
  if x == '\n' then ' '
  else x

filter1 :: [Char] -> [Char]
filter1 [] = []
filter1 (x:xs) =
  if x == '\n' then ", " ++ filter1 xs
  else x : filter1 xs

init_ :: [a] -> [a]
init_ x = take ((length x) - 2) x

d_obj = Obj_place {ident_ = 0, u__ = 4, v__ = 4, w__ = -1, rotation = [0, 0, 0], rotate_ = False, phase = 0, texture__ = 4, num_elem = 3, obj_flag = 1}

map_object :: Char -> [Obj_place] -> Int -> Int -> Int -> Int -> Maybe Obj_place
map_object '0' obj u v w c = Nothing
map_object 'a' obj u v w c = Just ((obj !! 0) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
map_object 'b' obj u v w c = Just ((obj !! 1) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
map_object 'c' obj u v w c = Just ((obj !! 2) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
map_object 'd' obj u v w c = Just ((obj !! 3) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
map_object 'e' obj u v w c = Just ((obj !! 4) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
map_object 'f' obj u v w c = Just ((obj !! 5) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
map_object 'g' obj u v w c = Just ((obj !! 6) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
map_object 'h' obj u v w c = Just ((obj !! 7) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
map_object 'i' obj u v w c = Just ((obj !! 8) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
map_object 'j' obj u v w c = Just ((obj !! 9) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
map_object '1' obj u v w c = Just ((obj !! 0) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})

load_object :: [[Char]] -> [Obj_place]
load_object [] = []
load_object (x0:x1:x2:x3:x4:x5:x6:xs) = Obj_place {ident_ = read x0, u__ = 0, v__ = 0, w__ = 0, rotation = proc_ints [x1, x2, x3], rotate_ = load_grid1 x4, phase = 0, texture__ = read x5, num_elem = read x6, obj_flag = 0} : load_object xs

wall_setup :: Char -> [Bool]
wall_setup 'a' = [False, False, False, False]
wall_setup 'b' = [False, False, False, True]
wall_setup 'c' = [False, False, True, False]
wall_setup 'd' = [False, False, True, True]
wall_setup 'e' = [False, True, False, False]
wall_setup 'f' = [False, True, False, True]
wall_setup 'g' = [False, True, True, False]
wall_setup 'h' = [False, True, True, True]
wall_setup 'i' = [True, False, False, False]
wall_setup 'j' = [True, False, False, True]
wall_setup 'k' = [True, False, True, False]
wall_setup 'l' = [True, False, True, True]
wall_setup 'm' = [True, True, False, False]
wall_setup 'n' = [True, True, False, True]
wall_setup 'o' = [True, True, True, False]
wall_setup 'p' = [True, True, True, True]

show_bool :: Bool -> [Char]
show_bool True = "1, "
show_bool False = "0, "

show_ints :: [Int] -> [Char]
show_ints [] = []
show_ints (x:xs) = (show x) ++ ", " ++ show_ints xs

maybe_object :: Obj_place -> [Char]
maybe_object x =
  if ident_ x == 0 then "0, "
  else if obj_flag x > 7499 then throw Invalid_obj_flag
  else "1, " ++ (show (ident_ x)) ++ ", " ++ (show (u__  x)) ++ ", " ++ (show (v__ x)) ++ ", " ++ (show (w__ x)) ++ ", " ++ (show_ints (rotation x)) ++ (show_bool (rotate_ x)) ++ (show (phase x)) ++ ", " ++ (show (texture__ x)) ++ ", " ++ (show (num_elem x)) ++ ", " ++ (show (obj_flag x)) ++ ", "

-- These functions parse the map file and generate engine interpretable data using the transformation functions above

pad_walls :: Wall_grid -> Int -> Int -> Int -> [Char]
pad_walls x u v w =
  if u1 x == True || u2 x == True || v1 x == True || v2 x == True then show w ++ ", " ++ show u ++ ", " ++ show v ++ ", " ++ "4, 0, "
  else []

-- The data needed to generate the Wall_grid array is produced here
grid_setup2 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int]
grid_setup2 c0 c1 u v t u_max v_max =
  if t == 1 && v > v_max && u == u_max then []
  else if t == 0 && v > v_max then grid_setup2 (c0 + 1) c1 u 0 1 u_max v_max
  else if t == 1 && v > v_max then grid_setup2 c0 (c1 + 1) (u + 1) 0 0 u_max v_max
  else if t == 0 then [c0, c0] ++ grid_setup2 (c0 + 1) c1 u (v + 2) 0 u_max v_max
  else [c1, c1] ++ grid_setup2 c0 (c1 + 1) u (v + 2) 1 u_max v_max

grid_setup1 :: [Char] -> [Int] -> [Obj_place] -> Int -> Int -> Int -> Int -> Int -> [Wall_grid]
grid_setup1 [] _ obj u v w v_max c = []
grid_setup1 (x0:x1:x2:x3:x4:x5:x6:xs) (y:ys) obj u v w v_max c =
  let ws = wall_setup x0
      w_grid = Wall_grid {u1 = ws !! 0, u2 = ws !! 1, v1 = ws !! 2, v2 = ws !! 3, u1_bound = fromIntegral u, u2_bound = fromIntegral (u + 1), v1_bound = fromIntegral v, v2_bound = fromIntegral (v + 1), w_level = fromIntegral w, wall_flag = [c, c + 1, c + 2, c + 3], texture = [read [x1], read [x2], read [x3], read [x4]], obj = map_object x5 obj ((div u 2) * 2) ((div v 2) * 2) w y}
  in
  if v == v_max then w_grid : grid_setup1 xs ys obj (u + 1) 0 w v_max (c + 4)
  else w_grid : grid_setup1 xs ys obj u (v + 1) w v_max (c + 4)

grid_setup0 :: [Wall_grid] -> Int -> Int -> Int -> Int -> Int -> Int -> [Char] -> [Char] -> ([Char], [Char])
grid_setup0 (x:xs) u v w u_max v_max w_max acc0 acc1 =
  let w_grid = (show_bool (u1 x)) ++ (show_bool (u2 x)) ++ (show_bool (v1 x)) ++ (show_bool (v2 x)) ++ (show (u1_bound x)) ++ ", " ++ (show (u2_bound x)) ++ ", " ++ (show (v1_bound x)) ++ ", " ++ (show (v2_bound x)) ++ ", " ++ (show (w_level x)) ++ ", " ++ (show_ints (wall_flag x)) ++ (show_ints (texture x)) ++ maybe_object (fromMaybe d_obj (obj x))
  in
  if (wall_flag x) !! 3 > 119999 then throw Invalid_wall_flag
  else if u == u_max && v == v_max && w == w_max then (acc0 ++ init_ w_grid ++ "~", acc1 ++ pad_walls x u v w)
  else if u == u_max && v == v_max then grid_setup0 xs 0 0 (w + 1) u_max v_max w_max (acc0 ++ init_ w_grid ++ "&") (acc1 ++ pad_walls x u v w)
  else if v == v_max then grid_setup0 xs (u + 1) 0 w u_max v_max w_max (acc0 ++ init_ w_grid ++ ":") (acc1 ++ pad_walls x u v w)
  else grid_setup0 xs u (v + 1) w u_max v_max w_max (acc0 ++ w_grid) (acc1 ++ pad_walls x u v w)

-- These two functions produce the data needed to generate the Floor_grid array
make_floor1 :: Int -> Char -> [Char]
make_floor1 w 'a' = (show w) ++ ", " ++ "0"
make_floor1 w 'b' = (show w) ++ ", " ++ "1"
make_floor1 w 'c' = (show w) ++ ", " ++ "2"
make_floor1 w 'd' = (show w) ++ ", " ++ "3"
make_floor1 w 'e' = (show w) ++ ", " ++ "4"
make_floor1 w 'f' = (show w) ++ ", " ++ "5"
make_floor1 w 'g' = (show (w + 1)) ++ ", " ++ "0"

make_floor0 :: [Char] -> Int -> Int -> Int -> Int -> Int -> Int -> [Char]
make_floor0 (x:xs) u v w u_max v_max w_max =
  if u == u_max && v == v_max && w == w_max then make_floor1 w x
  else if u == u_max && v == v_max then make_floor1 w x ++ "&" ++ make_floor0 xs 0 0 (w + 1) u_max v_max w_max
  else if v == v_max then make_floor1 w x ++ ":" ++ make_floor0 xs (u + 1) 0 w u_max v_max w_max
  else make_floor1 w x ++ ", " ++ make_floor0 xs u (v + 1) w u_max v_max w_max

proc_map :: [[Char]] -> Int -> Int -> Int -> ([Char], [Char])
proc_map pre_map u_max v_max w_max =
  let next_c = div ((u_max + 1) * (v_max + 1)) 4
      flag_seq = concat [grid_setup2 c c 0 0 0 u_max v_max | c <- [0, next_c..(next_c * w_max)]]
      c_max = 4 * (u_max + 1) * (v_max + 1) - 1
      floor = make_floor0 (concat (splitOn " " (map filter0 (concat [pre_map !! w | w <- [(w_max + 2)..(w_max + 2 + w_max)]])))) 0 0 0 ((div (u_max + 1) 2) - 1) ((div (v_max + 1) 2) - 1) w_max
      w_grid = grid_setup0 (concat [grid_setup1 (pre_map !! (w + 1)) (grid_setup2 (next_c * w) (next_c * w) 0 0 0 u_max v_max) (load_object (splitOn ", " (filter1 (pre_map !! 0)))) 0 0 w v_max ((u_max + 1) * (v_max + 1) * w * 4) | w <- [0..w_max]]) 0 0 0 u_max v_max w_max [] []
  in (fst w_grid ++ floor, snd w_grid)
  

