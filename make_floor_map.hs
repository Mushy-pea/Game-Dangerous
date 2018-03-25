module Main where

import System.IO
import System.Environment
import Build_model
import Decompress_map

def_floor_map :: Int -> Int -> Array (Int, Int, Int) (Int, Float, Float)
def_floor_map u_limit v_limit = array ((0, 0, 0), (2, u_limit, v_limit)) [((w, u, v), (0, 0, 0)) | w <- [0..2], u <- [0..u_limit], v <- [0..v_limit]]

gen_floor_map2 :: Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, Float, Float) -> [(Int, Int, Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Array (Int, Int, Int) (Int, Float, Float), [(Int, Int, Int)], (Int, Int))
gen_floor_map2 f_grid floor_map ramps w u v u_limit v_limit c (step0, step1) =
  let floor_map' = \t -> floor_map // [((w, div u 2, div v 2), (t, fromIntegral (u + 1), fromIntegral (v + 1)))]
      ramps' = ramps ++ [(w, div u 2, div v 2)]
  in
  if u > u_limit then
    if w == 2 then (floor_map, ramps, (step0, step1))
    else if w == 1 then gen_floor_map2 f_grid floor_map ramps (w + 1) 0 0 u_limit v_limit c (step0, c)
    else gen_floor_map2 f_grid floor_map ramps (w + 1) 0 0 u_limit v_limit c (c, step1)
  else if v > v_limit then gen_floor_map2 f_grid floor_map ramps w (u + 2) 0 u_limit v_limit c (step0, step1)
  else if surface (f_grid ! (w, div u 2, div v 2)) == Negative_u || surface (f_grid ! (w, div u 2, div v 2)) == Negative_v then gen_floor_map2 f_grid (floor_map' 1) ramps' w u (v + 2) u_limit v_limit (c + 1) (step0, step1)
  else if surface (f_grid ! (w, div u 2, div v 2)) == Positive_u || surface (f_grid ! (w, div u 2, div v 2)) == Positive_v then gen_floor_map2 f_grid (floor_map' 2) ramps' w u (v + 2) u_limit v_limit (c + 1) (step0, step1)
  else gen_floor_map2 f_grid (floor_map' 0) ramps w u (v + 2) u_limit v_limit c (step0, step1)

gen_floor_map1 :: [(Int, Int, Int)] -> Int -> Int -> Int -> Float -> Float -> (Int, Int) -> (Int, Int) -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, Float, Float) -> Array (Int, Int, Int) Floor_grid
gen_floor_map1 [] w u v best_up best_down i_best_up i_best_down f_grid floor_map = f_grid // [((w, u, v), (f_grid ! (w, u, v)) {local_down_ramp = i_best_down, local_up_ramp = i_best_up})]
gen_floor_map1 (x:xs) w u v best_up best_down i_best_up i_best_down f_grid floor_map =
  let origin = (snd__ (floor_map ! (w, u, v)), third_ (floor_map ! (w, u, v)))
      target = (snd__ (floor_map ! x), third_ (floor_map ! x))
      distance = sqrt ((((fst target) - (fst origin)) ** 2) + (((snd target) - (snd origin)) ** 2))
  in
  if fst__ (floor_map ! x) == 1 && distance < best_down then gen_floor_map1 xs w u v best_up distance i_best_up (snd__ x, third_ x) f_grid floor_map
  else if fst__ (floor_map ! x) == 2 && distance < best_up then gen_floor_map1 xs w u v distance best_down (snd__ x, third_ x) i_best_down f_grid floor_map
  else gen_floor_map1 xs w u v best_up best_down i_best_up i_best_down f_grid floor_map

gen_floor_map0 :: Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, Float, Float) -> [(Int, Int, Int)] -> (Int, Int) -> Array (Int, Int, Int) Floor_grid
gen_floor_map0 w u v u_limit v_limit f_grid floor_map ramps level_steps =
  if w > 2 then f_grid
  else if u > u_limit then gen_floor_map0 (w + 1) 0 0 u_limit v_limit f_grid floor_map ramps level_steps
  else if v > v_limit then gen_floor_map0 w (u + 1) 0 u_limit v_limit f_grid floor_map ramps level_steps
  else if w == 0 then gen_floor_map0 w u (v + 1) u_limit v_limit (gen_floor_map1 (take (fst level_steps) ramps) w u v 101 101 (0, 0) (0, 0) f_grid floor_map) floor_map ramps level_steps
  else if w == 1 then gen_floor_map0 w u (v + 1) u_limit v_limit (gen_floor_map1 (take ((snd level_steps) - (fst level_steps)) (drop (fst level_steps) ramps)) w u v 101 101 (0, 0) (0, 0) f_grid floor_map) floor_map ramps level_steps
  else gen_floor_map0 w u (v + 1) u_limit v_limit (gen_floor_map1 (drop (snd level_steps) ramps) w u v 101 101 (0, 0) (0, 0) f_grid floor_map) floor_map ramps level_steps

augment_f_grid :: [Char] -> Int -> Int -> Array (Int, Int, Int) Floor_grid
augment_f_grid comp_env_map u_limit v_limit =
  let proc_map_ = proc_map (splitOn "\n~\n" comp_env_map) (read ((splitOn "\n~\n" comp_env_map) !! 12)) (read ((splitOn "\n~\n" comp_env_map) !! 13)) (read ((splitOn "\n~\n" comp_env_map) !! 14))
      c = ".~.~.~.~" ++ fst (proc_map_) ++ "~" ++ snd (proc_map_) ++ ((splitOn "\n~\n" comp_env_map) !! 10) ++ "~" ++ ((splitOn "\n~\n" comp_env_map) !! 11) ++ "~" ++ ((splitOn "\n~\n" comp_env_map) !! 12) ++ "~" ++ ((splitOn "\n~\n" comp_env_map) !! 13) ++ "~" ++ ((splitOn "\n~\n" comp_env_map) !! 14)
      f_grid = (make_array1 (load_floor0 (splitOn "&" ((splitOn "~" c) !! 5))) (div (read ((splitOn "~" c) !! 8)) 2) (div (read ((splitOn "~" c) !! 9)) 2) (read ((splitOn "~" c) !! 10)))
      floor_map = gen_floor_map2 f_grid (def_floor_map (read ((splitOn "~" c) !! 8)) (read ((splitOn "~" c) !! 9))) [] 0 0 0 (read ((splitOn "~" c) !! 8)) (read ((splitOn "~" c) !! 9)) 0 (0, 0)
  in gen_floor_map0 0 0 0 (read ((splitOn "~" c) !! 8)) (read ((splitOn "~" c) !! 9)) f_grid (fst__ floor_map) (snd__ floor_map) (third_ floor_map)

show_f_grid :: Array (Int, Int, Int) Floor_grid -> Int -> Int -> Int -> Int -> Int -> [Char]
show_f_grid f_grid w u v u_limit v_limit =
  if w > 2 then []
  else if u > u_limit then show_f_grid f_grid (w + 1) 

main = do
  
