-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- The .dan map file format encodes map geometry in a form intended to be space efficient and editable with minimal tools.  This module transforms (and expands)
-- that text format into an intermediate text format that is then parsed and used to initialise the Wall_grid and Floor_grid arrays.  This transformation was
-- originally done by a tool chain program as a pre - processing step and was later moved into the engine.
-- I have retained the intermediate text format to avoid having to re - write some of the game state initialisation code, which appeared needless.

module DecompressMap where

import Prelude hiding ((!!))
import IndexWrapper0
import Data.Maybe
import Data.List.Split
import Data.Sequence hiding (take, length)
import Data.Foldable
import Control.Exception
import BuildModel

-- Convenience functions used by some of the map file parsing functions below.
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

showBool :: Bool -> [Char]
showBool True = "1, "
showBool False = "0, "

showInts :: [Int] -> [Char]
showInts [] = []
showInts (x:xs) = (show x) ++ ", " ++ showInts xs

d_obj = Obj_place {ident_ = 0, u__ = 4, v__ = 4, w__ = -1, rotation = [0, 0, 0], rotate_ = False, phase = 0, texture__ = 4, num_elem = 3, obj_flag = 1}

-- See section 2 of the map file specification.
mapObject :: Char -> [Obj_place] -> Int -> Int -> Int -> Int -> Maybe Obj_place
mapObject '0' obj u v w c = Nothing
mapObject 'a' obj u v w c = Just (((obj, 588) !! 0) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
mapObject 'b' obj u v w c = Just (((obj, 589) !! 1) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
mapObject 'c' obj u v w c = Just (((obj, 590) !! 2) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
mapObject 'd' obj u v w c = Just (((obj, 591) !! 3) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
mapObject 'e' obj u v w c = Just (((obj, 592) !! 4) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
mapObject 'f' obj u v w c = Just (((obj, 593) !! 5) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
mapObject 'g' obj u v w c = Just (((obj, 594) !! 6) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
mapObject 'h' obj u v w c = Just (((obj, 595) !! 7) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
mapObject 'i' obj u v w c = Just (((obj, 596) !! 8) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
mapObject 'j' obj u v w c = Just (((obj, 597) !! 9) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})
mapObject '1' obj u v w c = Just (((obj, 598) !! 0) {u__ = fromIntegral u, v__ = fromIntegral v, w__ = fromIntegral w, obj_flag = c})

loadObject :: [[Char]] -> [Obj_place]
loadObject [] = []
loadObject (x0:x1:x2:x3:x4:x5:x6:xs) = Obj_place {ident_ = read x0, u__ = 0, v__ = 0, w__ = 0, rotation = procInts [x1, x2, x3], rotate_ = loadGrid1 x4,
                                                  phase = 0, texture__ = read x5, num_elem = read x6, obj_flag = 0} : loadObject xs

maybeObject :: Obj_place -> [Char]
maybeObject x =
  if ident_ x == 0 then "0, "
  else if obj_flag x > 7499 then throw Invalid_obj_flag
  else "1, " ++ (show (ident_ x)) ++ ", " ++ (show (u__  x)) ++ ", " ++ (show (v__ x)) ++ ", " ++ (show (w__ x)) ++ ", " ++ (showInts (rotation x))
       ++ (showBool (rotate_ x)) ++ (show (phase x)) ++ ", " ++ (show (texture__ x)) ++ ", " ++ (show (num_elem x)) ++ ", " ++ (show (obj_flag x)) ++ ", "

-- See section 1 of the map file specification.
wallSetup :: Char -> [Bool]
wallSetup 'a' = [False, False, False, False]
wallSetup 'b' = [False, False, False, True]
wallSetup 'c' = [False, False, True, False]
wallSetup 'd' = [False, False, True, True]
wallSetup 'e' = [False, True, False, False]
wallSetup 'f' = [False, True, False, True]
wallSetup 'g' = [False, True, True, False]
wallSetup 'h' = [False, True, True, True]
wallSetup 'i' = [True, False, False, False]
wallSetup 'j' = [True, False, False, True]
wallSetup 'k' = [True, False, True, False]
wallSetup 'l' = [True, False, True, True]
wallSetup 'm' = [True, True, False, False]
wallSetup 'n' = [True, True, False, True]
wallSetup 'o' = [True, True, True, False]
wallSetup 'p' = [True, True, True, True]

-- These three functions handle the transformation of the wall grid part of the map file into an intermediate text format.
gridSetup2 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int]
gridSetup2 c0 c1 u v t u_max v_max =
  if t == 1 && v > v_max && u == u_max then []
  else if t == 0 && v > v_max then gridSetup2 (c0 + 1) c1 u 0 1 u_max v_max
  else if t == 1 && v > v_max then gridSetup2 c0 (c1 + 1) (u + 1) 0 0 u_max v_max
  else if t == 0 then [c0, c0] ++ gridSetup2 (c0 + 1) c1 u (v + 2) 0 u_max v_max
  else [c1, c1] ++ gridSetup2 c0 (c1 + 1) u (v + 2) 1 u_max v_max

gridSetup1 :: [Char] -> [Int] -> [Obj_place] -> Int -> Int -> Int -> Int -> Int -> [Wall_grid]
gridSetup1 [] _ obj u v w v_max c = []
gridSetup1 (x0:x1:x2:x3:x4:x5:x6:xs) (y:ys) obj u v w v_max c =
  let ws = wallSetup x0
      w_grid = Wall_grid {u1 = (ws, 599) !! 0, u2 = (ws, 600) !! 1, v1 = (ws, 601) !! 2, v2 = (ws, 602) !! 3, u1_bound = fromIntegral u,
                          u2_bound = fromIntegral (u + 1), v1_bound = fromIntegral v, v2_bound = fromIntegral (v + 1), w_level = fromIntegral w,
                          wall_flag = [c, c + 1, c + 2, c + 3], texture = [read [x1], read [x2], read [x3], read [x4]],
                          obj = mapObject x5 obj ((div u 2) * 2) ((div v 2) * 2) w y}
  in
  if v == v_max then w_grid : gridSetup1 xs ys obj (u + 1) 0 w v_max (c + 4)
  else w_grid : gridSetup1 xs ys obj u (v + 1) w v_max (c + 4)

gridSetup0 :: [Wall_grid] -> Int -> Int -> Int -> Int -> Int -> Int -> Seq Char -> Seq Char
gridSetup0 (x:xs) u v w u_max v_max w_max acc0 =
  let w_grid = (showBool (u1 x)) ++ (showBool (u2 x)) ++ (showBool (v1 x)) ++ (showBool (v2 x)) ++ (show (u1_bound x)) ++ ", " ++ (show (u2_bound x)) ++ ", "
               ++ (show (v1_bound x)) ++ ", " ++ (show (v2_bound x)) ++ ", " ++ (show (w_level x)) ++ ", " ++ (showInts (wall_flag x)) ++ (showInts (texture x))
               ++ maybeObject (fromMaybe d_obj (obj x))
  in
  if ((wall_flag x), 603) !! 3 > 119999 then throw Invalid_wall_flag
  else if u == u_max && v == v_max && w == w_max then acc0 >< fromList (init_ w_grid ++ "~")
  else if u == u_max && v == v_max then gridSetup0 xs 0 0 (w + 1) u_max v_max w_max (acc0 >< fromList (init_ w_grid ++ "&"))
  else if v == v_max then gridSetup0 xs (u + 1) 0 w u_max v_max w_max (acc0 >< fromList (init_ w_grid ++ ":"))
  else gridSetup0 xs u (v + 1) w u_max v_max w_max (acc0 >< fromList w_grid)

-- These two functions handle the transformation of the floor grid part of the map file into an intermediate text format.
makeFloor1 :: Int -> Int -> Int -> Char -> [Char]
makeFloor1 w u v 'a' = (show w) ++ ", " ++ "0"
makeFloor1 w u v 'b' = (show w) ++ ", " ++ "1"
makeFloor1 w u v 'c' = (show w) ++ ", " ++ "2"
makeFloor1 w u v 'd' = (show w) ++ ", " ++ "3"
makeFloor1 w u v 'e' = (show w) ++ ", " ++ "4"
makeFloor1 w u v 'f' = (show w) ++ ", " ++ "5"
makeFloor1 w u v 'g' = (show (w + 1)) ++ ", " ++ "0"
makeFloor1 w u v match = error ("\nmake_floor1 error.  w: " ++ show w ++ " u: " ++ show u ++ " v: " ++ show v ++ " non - match: " ++ [match])

makeFloor0 :: [[Char]] -> Int -> Int -> Int -> Int -> Int -> Int -> [Char]
makeFloor0 (x0:x1:x2:x3:x4:xs) u v w u_max v_max w_max =
  let f_map = ", " ++ x1 ++ ", " ++ x2 ++ ", " ++ x3 ++ ", " ++ x4
  in
  if u == u_max && v == v_max && w == w_max then makeFloor1 w u v (head x0) ++ f_map
  else if u == u_max && v == v_max then makeFloor1 w u v (head x0) ++ f_map ++ "&" ++ makeFloor0 xs 0 0 (w + 1) u_max v_max w_max
  else if v == v_max then makeFloor1 w u v (head x0) ++ f_map ++ ":" ++ makeFloor0 xs (u + 1) 0 w u_max v_max w_max
  else makeFloor1 w u v (head x0) ++ f_map ++ ", " ++ makeFloor0 xs u (v + 1) w u_max v_max w_max

-- The entry point to this module, called from Main.setup_game
procMap :: [[Char]] -> Int -> Int -> Int -> ([Char], [Char])
procMap pre_map u_max v_max w_max =
  let next_c = div ((u_max + 1) * (v_max + 1)) 4
      flag_seq = concat [gridSetup2 c c 0 0 0 u_max v_max | c <- [0, next_c..(next_c * w_max)]]
      c_max = 4 * (u_max + 1) * (v_max + 1) - 1
      floor = makeFloor0 (splitOn " " (map filter0 (concat [(pre_map, 604) !! w | w <- [(w_max + 2)..(w_max + 2 + w_max)]]))) 0 0 0 ((div (u_max + 1) 2) - 1)
                         ((div (v_max + 1) 2) - 1) w_max
      w_grid = gridSetup0 (concat [gridSetup1 ((pre_map, 605) !! (w + 1)) (gridSetup2 (next_c * w) (next_c * w) 0 0 0 u_max v_max) (loadObject (splitOn ", " (filter1 ((pre_map, 606) !! 0)))) 0 0 w v_max ((u_max + 1) * (v_max + 1) * w * 4) | w <- [0..w_max]]) 0 0 0 u_max v_max w_max empty
  in (toList w_grid ++ floor, [])


