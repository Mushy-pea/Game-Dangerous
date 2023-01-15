-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module OpenMap where

import Prelude hiding ((!!))
import IndexWrapper0
import Data.Array.IArray
import Data.List.Split
import BuildModel

-- These three functions update Obj_grid by patching the value blocks of GPLC programs contained in
-- it, according to the set of patches specified in the map file.  See the GPLC Specification for an explanation
-- of the value block.  This is done to allow for GPLC program instancing, whereby a single GPLC program
-- at source code level can be placed in multiple locations in a map with its value block patched to a different
-- state in each case.
patchValueBlock :: Array (Int, Int, Int) (Int, [Int]) -> Int -> Int -> Int -> Int -> [(Int, Int)] -> Array (Int, Int, Int) (Int, [Int])
patchValueBlock obj_grid w u v obj_type patch_list =
  let target = obj_grid ! (w, u, v)
      d_list = snd target
      value_block = (listArray (0, (length d_list) - 1) d_list) :: Array Int Int
      value_block' = value_block // patch_list
      d_list' = elems value_block'
  in obj_grid // [((w, u, v), (obj_type, d_list'))]

buildPatchList :: [Int] -> [(Int, Int)]
buildPatchList [] = []
buildPatchList (x0:x1:xs) = (x0, x1) : buildPatchList xs

patchObjGrid :: [[[Char]]] -> Array (Int, Int, Int) (Int, [Int]) -> Array (Int, Int, Int) (Int, [Int])
patchObjGrid [] obj_grid = obj_grid
patchObjGrid ((w : u : v : obj_type : prog_num : patched : patch_length : ys) : xs) obj_grid
  | patched == "y" = patchObjGrid xs (patchValueBlock obj_grid (read w) (read u) (read v) (read obj_type) patch_list)
  | patched == "n" = patchObjGrid xs (patchValueBlock obj_grid (read w) (read u) (read v) (read obj_type) [])
  | otherwise = error ("\npatchObjGrid: Invalid obj_grid patch data at obj_grid(" ++ show (w, u, v) ++ ").")
  where patch_list = buildPatchList (map (read) ys)

-- This is used as a library function by the engine (in Main) and the development server (in Server).
openMap :: [Char] -> [[Char]] -> Int -> Int -> Int -> Int
           -> (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) (Int, [Int]))
openMap map_text obj_grid_patch u_limit v_limit w_limit mode =
  let fd = \limit -> (div (limit + 1) 2) - 1
      buildTable1_ = buildTable1 (splitOn ", " (((splitOn "~" map_text), 59) !! 7)) (emptyWGrid u_limit v_limit w_limit) 7500
      buildTable0_ = buildTable0 (elems buildTable1_) u_limit v_limit w_limit
      w_grid = checkMapLayer (-3) 0 0 u_limit v_limit
               (makeArray0 (buildTable0_ ++ (sortGrid0 (splitOn "&" (((splitOn "~" map_text), 60) !! 4)))) u_limit v_limit w_limit)
               w_grid_flag
      f_grid = checkMapLayer 0 0 0 (fd u_limit) (fd v_limit)
                             (makeArray1 (loadFloor0 (splitOn "&" (((splitOn "~" map_text), 61) !! 5))) (fd u_limit) (fd v_limit) w_limit) f_grid_flag
      obj_grid = checkMapLayer 0 0 0 u_limit v_limit (emptyObjGrid u_limit v_limit w_limit // loadObjGrid (splitOn ", " (((splitOn "~" map_text), 62) !! 6)))
                               obj_grid_flag
  in 
  if mode == 0 then (w_grid, f_grid, patchObjGrid (map (splitOn " ") obj_grid_patch) obj_grid)
  else (w_grid, f_grid, obj_grid)

