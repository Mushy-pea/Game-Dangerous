-- The map loading code in this module is used as a library by the engine (in Main) and the development server (in Server).

module OpenMap where

import Prelude hiding ((!!))
import IndexWrapper0
import Data.Array.IArray
import Data.List.Split
import BuildModel

openMap :: [Char] -> Int -> Int -> Int -> (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) Obj_grid)
openMap map_text u_limit v_limit w_limit =
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
  in (w_grid, f_grid, obj_grid)

