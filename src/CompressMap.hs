-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module CompressMap where

import Data.Array.IArray
import Data.Maybe
import BuildModel

encodeFloorModel :: Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Int -> Int -> Int -> Char
encodeFloorModel w_grid f_grid w u v
  | surface floor_voxel == Flat =
    if floor_texture == 1 then 'a'
    else if floor_texture == 2 then 'b'
    else if floor_texture == 3 then 'c'
    else if floor_texture == 4 then 'd'
    else '0'
  | surface floor_voxel == Positive_u = 'e'
  | surface floor_voxel == Negative_u = 'f'
  | surface floor_voxel == Positive_v = 'g'
  | surface floor_voxel == Negative_v = 'h'
  | surface floor_voxel == Open = '0'
  where u_ = div u 2
        v_ = div v 2
        wall_voxel = w_grid ! (w, u, v)
        floor_voxel = f_grid ! (w, u_, v_)
        floor_texture = texture__ (fromMaybe def_obj_place (obj wall_voxel))

encodeWallStructure :: Wall_grid -> Char
encodeWallStructure voxel
  | not u1_ && not u2_ && not v1_ && not v2_ = 'a'
  | not u1_ && not u2_ && not v1_ && v2_ = 'b'
  | not u1_ && not u2_ && v1_ && not v2_ = 'c'
  | not u1_ && not u2_ && v1_ && v2_ = 'd'
  | not u1_ && u2_ && not v1_ && not v2_ = 'e'
  | not u1_ && u2_ && not v1_ && v2_ = 'f'
  | not u1_ && u2_ && v1_ && not v2_ = 'g'
  | not u1_ && u2_ && v1_ && v2_ = 'h'
  | u1_ && not u2_ && not v1_ && not v2_ = 'i'
  | u1_ && not u2_ && not v1_ && v2_ = 'j'
  | u1_ && not u2_ && v1_ && not v2_ = 'k'
  | u1_ && not u2_ && v1_ && v2_ = 'l'
  | u1_ && u2_ && not v1_ && not v2_ = 'm'
  | u1_ && u2_ && not v1_ && v2_ = 'n'
  | u1_ && u2_ && v1_ && not v2_ = 'o'
  | u1_ && u2_ && v1_ && v2_ = 'p'
  where u1_ = u1 voxel
        u2_ = u2 voxel
        v1_ = v1 voxel
        v2_ = v2 voxel

encodeWallGrid :: Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Int -> Int -> Int -> Int -> Int -> [[Char]] -> [Char]
encodeWallGrid w_grid f_grid w u v u_max v_max acc
  | w > 2 = reverse (concat acc)
  | u > u_max = encodeWallGrid w_grid f_grid (w + 1) 0 0 u_max v_max ("\n~\n " : acc)
  | v > v_max && u < u_max = encodeWallGrid w_grid f_grid w (u + 1) 0 u_max v_max ("\n" : acc)
  | v > v_max = encodeWallGrid w_grid f_grid w (u + 1) 0 u_max v_max acc
  | v == v_max = encodeWallGrid w_grid f_grid w u (v + 1) u_max v_max ([floor_type, tex3, tex2, tex1, tex0, structure] : acc)
  | otherwise = encodeWallGrid w_grid f_grid w u (v + 1) u_max v_max ([' ', floor_type, tex3, tex2, tex1, tex0, structure] : acc)
  where voxel = w_grid ! (w, u, v)
        structure = encodeWallStructure voxel
        tex0 = head (show ((texture voxel) !! 0))
        tex1 = head (show ((texture voxel) !! 1))
        tex2 = head (show ((texture voxel) !! 2))
        tex3 = head (show ((texture voxel) !! 3))
        floor_type = encodeFloorModel w_grid f_grid w u v


