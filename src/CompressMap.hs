-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- The functions in this module are used by the server to encode a map state into the engine's map file format,
-- so it can be saved to disk.

module CompressMap where

import Data.Array.IArray
import Data.Maybe
import Data.List
import BuildModel hiding (Game_state, w_grid_, f_grid_, obj_grid_)
import HandleInput

-- The header for the Wall_grid section in the map file does not depend on any degree of freedom that can be edited through the server,
-- so it has been hard coded.
wGridHeader = concat ["16, 0, 0, 0, 0, 1, 36\n",
                      "16, 0, 0, 0, 0, 2, 36\n",
                      "16, 0, 0, 0, 0, 3, 36\n",
                      "16, 0, 0, 0, 0, 4, 36\n",
                      "32, 0, 0, 0, 0, 1, 6\n",
                      "48, 0, 0, 0, 0, 1, 6\n",
                      "64, 0, 0, 0, 0, 1, 6\n",
                      "80, 0, 0, 0, 0, 1, 6\n~\n"]

-- These three functions encode the Wall_grid array (for w >= 0) into the engine's map file format.
encodeFloorModel :: Wall_grid -> Char
encodeFloorModel voxel
  | floor_model == 16 =
    if floor_texture == 1 then 'a'
    else if floor_texture == 2 then 'b'
    else if floor_texture == 3 then 'c'
    else 'd'
  | floor_model == 32 = 'e'
  | floor_model == 48 = 'f'
  | floor_model == 64 = 'g'
  | floor_model == 80 = 'h'
  | otherwise = '0'
  where floor_model = ident_ (fromMaybe def_obj_place (obj voxel))
        floor_texture = texture__ (fromMaybe def_obj_place (obj voxel))

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

encodeWallGrid :: Array (Int, Int, Int) Wall_grid -> Int -> Int -> Int -> Int -> Int -> [[Char]] -> [Char]
encodeWallGrid w_grid w u v u_max v_max acc
  | w > 2 = wGridHeader ++ reverse (concat acc)
  | u > u_max = encodeWallGrid w_grid (w + 1) 0 0 u_max v_max ("\n~\n " : acc)
  | v > v_max && u < u_max = encodeWallGrid w_grid w (u + 1) 0 u_max v_max ("\n" : acc)
  | v > v_max = encodeWallGrid w_grid w (u + 1) 0 u_max v_max acc
  | v == v_max = encodeWallGrid w_grid w u (v + 1) u_max v_max ([floor_type, tex3, tex2, tex1, tex0, structure] : acc)
  | otherwise = encodeWallGrid w_grid w u (v + 1) u_max v_max ([' ', floor_type, tex3, tex2, tex1, tex0, structure] : acc)
  where voxel = w_grid ! (w, u, v)
        structure = encodeWallStructure voxel
        tex0 = head (show ((texture voxel) !! 0))
        tex1 = head (show ((texture voxel) !! 1))
        tex2 = head (show ((texture voxel) !! 2))
        tex3 = head (show ((texture voxel) !! 3))
        floor_type = encodeFloorModel voxel

-- These two functions encode the Floor_grid array into the engine's map file format.
encodeFloorTerrain :: Array (Int, Int, Int) Floor_grid -> Int -> Int -> Int -> Char
encodeFloorTerrain f_grid w u v
  | surface voxel == Flat =
    if truncate (w_ voxel) == w then 'a'
    else if truncate (w_ voxel) == w + 1 then 'g'
    else error ("\nencodeFloorTerrain: Invalid voxel found in Floor_grid at " ++ show (w, u, v))
  | surface voxel == Positive_u = 'b'
  | surface voxel == Negative_u = 'c'
  | surface voxel == Positive_v = 'd'
  | surface voxel == Negative_v = 'e'
  | surface voxel == Open = 'f'
  where voxel = f_grid ! (w, u, v)

encodeFloorGrid :: Array (Int, Int, Int) Floor_grid -> Int -> Int -> Int -> Int -> Int -> [[Char]] -> [Char]
encodeFloorGrid f_grid w u v u_max v_max acc
  | w > 2 = reverse (concat acc)
  | u > u_max = encodeFloorGrid f_grid (w + 1) 0 0 u_max v_max ("\n~\n " : acc)
  | v > v_max && u < u_max = encodeFloorGrid f_grid w (u + 1) 0 u_max v_max ("\n" : acc)
  | v > v_max = encodeFloorGrid f_grid w (u + 1) 0 u_max v_max acc
  | v == v_max = encodeFloorGrid f_grid w u (v + 1) u_max v_max ((ramp3 ++ " " ++ ramp2 ++ " " ++ ramp1 ++ " " ++ ramp0 ++ " " ++ [floor_terrain]) : acc)
  | otherwise = encodeFloorGrid f_grid w u (v + 1) u_max v_max ((" " ++ ramp3 ++ " " ++ ramp2 ++ " " ++ ramp1 ++ " " ++ ramp0 ++ " " ++ [floor_terrain]) : acc)
  where floor_terrain = encodeFloorTerrain f_grid w u v
        voxel = f_grid ! (w, u, v)
        ramp0 = reverse (show (fst (local_up_ramp voxel)))
        ramp1 = reverse (show (snd (local_up_ramp voxel)))
        ramp2 = reverse (show (fst (local_down_ramp voxel)))
        ramp3 = reverse (show (snd (local_down_ramp voxel)))

-- This function encodes the Obj_grid array into the engine's map file format.
encodeObjGrid :: Array (Int, Int, Int) Obj_grid -> Int -> Int -> Int -> Int -> Int -> [Char] -> [Char]
encodeObjGrid obj_grid w u v u_max v_max acc
  | w > 2 = acc ++ "\n~\n"
  | u > u_max = encodeObjGrid obj_grid (w + 1) 0 0 u_max v_max acc
  | v > v_max = encodeObjGrid obj_grid w (u + 1) 0 u_max v_max acc
  | otherwise = if voxel == def_obj_grid then encodeObjGrid obj_grid w u (v + 1) u_max v_max acc
                else encodeObjGrid obj_grid w u (v + 1) u_max v_max (encoded_voxel ++ separator ++ acc)
  where voxel = obj_grid ! (w, u, v)
        separator = if (w, u, v) == (0, 0, 0) then []
                    else ", "
        encoded_voxel = intercalate ", " (map (show) ([w, u, v, objType voxel, length (program voxel)] ++ program voxel))

-- This function encodes the Wall_grid array (for w < 0) into the engine's map file format.
encodeSubWallGrid :: Array (Int, Int, Int) Wall_grid -> Int -> Int -> Int -> Int -> Int -> Bool -> [Char] -> [Char]
encodeSubWallGrid w_grid w u v u_max v_max first_voxel acc
  | w < -3 = acc ++ "\n~\n"
  | u > u_max = encodeSubWallGrid w_grid (w - 1) 0 0 u_max v_max first_voxel acc
  | v > v_max = encodeSubWallGrid w_grid w (u + 1) 0 u_max v_max first_voxel acc
  | otherwise = if not (isNothing (obj voxel)) then encodeSubWallGrid w_grid w u (v + 1) u_max v_max False (encoded_voxel ++ separator ++ acc)
                else encodeSubWallGrid w_grid w u (v + 1) u_max v_max first_voxel acc
  where voxel = w_grid ! (w, u, v)
        obj_ = fromJust (obj voxel)
        separator = if first_voxel then []
                    else ", "
        encoded_voxel = intercalate ", " [show (-(w + 1)), show u, show v, show (ident_ obj_), show (u__ obj_), show (v__ obj_), show (w__ obj_),
                                          "0", "0", "0", "0", "0", show (texture__ obj_), show (num_elem obj_)]

