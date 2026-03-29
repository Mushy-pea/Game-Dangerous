-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This module is part of the light source to vertex ray cast shadow system.

module GenShadowMap where

import Data.Word
import Data.Foldable
import qualified Data.Sequence as SEQ
import qualified Data.ByteString as BS
import Data.Array.IArray
import Graphics.GL.Core33
import BuildModel

boolToGLint :: Bool -> GLint
boolToGLint True = 1
boolToGLint False = 0

terrainToGLint :: Terrain -> GLint
terrainToGLint Open = 0
terrainToGLint _ = 1

encodedWallVoxel :: Int -> Int -> Int -> Int -> GLint -> GLint -> GLint -> GLint -> Obj_grid -> SEQ.Seq GLint
encodedWallVoxel u v u_limit v_limit wall_u1 wall_u2 wall_v1 wall_v2 obj_voxel
  | u > u_limit || v > v_limit = SEQ.singleton (boolToGLint False) SEQ.|> boolToGLint False SEQ.|>
                                 boolToGLint False SEQ.|> boolToGLint False
  | otherwise =
    if objType obj_voxel == 2 then
      SEQ.singleton (boolToGLint True) SEQ.|> boolToGLint True SEQ.|>
      boolToGLint True SEQ.|> boolToGLint True
    else
      SEQ.singleton wall_u1 SEQ.|> wall_u2 SEQ.|> wall_v1 SEQ.|> wall_v2

class ShadowMap a where
  genShadowMap :: Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) a -> Array (Int, Int, Int) Obj_grid -> SEQ.Seq GLint -> [GLint]

instance ShadowMap Wall_grid where
  genShadowMap w u v u_limit v_limit w_grid obj_grid acc
    | w == 2 && u > 59 = toList acc
    | u > 59 = genShadowMap (w + 1) 0 0 u_limit v_limit w_grid obj_grid acc
    | v > 59 = genShadowMap w (u + 1) 0 u_limit v_limit w_grid obj_grid acc
    | otherwise = genShadowMap w u (v + 1) u_limit v_limit w_grid obj_grid (acc SEQ.>< encoded_voxel)
    where wall_voxel = w_grid ! (w, u, v)
          obj_voxel = obj_grid ! (w, u, v)
          wall_u1 = boolToGLint (u1 wall_voxel)
          wall_u2 = boolToGLint (u2 wall_voxel)
          wall_v1 = boolToGLint (v1 wall_voxel)
          wall_v2 = boolToGLint (v2 wall_voxel)
          encoded_voxel = encodedWallVoxel u v u_limit v_limit wall_u1 wall_u2 wall_v1 wall_v2 obj_voxel

instance ShadowMap Floor_grid where
  genShadowMap w u v u_limit v_limit f_grid obj_grid acc
    | w == 2 && u > 59 = toList acc
    | u > 59 = genShadowMap (w + 1) 0 0 u_limit v_limit f_grid obj_grid acc
    | v > 59 = genShadowMap w (u + 1) 0 u_limit v_limit f_grid obj_grid acc
    | otherwise = genShadowMap w u (v + 1) u_limit v_limit f_grid obj_grid (acc SEQ.>< encoded_voxel)
    where encoded_voxel = if u > u_limit || v > v_limit then 
                            SEQ.singleton (terrainToGLint Open)
                          else
                            SEQ.singleton (terrainToGLint (surface (f_grid ! (w, div u 2, div v 2))))

