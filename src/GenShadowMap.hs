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

class ShadowMap a where
  genShadowMap :: Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) a -> SEQ.Seq GLint -> [GLint]

instance ShadowMap Wall_grid where
  genShadowMap w u v u_limit v_limit w_grid acc
    | w == 2 && u > u_limit = toList acc
    | u > u_limit = genShadowMap (w + 1) 0 0 u_limit v_limit w_grid acc
    | v > v_limit = genShadowMap w (u + 1) 0 u_limit v_limit w_grid acc
    | otherwise = genShadowMap w u (v + 1) u_limit v_limit w_grid (acc SEQ.>< encoded_voxel)
    where voxel = w_grid ! (w, u, v)
          wall_u1 = boolToGLint (u1 voxel)
          wall_u2 = boolToGLint (u2 voxel)
          wall_v1 = boolToGLint (v1 voxel)
          wall_v2 = boolToGLint (v2 voxel)
          encoded_voxel = SEQ.singleton wall_u1 SEQ.|> wall_u2 SEQ.|> wall_v1 SEQ.|> wall_v2

instance ShadowMap Floor_grid where
  genShadowMap w u v u_limit v_limit f_grid acc
    | w == 2 && u > u_limit = toList acc
    | u > u_limit = genShadowMap (w + 1) 0 0 u_limit v_limit f_grid acc
    | v > v_limit = genShadowMap w (u + 1) 0 u_limit v_limit f_grid acc
    | otherwise = genShadowMap w u (v + 1) u_limit v_limit f_grid (acc SEQ.>< encoded_voxel)
    where encoded_voxel = SEQ.singleton (terrainToGLint (surface (f_grid ! (w, div u 2, div v 2))))

