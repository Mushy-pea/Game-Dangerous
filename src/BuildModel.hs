-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- The purpose of the functions in this module mostly fall into one of the following three catagories.
-- 1. Initialisation of the arrays that hold game state using the decompressed form of the map.
-- 2. Generation of vector transforms, which are either passed to the shaders for rendering purposes or used in the game logic.
-- 3. Implementation of the ray tracing algorhythm used for visible surface determination.

{-# LANGUAGE FlexibleInstances #-}

module BuildModel where

import Prelude hiding ((!!))
import IndexWrapper0
import Data.Word
import Data.List.Split
import Data.Matrix hiding ((!))
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Maybe
import Data.IORef
import qualified Data.Sequence as SEQ
import Data.Binary
import Data.Char
import Foreign hiding (rotate)
import Foreign.C.String
import Foreign.C.Types
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Exception
import Graphics.GL.Core33

fst_ (a, b, c, d, e) = a
snd_ (a, b, c, d, e) = b
third (a, b, c, d, e) = c
fourth (a, b, c, d, e) = d
fifth (a, b, c, d, e) = e
fst__ (a, b, c) = a
snd__ (a, b, c) = b
third_ (a, b, c) = c

intToBool :: Int -> Bool
intToBool 0 = False
intToBool 1 = True

-- These two functions generate trigonometric look up tables to time optimise various functions.
makeTable :: Int -> Float -> [Float]
makeTable 0 a =
  if a > 6.29 then []
  else a : makeTable 0 (a + 0.01)
makeTable 1 a =
  if a > 6.29 then []
  else sin a : makeTable 1 (a + 0.01)
makeTable 2 a =
  if a > 6.29 then []
  else cos a : makeTable 2 (a + 0.01)
makeTable 3 a =
  if a > 6.29 then []
  else tan a : makeTable 3 (a + 0.01)

lookUp :: [[Float]] -> UArray (Int, Int) Float
lookUp a = array ((0, 0), (3, 628)) [((x, y), realToFrac ((((a, 611) !! x), 612) !! y)) | x <- [0..3], y <- [0..628]]

glfloat_ = 0 :: GLfloat; glfloat = sizeOf glfloat_
gluint_ = 0 :: GLuint; gluint = sizeOf gluint_
glint_ = 0 :: GLint; glint = sizeOf glint_
glushort_ = 0 :: GLushort; glushort = sizeOf glushort_
int__ = 0 :: Int; int_ = sizeOf int__
ptr_size = 8 :: Int -- Corresponds to the 8 byte pointers used on the Windows x86_64 platform.

-- Data types that store information about the environment and game state, as well as an exception type.
-- There are also a number of default and initial values for these types.
data Wall_grid = Wall_grid {u1 :: Bool, u2 :: Bool, v1 :: Bool, v2 :: Bool, u1_bound :: Float, u2_bound :: Float, v1_bound :: Float, v2_bound :: Float,
w_level :: Float,  wall_flag :: [Int], texture :: [Int], obj :: Maybe Obj_place} deriving (Eq, Show)

data Object = Object {ident :: Int, att_offset :: Int, num_tex :: Int, tex_w :: GLsizei, tex_h :: GLsizei, behaviours :: [Int]} deriving (Show)

data Wall_place = Wall_place {rotate :: GLint, translate_u :: Float, translate_v :: Float, translate_w :: Float, wall_flag_ :: Int,
texture_ :: Int, isNull :: Bool} deriving (Show)

data Obj_place = Obj_place {ident_ :: Int, u__ :: Float, v__ :: Float, w__ :: Float, rotation :: [Int], rotate_ :: Bool, phase :: Float, texture__ :: Int,
num_elem :: CInt, obj_flag :: Int} deriving (Eq, Show)

instance Binary Obj_place where
  put Obj_place {ident_ = a, u__ = b, v__ = c, w__ = d, texture__ = e, num_elem = f, obj_flag = g} =
    put a >> put b >> put c >> put d >> put e >> (put ((fromIntegral f) :: Int)) >> put g

  get = do a <- get
           b <- get
           c <- get
           d <- get
           e <- get
           f <- get :: Get Int
           g <- get
           return (Obj_place {ident_ = a, u__ = b, v__ = c, w__ = d, rotation = [], rotate_ = False, phase = 0, texture__ = e,
                   num_elem = (fromIntegral f) :: CInt, obj_flag = g})

data Ray_hit = U1 | U2 | V1 | V2 | Corner0 | Corner1 | Corner2 | Corner3 | U1_hit | U2_hit | V1_hit | V2_hit | Corner0_hit | Corner1_hit | Corner2_hit |
               Corner3_hit | Object_hit | Ramp_found deriving (Eq)

data Terrain = Flat | Positive_u | Negative_u | Positive_v | Negative_v | Open deriving (Eq, Show, Read)

instance Binary Terrain where
  put t = do
                      case t of
                        Flat -> put (0 :: Word8)
                        Positive_u -> put (1 :: Word8)
                        Negative_u -> put (2 :: Word8)
                        Positive_v -> put (3 :: Word8)
                        Negative_v -> put (4 :: Word8)
                        Open -> put (5 :: Word8)

  get = do t <- get :: Get Word8
           case t of
             0 -> return Flat
             1 -> return Positive_u
             2 -> return Negative_u
             3 -> return Positive_v
             4 -> return Negative_v
             5 -> return Open

data Floor_grid = Floor_grid {w_ :: Float, surface :: Terrain, local_up_ramp :: (Int, Int), local_down_ramp :: (Int, Int)} deriving (Eq, Show)

instance Binary Floor_grid where
  put Floor_grid {w_ = a, surface = b, local_up_ramp = c, local_down_ramp = d} = put a >> put b >> put c >> put d

  get = do a <- get
           b <- get
           c <- get
           d <- get
           return (Floor_grid {w_ = a, surface = b, local_up_ramp = c, local_down_ramp = d})

data Play_state0 = Play_state0 {pos_u :: Float, pos_v :: Float, pos_w :: Float, vel :: [Float], angle :: Int, angle_ :: Float, message_ :: [(Int, [Int])],
rend_mode :: Int, view_mode :: Int, view_angle :: Int, gameClock :: (Int, Float, Int), torch_t0 :: Int, torch_t_limit :: Int, on_screen_metrics :: Int,
prob_seq :: UArray Int Int, mobile_lights :: ([Float], [Float])} deriving (Eq, Show)

instance Binary Play_state0 where
  put Play_state0 {pos_u = a, pos_v = b, pos_w = c, vel = d, angle = e, angle_ = f, rend_mode = g, view_mode = h, view_angle = i, gameClock = j, torch_t0 = k, torch_t_limit = l} =
    put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i >> put j >> put k >> put l

  get = do a <- get
           b <- get
           c <- get
           d <- get
           e <- get
           f <- get
           g <- get
           h <- get
           i <- get
           j <- get
           k <- get
           l <- get
           return (Play_state0 {pos_u = a, pos_v = b, pos_w = c, vel = d, angle = e, angle_ = f, message_ = [], rend_mode = g, view_mode = h, view_angle = i,
                   gameClock = j, torch_t0 = k, torch_t_limit = l, on_screen_metrics = 0, prob_seq = def_prob_seq, mobile_lights = ([], [])})

data Play_state1 = Play_state1 {health :: Int, ammo :: Int, gems :: Int, torches :: Int, keys :: [Int], region :: [Int], difficulty :: ([Char], Int, Int, Int),
sig_q :: [Int], next_sig_q :: [Int], message :: [Int], state_chg :: Int, verbose_mode :: Bool, npc_states :: Array Int NPC_state,
story_state :: Int} deriving (Eq, Show)

instance Binary Play_state1 where
  put Play_state1 {health = a, ammo = b, gems = c, torches = d, keys = e, region = f, difficulty = g, sig_q = h, message = i, state_chg = j, npc_states = k} =
    put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i >> put j >> put k

  get = do a <- get
           b <- get
           c <- get
           d <- get
           e <- get
           f <- get
           g <- get
           h <- get
           i <- get
           j <- get
           k <- get
           return Play_state1 {health = a, ammo = b, gems = c, torches = d, keys = e, region = f, difficulty = g, sig_q = h, next_sig_q = [], message = i,
                  state_chg = j, verbose_mode = False, npc_states = k}

data NPC_state = NPC_state {npc_type :: Int, c_health :: Int, ticks_left0 :: Int, ticks_left1 :: Int, node_locations :: [Int],
fg_position :: (Float, Float, Float), dir_vector :: (Float, Float), direction :: Int, lastDir :: Int, dir_list :: [Int], node_num :: Int, end_node :: Int,
head_index :: Int, reversed :: Bool, target_u' :: Int, target_v' :: Int, target_w' :: Int, speed :: Float, avoid_dist :: Int, attack_mode :: Bool,
finalAppr :: Bool, fire_prob :: Int, fireball_state :: [(Int, Int)]} deriving (Eq, Show)

instance Binary NPC_state where
  put NPC_state {npc_type = a, c_health = b, ticks_left0 = c, ticks_left1 = d, node_locations = e, fg_position = f, dir_vector = g, direction = h, lastDir = i, dir_list = j, node_num = k, end_node = l, head_index = m, reversed = n, target_u' = o, target_v' = p, target_w' = q, speed = r, avoid_dist = s, attack_mode = t, finalAppr = u, fire_prob = v, fireball_state = w} =
    put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i >> put j >> put k >> put l >> put m >> put n >> put o >> put p >> put q >>
    put r >> put s >> put t >> put u >> put v >> put w

  get = do a <- get
           b <- get
           c <- get
           d <- get
           e <- get
           f <- get
           g <- get
           h <- get
           i <- get
           j <- get
           k <- get
           l <- get
           m <- get
           n <- get
           o <- get
           p <- get
           q <- get
           r <- get
           s <- get
           t <- get
           u <- get
           v <- get
           w <- get
           return (NPC_state {npc_type = a, c_health = b, ticks_left0 = c, ticks_left1 = d, node_locations = e, fg_position = f, dir_vector = g, direction = h,
                   lastDir = i, dir_list = j, node_num = k, end_node = l, head_index = m, reversed = n, target_u' = o, target_v' = p, target_w' = q, speed = r,
                   avoid_dist = s, attack_mode = t, finalAppr = u, fire_prob = v, fireball_state = w})

data Obj_grid = Obj_grid {objType :: Int, program :: [Int], programName :: [Char]} deriving (Eq)

instance Binary Obj_grid where
  put Obj_grid {objType = a, program = b, programName = c} = put a >> put b >> put c

  get = do a <- get
           b <- get
           c <- get
           return (Obj_grid {objType = a, program = b, programName = c})

data Game_state = Game_state {is_set :: Bool, w_grid_ :: Array (Int, Int, Int) Wall_grid, f_grid_ :: Array (Int, Int, Int) Floor_grid,
                  obj_grid_ :: Array (Int, Int, Int) Obj_grid, s0_ :: Play_state0, s1_ :: Play_state1, map_transit_string :: ([Char], [Char])}

data Io_box = Io_box {uniform_ :: UArray Int Int32, p_bind_ :: (UArray Int Word32, Int), control_ :: IORef Int}

data EngineError = Invalid_wall_flag | Invalid_obj_flag | Invalid_GPLC_opcode | Invalid_conf_reg_field | Invalid_GPLC_op_argument | Invalid_map_element |
                   NPC_feature_not_implemented deriving (Show)

instance Exception EngineError

ps0_init = Play_state0 {pos_u = 0, pos_v = 0, pos_w = 0, vel = [0, 0, 0], angle = 0, angle_ = 0, message_ = [], rend_mode = 0, view_mode = 0, view_angle = 0,
gameClock = (1, 1, 1), torch_t0 = 1, torch_t_limit = 0, on_screen_metrics = 0, prob_seq = def_prob_seq, mobile_lights = ([], [])}

ps1_init = Play_state1 {health = 100, ammo = 0, gems = 0, torches = 0, keys = [63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63],
region = [19,46,41,44,27,33,31,63,28,27,51,63,4], difficulty = ("Plenty of danger please", 6, 10, 14), sig_q = [], next_sig_q = [], message = [], state_chg = 0,
verbose_mode = False, npc_states = empty_npc_array, story_state = 0}

def_w_grid = Wall_grid {u1 = False, u2 = False, v1 = False, v2 = False, u1_bound = 0, u2_bound = 0, v1_bound = 0, v2_bound = 0, w_level = 0,  wall_flag = [],
texture = [], obj = Nothing}

defWGridArr :: Array (Int, Int, Int) Wall_grid
defWGridArr = array ((-1, 0, 0), (-3, 99, 99)) [((w, u, v), def_w_grid) | w <- [(-1)..(-3)], u <- [0..99], v <- [0..99]]

def_f_grid = Floor_grid {w_ = 0, surface = Flat, local_up_ramp = (0, 0), local_down_ramp = (0, 0)}
def_f_grid1 = Floor_grid {w_ = 0, surface = Open, local_up_ramp = (0, 0), local_down_ramp = (0, 0)}
def_f_grid_arr = array ((0, 0, 0), (2, 9, 9)) [((w, u, v), def_f_grid) | w <- [0..2], u <- [0..9], v <- [0..9]] :: Array (Int, Int, Int) Floor_grid
def_obj_grid = Obj_grid {objType = 0, program = [], programName = []}

defObjGridArr :: Array (Int, Int, Int) Obj_grid
defObjGridArr = array ((0, 0, 0), (2, 99, 99)) [((w, u, v), def_obj_grid) | w <- [0..2], u <- [0..99], v <- [0..99]] :: Array (Int, Int, Int) Obj_grid

def_obj_place = Obj_place {ident_ = 0, u__ = 0, v__ = 0, w__ = 0, rotation = [], rotate_ = False, phase = 0, texture__ = 0, num_elem = 0, obj_flag = 0}

w_grid_flag = Wall_grid {u1 = True, u2 = True, v1 = True, v2 = True, u1_bound = 0, u2_bound = 0, v1_bound = 0, v2_bound = 0, w_level = 0,  wall_flag = [],
texture = [], obj = Just def_obj_place}

f_grid_flag = Floor_grid {w_ = 3, surface = Flat, local_up_ramp = (0, 0), local_down_ramp = (0, 0)}
obj_grid_flag = Obj_grid {objType = 5, program = [], programName = []} :: Obj_grid

def_save_state = Game_state {is_set = False, w_grid_ = defWGridArr, f_grid_ = def_f_grid_arr, obj_grid_ = defObjGridArr, s0_ = ps0_init, s1_ = ps1_init}
def_wall_place = Wall_place {rotate = 0, translate_u = 0, translate_v = 0, translate_w = 0, wall_flag_ = 0, texture_ = 0, isNull = True}
def_prob_seq = array (0, 239) [(i, 0) | i <- [0..239]]

def_npc_state = NPC_state {npc_type = 0, c_health = 0, ticks_left0 = 40, ticks_left1 = 0, node_locations = [], fg_position = (0, 0, 0), dir_vector = (0, 0),
direction = 0, lastDir = 0, dir_list = [], node_num = 0, end_node = 0, head_index = 0, reversed = False, target_u' = 0, target_v' = 0, target_w' = 0, speed = 0,
avoid_dist = 0, attack_mode = False, finalAppr = False, fire_prob = 0, fireball_state = []}

empty_npc_array = array (0, 127) [(i, def_npc_state) | i <- [0..127]]

def_save_log = "1\n*save0.sav_000000\n*save1.sav_000000\n*save2.sav_000000\n*save3.sav_000000\n*save4.sav_000000\n*save5.sav_000000"

-- The implementation of the environmental ceiling is simple and usea a single model that is rendered in every frame.  The Obj_place value for this model has
-- therefore been hard coded and is added directly to the [Obj_place] taken by Main.show_object, as there is no requirement for the ray tracer to be involved.
ceiling_model = Obj_place {ident_ = 1044, u__ = 0, v__ = 0, w__ = 0, rotation = [], rotate_ = False, phase = 0, texture__ = 1, num_elem = 36, obj_flag = 0}

-- This list is used to sequence centipede NPC animation.
cpede_frames = [0, 1, 2, 3, 4, 5, 4, 3, 2, 1, 0] :: [Int]

-- This class is used in functions that filter the result of the ray tracer to avoid multiple rendering.
class Flag a where
  theFlag :: a -> Int

instance Flag Wall_place where
  theFlag a = wall_flag_ a

instance Flag Obj_place where
  theFlag a = obj_flag a

-- Deafult values used in the perspective transformation.
zNear = 0.5 :: Float; zFar = 100 :: Float

-- Used in preference to nullPtr as this value is explicitly defined as zero.
zero_ptr = (unsafeCoerce (0 :: Word64) :: Ptr a)

n = Nothing

j x = Just x

-- These functions process vertex and map data, which is either loaded into GPU memory or held in the (CPU memory) environment map respectively.
procFloats :: [[Char]] -> [Float]
procFloats [] = []
procFloats (x:xs) = (read x :: Float) : procFloats xs

procElements :: [[Char]] -> [GLushort]
procElements [] = []
procElements (x:xs) = (read x :: GLushort) : procElements xs

procInts :: [[Char]] -> [Int]
procInts [] = []
procInts (x:xs) = (read x :: Int) : procInts xs

procIntsPlus :: [[Char]] -> [Int]
procIntsPlus [] = []
procIntsPlus (x:xs)
  | x == "\n" = 536870912 : procIntsPlus xs
  | otherwise = (read x :: Int) : procIntsPlus xs

procFname :: [[Char]] -> [Ptr CChar] -> IO [Ptr CChar]
procFname [] acc = return acc
procFname (x:xs) acc = do
  p <- newCString x
  procFname xs (acc ++ [p])

procMarker :: [Int] -> [[Int]]
procMarker [] = []
procMarker (x0:x1:x2:x3:xs) = [x0, x1, x2, x3] : procMarker xs

padVertex :: [Float] -> [Float]
padVertex [] = []
padVertex (x0:x1:x2:xs) = [x0, x1, x2, 1] ++ padVertex xs

-- The vector transforms generated here are used both to transform vertices in the shaders and in parts of the game logic.
transform :: Floating a => [a] -> Matrix a -> [a]
transform [] mat = []
transform (v0:v1:v2:v3:vs) mat = toList (multStd mat (fromList 4 1 [v0, v1, v2, v3])) ++ transform vs mat

translation :: Floating a => a -> a -> a -> Matrix a
translation u v w = fromList 4 4 [1, 0, 0, u, 0, 1, 0, v, 0, 0, 1, w, 0, 0, 0, 1]

rotationU :: Int -> UArray (Int, Int) Float -> Matrix Float
rotationU a lookUp = fromList 4 4 [1, 0, 0, 0, 0, lookUp ! (2, a), - lookUp ! (1, a), 0, 0, lookUp ! (1, a), lookUp ! (2, a), 0, 0, 0, 0, 1]

rotationV :: Int -> UArray (Int, Int) Float -> Matrix Float
rotationV a lookUp = fromList 4 4 [lookUp ! (2, a), 0, - lookUp ! (1, a), 0, 0, 1, 0, 0, lookUp ! (1, a), 0, lookUp ! (2, a), 0, 0, 0, 0, 1]

rotationW :: Int -> UArray (Int, Int) Float -> Matrix Float
rotationW a lookUp = fromList 4 4 [lookUp ! (2, a), - lookUp ! (1, a), 0, 0, lookUp ! (1, a), lookUp ! (2, a), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]

modelToWorld :: Float -> Float -> Float -> Int -> Bool -> UArray (Int, Int) Float -> Matrix Float
modelToWorld u v w aw r lookUp =
  if r == True then multStd (translation u v w) (rotationW aw lookUp)
  else (translation u v w)

worldToModel :: Float -> Float -> Float -> Int -> Bool -> UArray (Int, Int) Float -> Matrix Float
worldToModel u v w aw r lookUp =
  if r == True then multStd (rotationW (628 - aw) lookUp) (translation (-u) (-v) (-w))
  else (translation (-u) (-v) (-w))

worldToCamera :: Float -> Float -> Float -> Int -> UArray (Int, Int) Float -> Matrix Float
worldToCamera u v w a lookUp = multStd (rotationV a lookUp) (multStd (fromList 4 4 [0, -1, 0, 0, 0, 0, 1, 0, -1, 0, 0, 0, 0, 0, 0, 1]) (translation u v w))

-- The modAngle functions are used to handle changes to values that encode angles in the engine's look up table reference format.
modAngle :: Int -> Int -> Int
modAngle a b =
  if a + b < 0 then a + b + 629
  else if a + b > 628 then a + b - 629
  else a + b

mod_angle' a b = b

-- This function was introduced as the transition to variable frame rate management was made.  It handles updates to a floating point representation of angle,
-- which allows for fine grain frame rate dependent changes to the player angle to be made.
modAngle_ :: Float -> Float -> Bool -> Float
modAngle_ a f_rate clockwise =
  if clockwise == True then
    if a - 5 < 0 then 200 * pi + (a - 5)
    else a - 5
  else
    if a + 5 > 200 * pi then 5 - (200 * pi - a)
    else a + 5

-- These functions implement a ray tracing algorhythm, which is part of the visible surface determination (VSD) system and is used for line of sight checks by
-- the non - player character logic.  The Ray_test class exists so that the ray tracer can conveniently provide differing functionality when called from the
-- VSD system or game logic.
fBlock :: Int -> Int -> Int -> Terrain -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) Obj_grid -> Int
fBlock w u v f_target0 f_grid obj_grid =
  let f_target1 = surface (f_grid ! (w - 1, div u 2, div v 2))
  in
  if f_target0 == Open && f_target1 == Positive_u && objType (obj_grid ! (w - 1, u, v)) == 0 then -1
  else if f_target0 == Open && f_target1 == Negative_u && objType (obj_grid ! (w - 1, u, v)) == 0 then -2
  else if f_target0 == Open && f_target1 == Positive_v && objType (obj_grid ! (w - 1, u, v)) == 0 then -3
  else if f_target0 == Open && f_target1 == Negative_v && objType (obj_grid ! (w - 1, u, v)) == 0 then -4
  else if f_target0 == Positive_u && objType (obj_grid ! (w, u, v)) == 0 then -5
  else if f_target0 == Negative_u && objType (obj_grid ! (w, u, v)) == 0 then -6
  else if f_target0 == Positive_v && objType (obj_grid ! (w, u, v)) == 0 then -7
  else if f_target0 == Negative_v && objType (obj_grid ! (w, u, v)) == 0 then -8
  else 0

class Ray_test a where
  intersect :: Ray_hit -> Array (Int, Int, Int) a -> Array (Int, Int, Int) Floor_grid -> Float -> Float -> Int -> Int -> Int -> Int -> Int -> (Ray_hit, Int)

  scan_cube :: Array (Int, Int, Int) a -> Int -> Int -> Int -> [Obj_place]

instance Ray_test Obj_grid where
  intersect U1 obj_grid f_grid u v w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (truncate (u - 0.025)) 2, div v_block 2))
        f_block_ = fBlock w_block (u_block - 1) v_block f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if objType (obj_grid ! (w_block, u_block - 1, v_block)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block - 1) 2, div v_block 2)) /= Flat then (Object_hit, 0)
      else (U1, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if objType (obj_grid ! (w_block, u_block - 1, v_block)) > 0 then (Object_hit, 0)
      else (U1, 0)
  intersect U2 obj_grid f_grid u v w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (truncate (u + 0.025)) 2, div v_block 2))
        f_block_ = fBlock w_block (u_block + 1) v_block f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if objType (obj_grid ! (w_block, u_block + 1, v_block)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block + 1) 2, div v_block 2)) /= Flat then (Object_hit, 0)
      else (U2, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if objType (obj_grid ! (w_block, u_block + 1, v_block)) > 0 then (Object_hit, 0)
      else (U2, 0)
  intersect V1 obj_grid f_grid u v w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div u_block 2, div (truncate (v - 0.025)) 2))
        f_block_ = fBlock w_block u_block (v_block - 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if objType (obj_grid ! (w_block, u_block, v_block - 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div u_block 2, div (v_block - 1) 2)) /= Flat then (Object_hit, 0)
      else (V1, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if objType (obj_grid ! (w_block, u_block, v_block - 1)) > 0 then (Object_hit, 0)
      else (V1, 0)
  intersect V2 obj_grid f_grid u v w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div u_block 2, div (truncate (v + 0.025)) 2))
        f_block_ = fBlock w_block u_block (v_block + 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if objType (obj_grid ! (w_block, u_block, v_block + 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div u_block 2, div (v_block + 1) 2)) /= Flat then (Object_hit, 0)
      else (V2, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if objType (obj_grid ! (w_block, u_block, v_block + 1)) > 0 then (Object_hit, 0)
      else (V2, 0)
  intersect Corner0 obj_grid f_grid u v w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (truncate (u - 0.025)) 2, div (truncate (v + 0.025)) 2))
        f_block_ = fBlock w_block (u_block - 1) (v_block + 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if objType (obj_grid ! (w_block, u_block - 1, v_block + 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block - 1) 2, div (v_block + 1) 2)) /= Flat then (Object_hit, 0)
      else (Corner0, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if objType (obj_grid ! (w_block, u_block - 1, v_block + 1)) > 0 then (Object_hit, 0)
      else (Corner0, 0)
  intersect Corner1 obj_grid f_grid u v w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (truncate (u + 0.025)) 2, div (truncate (v + 0.025)) 2))
        f_block_ = fBlock w_block (u_block + 1) (v_block + 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if objType (obj_grid ! (w_block, u_block + 1, v_block + 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block + 1) 2, div (v_block + 1) 2)) /= Flat then (Object_hit, 0)
      else (Corner1, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if objType (obj_grid ! (w_block, u_block + 1, v_block + 1)) > 0 then (Object_hit, 0)
      else (Corner1, 0)
  intersect Corner2 obj_grid f_grid u v w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (truncate (u + 0.025)) 2, div (truncate (v - 0.025)) 2))
        f_block_ = fBlock w_block (u_block + 1) (v_block - 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if objType (obj_grid ! (w_block, u_block + 1, v_block - 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block + 1) 2, div (v_block - 1) 2)) /= Flat then (Object_hit, 0)
      else (Corner2, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if objType (obj_grid ! (w_block, u_block + 1, v_block - 1)) > 0 then (Object_hit, 0)
      else (Corner2, 0)
  intersect Corner3 obj_grid f_grid u v w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (truncate (u - 0.025)) 2, div (truncate (v - 0.025)) 2))
        f_block_ = fBlock w_block (u_block - 1) (v_block - 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if objType (obj_grid ! (w_block, u_block - 1, v_block - 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block - 1) 2, div (v_block - 1) 2)) /= Flat then (Object_hit, 0)
      else (Corner3, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if objType (obj_grid ! (w_block, u_block - 1, v_block - 1)) > 0 then (Object_hit, 0)
      else (Corner3, 0)

  scan_cube obj_grid w u v = []

instance Ray_test Wall_grid where
  intersect U1 w_grid f_grid u v w_block u_block v_block seek_mode c =
    if u1 (w_grid ! (w_block, u_block, v_block)) == True then (U1_hit, 0)
    else (U1, 0)
  intersect U2 w_grid f_grid u v w_block u_block v_block seek_mode c =
    if u2 (w_grid ! (w_block, u_block, v_block)) == True then (U2_hit, 0)
    else (U2, 0)
  intersect V1 w_grid f_grid u v w_block u_block v_block seek_mode c =
    if v1 (w_grid ! (w_block, u_block, v_block)) == True then (V1_hit, 0)
    else (V1, 0)
  intersect V2 w_grid f_grid u v w_block u_block v_block seek_mode c =
    if v2 (w_grid ! (w_block, u_block, v_block)) == True then (V2_hit, 0)
    else (V2, 0)
  intersect Corner0 w_grid f_grid u v w_block u_block v_block seek_mode c =
    if v2 (w_grid ! (w_block, u_block, v_block)) == True || u1 (w_grid ! (w_block, u_block, v_block)) == True then (Corner0_hit, 0)
    else (Corner0, 0)
  intersect Corner1 w_grid f_grid u v w_block u_block v_block seek_mode c =
    if u2 (w_grid ! (w_block, u_block, v_block)) == True || v2 (w_grid ! (w_block, u_block, v_block)) == True then (Corner1_hit, 0)
    else (Corner1, 0)
  intersect Corner2 w_grid f_grid u v w_block u_block v_block seek_mode c =
    if v1 (w_grid ! (w_block, u_block, v_block)) == True || u2 (w_grid ! (w_block, u_block, v_block)) == True then (Corner2_hit, 0)
    else (Corner2, 0)
  intersect Corner3 w_grid f_grid u v w_block u_block v_block seek_mode c =
    if u1 (w_grid ! (w_block, u_block, v_block)) == True || v1 (w_grid ! (w_block, u_block, v_block)) == True then (Corner3_hit, 0)
    else (Corner3, 0)

  scan_cube w_grid w u v = maybeToList (obj (w_grid ! (w, u, v))) ++ maybeToList (obj (w_grid ! (-w - 1, u, v)))

rayTrace1 :: Float -> Float -> Float -> Float -> Float -> Float -> Int -> Bool -> Bool -> UArray (Int, Int) Float -> Ray_hit
rayTrace1 u v u1_bound u2_bound v1_bound v2_bound a True True lookUp =
  if (v2_bound - v) / (u2_bound - u) == lookUp ! (3, a) then Corner1
  else if (v2_bound - v) / (u2_bound - u) > lookUp ! (3, a) then U2
  else V2
rayTrace1 u v u1_bound u2_bound v1_bound v2_bound a True False lookUp =
  if (v - v1_bound) / (u2_bound - u) == lookUp ! (3, a) then Corner2
  else if (v - v1_bound) / (u2_bound - u) > lookUp ! (3, a) then U2
  else V1
rayTrace1 u v u1_bound u2_bound v1_bound v2_bound a False True lookUp =
  if (v2_bound - v) / (u - u1_bound) == lookUp ! (3, a) then Corner0
  else if (v2_bound - v) / (u - u1_bound) > lookUp ! (3, a) then U1
  else V2
rayTrace1 u v u1_bound u2_bound v1_bound v2_bound a False False lookUp =
  if (v - v1_bound) / (u - u1_bound) == lookUp ! (3, a) then Corner3
  else if (v - v1_bound) / (u - u1_bound) > lookUp ! (3, a) then U1
  else V1

rayTrace0 :: Ray_test a => Float -> Float -> Int -> Bool -> Bool -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
             -> Array (Int, Int, Int) a -> UArray (Int, Int) Float -> Int -> [Obj_place] -> Int -> Int -> Int -> Int -> (Wall_place, [Obj_place], Int)
{-# SPECIALISE rayTrace0 :: Float -> Float -> Int -> Bool -> Bool -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
                            -> Array (Int, Int, Int) Wall_grid -> UArray (Int, Int) Float -> Int -> [Obj_place] -> Int -> Int -> Int -> Int
                            -> (Wall_place, [Obj_place], Int) #-}
rayTrace0 u v a u_positive v_positive u_block v_block w_grid f_grid grid lookUp w_block acc target_u target_v seek_mode c =
  let grid_i = w_grid ! (w_block, u_block, v_block)
      result = intersect (rayTrace1 u v (u1_bound grid_i) (u2_bound grid_i) (v1_bound grid_i) (v2_bound grid_i) a u_positive v_positive lookUp) grid f_grid u v
                         w_block u_block v_block seek_mode c
      found = scan_cube grid w_block u_block v_block ++ acc
  in
  if seek_mode == 1 && c == 2 then (def_wall_place, [], 0)
  else if seek_mode > 1 && u_block == target_u && v_block == target_v then (def_wall_place, [], 0)
  else if fst result == U1 && v_positive == True then rayTrace0 (u1_bound grid_i) (v + (lookUp ! (3, a)) * (u - u1_bound grid_i)) a u_positive v_positive
                                                                (u_block - 1) v_block w_grid f_grid grid lookUp w_block found target_u target_v seek_mode
                                                                (c + 1)
  else if fst result == U1 && v_positive == False then rayTrace0 (u1_bound grid_i) (v - (lookUp ! (3, a)) * (u - u1_bound grid_i)) a u_positive v_positive
                                                                 (u_block - 1) v_block w_grid f_grid grid lookUp w_block found target_u target_v seek_mode
                                                                 (c + 1)
  else if fst result == U2 && v_positive == True then rayTrace0 (u2_bound grid_i) (v + (lookUp ! (3, a)) * ((u2_bound grid_i) - u)) a u_positive v_positive
                                                                (u_block + 1) v_block w_grid f_grid grid lookUp w_block found target_u target_v seek_mode
                                                                (c + 1)
  else if fst result == U2 && v_positive == False then rayTrace0 (u2_bound grid_i) (v - (lookUp ! (3, a)) * ((u2_bound grid_i) - u)) a u_positive v_positive
                                                                 (u_block + 1) v_block w_grid f_grid grid lookUp w_block found target_u target_v seek_mode
                                                                 (c + 1)
  else if fst result == V1 && u_positive == True then rayTrace0 (u + (1 / lookUp ! (3, a)) * (v - v1_bound grid_i)) (v1_bound grid_i) a u_positive v_positive
                                                                u_block (v_block - 1) w_grid f_grid grid lookUp w_block found target_u target_v seek_mode
                                                                (c + 1)
  else if fst result == V1 && u_positive == False then rayTrace0 (u - (1 / lookUp ! (3, a)) * (v - v1_bound grid_i)) (v1_bound grid_i) a u_positive v_positive
                                                                 u_block (v_block - 1) w_grid f_grid grid lookUp w_block found target_u target_v seek_mode
                                                                 (c + 1)
  else if fst result == V2 && u_positive == True then rayTrace0 (u + (1 / lookUp ! (3, a)) * ((v2_bound grid_i) - v)) (v2_bound grid_i) a u_positive v_positive
                                                                u_block (v_block + 1) w_grid f_grid grid lookUp w_block found target_u target_v seek_mode
                                                                (c + 1)
  else if fst result == V2 && u_positive == False then rayTrace0 (u - (1 / lookUp ! (3, a)) * ((v2_bound grid_i) - v)) (v2_bound grid_i) a u_positive v_positive
                                                                 u_block (v_block + 1) w_grid f_grid grid lookUp w_block found target_u target_v seek_mode
                                                                 (c + 1)
  else if fst result == Corner0 then rayTrace0 (u1_bound grid_i) (v2_bound grid_i) a u_positive v_positive (u_block - 1) (v_block + 1) w_grid f_grid grid lookUp
                                               w_block found target_u target_v seek_mode (c + 1)
  else if fst result == Corner1 then rayTrace0 (u2_bound grid_i) (v2_bound grid_i) a u_positive v_positive (u_block + 1) (v_block + 1) w_grid f_grid grid lookUp
                                               w_block found target_u target_v seek_mode (c + 1)
  else if fst result == Corner2 then rayTrace0 (u2_bound grid_i) (v1_bound grid_i) a u_positive v_positive (u_block + 1) (v_block - 1) w_grid f_grid grid lookUp
                                               w_block found target_u target_v seek_mode (c + 1)
  else if fst result == U1_hit then (Wall_place {rotate = 0, translate_u = u1_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i,
                                                 wall_flag_ = ((wall_flag grid_i), 74) !! 3, texture_ = ((texture grid_i), 75) !! 3, isNull = False}, found, 0)
  else if fst result == U2_hit then (Wall_place {rotate = 1, translate_u = u2_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i,
                                                 wall_flag_ = ((wall_flag grid_i), 76) !! 1, texture_ = ((texture grid_i), 77) !! 1, isNull = False}, found, 0)
  else if fst result == V1_hit then (Wall_place {rotate = 2, translate_u = u1_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i,
                                                 wall_flag_ = ((wall_flag grid_i), 78) !! 2, texture_ = ((texture grid_i), 79) !! 2, isNull = False}, found, 0)
  else if fst result == V2_hit then (Wall_place {rotate = 3, translate_u = u1_bound grid_i, translate_v = v2_bound grid_i, translate_w = w_level grid_i,
                                                 wall_flag_ = ((wall_flag grid_i), 80) !! 0, texture_ = ((texture grid_i), 81) !! 0, isNull = False}, found, 0)
  else if fst result == Corner0_hit then (Wall_place {rotate = 3, translate_u = u1_bound grid_i, translate_v = v2_bound grid_i, translate_w = w_level grid_i,
                                          wall_flag_ = ((wall_flag grid_i), 82) !! 0, texture_ = ((texture grid_i), 83) !! 0, isNull = False}, found, 0)
  else if fst result == Corner1_hit then (Wall_place {rotate = 1, translate_u = u2_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i,
                                                      wall_flag_ = ((wall_flag grid_i), 84) !! 1, texture_ = ((texture grid_i), 85) !! 1, isNull = False},
                                         found, 0)
  else if fst result == Corner2_hit then (Wall_place {rotate = 2, translate_u = u1_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i,
                                                      wall_flag_ = ((wall_flag grid_i), 86) !! 2, texture_ = ((texture grid_i), 87) !! 2, isNull = False},
                                         found, 0)
  else if fst result == Corner3_hit then (Wall_place {rotate = 0, translate_u = u1_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i,
                                                      wall_flag_ = ((wall_flag grid_i), 88) !! 3, texture_ = ((texture grid_i), 89) !! 3, isNull = False},
                                         found, 0)
  else if fst result == Object_hit then (def_wall_place, [], c)
  else if fst result == Ramp_found then (def_wall_place, [], snd result)
  else rayTrace0 (u1_bound grid_i) (v1_bound grid_i) a u_positive v_positive (u_block - 1) (v_block - 1) w_grid f_grid grid lookUp w_block found target_u
                 target_v seek_mode (c + 1)

procAngle :: Int -> (Int, Bool, Bool)
{-# INLINE procAngle #-}
procAngle a =
  if a < 158 then (a, True, True)
  else if a < 315 then (157 - (a - 157), False, True)
  else if a < 472 then (a - 314, False, False)
  else (157 - (a - 471), True, False)

-- These two functions handle the tracing of rays over the range of the field of view, returning a list of the wall sections that border the visible region and
-- a list of any objects visible within that region.
surveyView :: Int -> Int -> Int -> Float -> Float -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
              -> UArray (Int, Int) Float -> Int -> [Wall_place] -> [Obj_place] -> ([Wall_place], [Obj_place])
surveyView a da limit u v u_block v_block w_grid f_grid lookUp w_block acc0 acc1 =
  let proc_angle_ = procAngle a
      ray = rayTrace0 u v (fst__ proc_angle_) (snd__ proc_angle_) (third_ proc_angle_) u_block v_block w_grid f_grid w_grid lookUp w_block [] 0 0 0 0
  in
  if da > limit then (acc0, acc1)
  else surveyView (modAngle a 2) (da + 2) limit u v u_block v_block w_grid f_grid lookUp w_block (acc0 ++ [fst__ ray]) (acc1 ++ snd__ ray)

multiSurvey :: Int -> Int -> Float -> Float -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
               -> Array (Int, Int, Int) Obj_grid -> UArray (Int, Int) Float -> Int -> Int -> [Wall_place] -> [Obj_place] -> ([Wall_place], [Obj_place])
multiSurvey a a_limit u v u_block v_block w_grid f_grid obj_grid lookUp w_limit w_block acc0 acc1 =
  let survey = (surveyView a 0 a_limit u v u_block v_block w_grid f_grid lookUp w_block [] [])
  in
  if w_block > w_limit then (acc0, acc1)
  else multiSurvey a a_limit u v u_block v_block w_grid f_grid obj_grid lookUp w_limit (w_block + 1) (acc0 ++ fst survey) (acc1 ++ snd survey)

-- This function filters the output of the ray tracer to avoid multiple rendering.  It has been implemented using direct memory access because of the
-- performance critical role of this logic.
filterSurv :: Flag a => Int -> [a] -> [a] -> Ptr Int -> Int -> IO [a]
filterSurv mode [] acc p_table game_t = return acc
filterSurv mode (x:xs) acc p_table game_t = do
  if mode == 0 && (theFlag x > 119999 || theFlag x < 0) then error ("\nfilterSurv: wall_flag_ value out of range: " ++ show (theFlag x))
  else if mode == 1 && (theFlag x > 37499 || theFlag x < 0) then error ("\nfilterSurv: obj_flag value out of range: " ++ show (theFlag x))
  else do
    test <- peekElemOff p_table (theFlag x)
    if test == game_t then filterSurv mode xs acc p_table game_t
    else do
      pokeElemOff p_table (theFlag x) game_t
      filterSurv mode xs (acc ++ [x]) p_table game_t

-- These functions process the wall and floor grid data from the level map file before it is used to form the environment map.
loadGrid1 :: [Char] -> Bool
loadGrid1 i =
  if (read i :: Int) == 0 then False
  else True

loadGrid0 :: [[Char]] -> [Wall_grid]
loadGrid0 [] = []
loadGrid0 (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:x14:x15:x16:x17:xs) =
  if (read x17 :: Int) == 0 then Wall_grid {u1 = loadGrid1 x0, u2 = loadGrid1 x1, v1 = loadGrid1 x2, v2 = loadGrid1 x3, u1_bound = read x4, u2_bound = read x5,
                                            v1_bound = read x6, v2_bound = read x7, w_level = read x8, wall_flag = procInts [x9, x10, x11, x12],
                                            texture = procInts [x13, x14, x15, x16], obj = Nothing} : loadGrid0 xs
  else Wall_grid {u1 = loadGrid1 x0, u2 = loadGrid1 x1, v1 = loadGrid1 x2, v2 = loadGrid1 x3, u1_bound = read x4, u2_bound = read x5, v1_bound = read x6,
                  v2_bound = read x7, w_level = read x8, wall_flag = procInts [x9, x10, x11, x12], texture = procInts [x13, x14, x15, x16],
                  obj =  Just Obj_place {ident_ = read ((xs, 90) !! 0), u__ = read ((xs, 91) !! 1), v__ = read ((xs, 92) !! 2), w__ = read ((xs, 93) !! 3),
                  rotation = procInts (take 3 (drop 4 xs)), rotate_ = loadGrid1 ((xs, 94) !! 7), phase = read ((xs, 95) !! 8), texture__ = read ((xs, 96) !! 9),
                  num_elem = read ((xs, 97) !! 10), obj_flag = read ((xs, 98) !! 11)}} : loadGrid0 (drop 12 xs)

sortGrid1 :: [[Char]] -> [[Wall_grid]]
sortGrid1 [] = []
sortGrid1 (x:xs) = loadGrid0 (splitOn ", " x) : sortGrid1 xs

sortGrid0 :: [[Char]] -> [[[Wall_grid]]]
sortGrid0 [] = []
sortGrid0 (x:xs) = sortGrid1 (splitOn ":" x) : sortGrid0 xs

makeArray2 :: Int -> Int -> Int
makeArray2 i0 i1 = (i1 - i0)

makeArray1 :: [[[a]]] -> Int -> Int -> Int -> Array (Int, Int, Int) a
makeArray1 grid u_max v_max w_max = array ((0, 0, 0), (w_max, u_max, v_max))
                                          [((w, u, v), ((((((grid, 613) !! w), 614) !! u), 615) !! v)) |w <- [0..w_max], u <- [0..u_max], v <- [0..v_max]]

makeArray0 :: [[[a]]] -> Int -> Int -> Int -> Array (Int, Int, Int) a
makeArray0 grid u_max v_max w_max = array ((-w_max - 1, 0, 0), (w_max, u_max, v_max))
                                          [((w, u, v), ((((((grid, 616) !! (makeArray2 (-w_max - 1) w)), 617) !! u), 618) !! v)) |w <- [-w_max - 1..w_max], u <- [0..u_max], v <- [0..v_max]]

loadFloor2 :: [[Char]] -> [Floor_grid]
loadFloor2 [] = []
loadFloor2 (x0:x1:x2:x3:x4:x5:xs) =
  if x1 == "0" then Floor_grid {w_ = read x0, surface = Flat, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)} : loadFloor2 xs
  else if x1 == "1" then Floor_grid {w_ = read x0, surface = Positive_u, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)}
                         : loadFloor2 xs
  else if x1 == "2" then Floor_grid {w_ = read x0, surface = Negative_u, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)}
                         : loadFloor2 xs
  else if x1 == "3" then Floor_grid {w_ = read x0, surface = Positive_v, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)}
                         : loadFloor2 xs
  else if x1 == "4" then Floor_grid {w_ = read x0, surface = Negative_v, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)}
                         : loadFloor2 xs
  else Floor_grid {w_ = read x0, surface = Open, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)} : loadFloor2 xs

loadFloor1 :: [[Char]] -> [[Floor_grid]]
loadFloor1 [] = []
loadFloor1 (x:xs) = loadFloor2 (splitOn ", " x) : loadFloor1 xs

loadFloor0 :: [[Char]] -> [[[Floor_grid]]]
loadFloor0 [] = []
loadFloor0 (x:xs) = loadFloor1 (splitOn ":" x) : loadFloor0 xs

-- These two functions process object description data, which is used to set up OpenGL vertex array and texture objects.
loadObject1 :: [[Char]] -> Object
loadObject1 (x0:x1:x2:x3:x4:xs) = Object {ident = read x0, att_offset = read x1, num_tex = read x2, tex_w = read x3, tex_h = read x4, behaviours = procInts xs}

loadObject0 :: [[Char]] -> [Object]
loadObject0 [] = []
loadObject0 (x:xs) = loadObject1 (splitOn ", " x) : loadObject0 xs

-- These functions are also used to genarate the environment map from a map file.
emptyObjGrid :: Int -> Int -> Int -> Array (Int, Int, Int) Obj_grid
emptyObjGrid u_max v_max w_max = array ((0, 0, 0), (w_max, u_max, v_max)) [((w, u, v), def_obj_grid) | w <- [0..w_max], u <- [0..u_max], v <- [0..v_max]]

loadObjGrid :: Int -> [[Char]] -> [((Int, Int, Int), Obj_grid)]
loadObjGrid mode [] = []
loadObjGrid mode (x0:x1:x2:x3:x4:xs)
  | xs == [] = [((read x0, read x1, read x2),
                Obj_grid {objType = read x3, program = [], programName = "null"})]
  | isDigit (head (head xs)) = ((read x0, read x1, read x2),
                                Obj_grid {objType = read x3, program = read_program (take (read x4) xs), programName = "null"})
                                : loadObjGrid mode (drop (read x4) xs)
  | otherwise = ((read x0, read x1, read x2),
                 Obj_grid {objType = read x3, program = read_program (take ((read x4) - 1) (tail xs)), programName = head xs})
                 : loadObjGrid mode (drop (read x4) xs)
  where read_program = \prog -> if mode == 0 then procInts prog
                                else procIntsPlus prog

emptyWGrid :: Int -> Int -> Int -> Array (Int, Int, Int) Wall_grid
emptyWGrid u_max v_max w_max = array ((0, 0, 0), (w_max, u_max, v_max))
                                     [((w, u, v), Wall_grid {u1 = False, u2 = False, v1 = False, v2 = False, u1_bound = 0, u2_bound = 0, v1_bound = 0, v2_bound = 0, w_level = 0,  wall_flag = [], texture = [], obj = Nothing}) | w <- [0..w_max], u <- [0..u_max], v <- [0..v_max]]

buildTable1 :: [[Char]] -> Array (Int, Int, Int) Wall_grid -> Int -> Array (Int, Int, Int) Wall_grid
buildTable1 [] w_grid c = w_grid
buildTable1 (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:xs) w_grid c =
  if c > 37499 then throw Invalid_obj_flag
  else buildTable1 xs (w_grid // [((read x0, read x1, read x2), def_w_grid {obj = Just Obj_place {ident_ = read x3, u__ = read x4, v__ = read x5, w__ = read x6,
                                                                            rotation = procInts [x7, x8, x9], rotate_ = loadGrid1 x10, phase = read x11,
                                                                            texture__ = read x12, num_elem = read x13, obj_flag = c}})]) (c + 1)

buildTable0 :: [Wall_grid] -> Int -> Int -> Int -> [[[Wall_grid]]]
buildTable0 w_grid u_max v_max w_max = reverse (map (splitEvery (v_max + 1)) (splitEvery ((u_max + 1) * (v_max + 1)) w_grid))

-- Used to generate the set of allowed camera positions in third person rendering mode.
viewCircle :: Float -> Float -> Float -> Int -> UArray (Int, Int) Float -> (Float, Float)
viewCircle a b r t lookUp = (a + r * lookUp ! (2, t), b + r * lookUp ! (1, t))

-- Used to query the conf_reg array, which holds startup parameters passed at the command line or from the engine's configuration file.
cfg :: Array Int [Char] -> Int -> [Char] -> [Char]
cfg conf_reg i query =
  if i > 88 then error ("Invalid conf_reg field in query operation: " ++ query ++ "!")
  else if conf_reg ! i == query then conf_reg ! (i + 1)
  else cfg conf_reg (i + 2) query

-- Used to update the conf_reg array.
updateCfg :: Array Int [Char] -> [Char] -> [Char] -> Int -> Array Int [Char]
updateCfg conf_reg field update i =
  if i > 86 then error ("Invalid conf_reg field in update operation: " ++ field ++ "!")
  else if conf_reg ! i == field then conf_reg // [((i + 1), update)]
  else updateCfg conf_reg field update (i + 2)

-- Used to construct a string representation of the conf_reg array so that an updated version can be saved to disk.
writeCfg :: Array Int [Char] -> Int -> [Char]
writeCfg conf_reg i =
  if i > 86 then []
  else (conf_reg ! i) ++ "=" ++ (conf_reg ! (i + 1)) ++ "\n" ++ writeCfg conf_reg (i + 2)

-- Used to initialise the p_bind array, which contains references to all the OpenGL vertex array objects and texture objects used in the current map.
bufferToArray :: Ptr Word32 -> UArray Int Word32 -> Int -> Int -> Int -> IO (UArray Int Word32)
bufferToArray p arr i0 i1 limit = do
  if i0 > limit then return arr
  else do
    val <- peekElemOff p i0
    bufferToArray p (arr // [(i1, val)]) (i0 + 1) (i1 + 1) limit

checkMapLayer :: Eq a => Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) a -> a -> Array (Int, Int, Int) a
checkMapLayer w u v u_limit v_limit grid flag =
  if w == 2 && u > u_limit then grid
  else if u > u_limit then checkMapLayer (w + 1) 0 0 u_limit v_limit grid flag
  else if v > v_limit then checkMapLayer w (u + 1) 0 u_limit v_limit grid flag
  else
    if grid ! (w, u, v) == flag then throw Invalid_map_element
    else checkMapLayer w u (v + 1) u_limit v_limit grid flag

-- This function determines the differential between an original map state array (Wall_grid, Floor_grid or Obj_grid) and a newer map state.  It is part of the
-- implementation of the game state saving system.
genArrayDiff :: Eq a => Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) a -> Array (Int, Int, Int) a -> SEQ.Seq ((Int, Int, Int), a)
                -> SEQ.Seq ((Int, Int, Int), a)
genArrayDiff w u v u_limit v_limit arr0 arr1 acc =
  if w == 2 && u > u_limit then acc
  else if u > u_limit then genArrayDiff (w + 1) 0 0 u_limit v_limit arr0 arr1 acc
  else if v > v_limit then genArrayDiff w (u + 1) 0 u_limit v_limit arr0 arr1 acc
  else
    if arr0 ! (w, u, v) == arr1 ! (w, u, v) then genArrayDiff w u (v + 1) u_limit v_limit arr0 arr1 acc
    else genArrayDiff w u (v + 1) u_limit v_limit arr0 arr1 (acc SEQ.>< (SEQ.singleton ((w, u, v), (arr1 ! (w, u, v)))))

-- This function processes text from input files to return a result that is independent on whether the file has the Windows or Unix end of file format.
tailFile :: [Char] -> [Char]
tailFile contents =
  if last (splitOn "\n" contents) == [] then init contents
  else contents

