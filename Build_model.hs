-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- The purpose of the functions in this module mostly fall into one of the following three catagories.
-- 1. Initialisation of the arrays that hold game state using the decompressed form of the map.
-- 2. Generation of vector transforms, which are either passed to the shaders for rendering purposes or used in the game logic.
-- 3. Implementation of the ray tracing algorhythm used for visible surface determination.

module Build_model where

import Graphics.Win32
import Data.Word
import Data.List.Split
import Data.Matrix hiding ((!))
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Maybe
import Data.IORef
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

-- These two functions generate trigonometric look up tables to time optimise various functions.
make_table :: Int -> Float -> [Float]
make_table 0 a =
  if a > 6.29 then []
  else a : make_table 0 (a + 0.01)
make_table 1 a =
  if a > 6.29 then []
  else sin a : make_table 1 (a + 0.01)
make_table 2 a =
  if a > 6.29 then []
  else cos a : make_table 2 (a + 0.01)
make_table 3 a =
  if a > 6.29 then []
  else tan a : make_table 3 (a + 0.01)

look_up :: [[Float]] -> UArray (Int, Int) Float
look_up a = array ((0, 0), (3, 628)) [((x, y), realToFrac ((a !! x) !! y)) | x <- [0..3], y <- [0..628]]

glfloat_ = 0 :: GLfloat; glfloat = sizeOf glfloat_
gluint_ = 0 :: GLuint; gluint = sizeOf gluint_
glint_ = 0 :: GLint; glint = sizeOf glint_
glushort_ = 0 :: GLushort; glushort = sizeOf glushort_
int__ = 0 :: Int; int_ = sizeOf int__
ptr_size = 8 :: Int -- Corresponds to the 8 byte pointers used on the Windows x86_64 platform.  This value should be changed to 4 if compiling for systems with 4 byte pointers

-- Data types that store information about the environment and game state, as well as an exception type.
data Wall_grid = Wall_grid {u1 :: Bool, u2 :: Bool, v1 :: Bool, v2 :: Bool, u1_bound :: Float, u2_bound :: Float, v1_bound :: Float, v2_bound :: Float, w_level :: Float,  wall_flag :: [Int], texture :: [Int], obj :: Maybe Obj_place} deriving (Eq, Show)

data Object = Object {ident :: Int, att_offset :: Int, num_tex :: Int, tex_w :: GLsizei, tex_h :: GLsizei, behaviours :: [Int]} deriving (Show)

data Wall_place = Wall_place {rotate :: GLint, translate_u :: Float, translate_v :: Float, translate_w :: Float, wall_flag_ :: Int, texture_ :: Int, isNull :: Bool} deriving (Show)

data Obj_place = Obj_place {ident_ :: Int, u__ :: Float, v__ :: Float, w__ :: Float, rotation :: [Int], rotate_ :: Bool, phase :: Float, texture__ :: Int, num_elem :: CInt, obj_flag :: Int} deriving (Eq, Show)

data Ray_hit = U1 | U2 | V1 | V2 | Corner0 | Corner1 | Corner2 | Corner3 | U1_hit | U2_hit | V1_hit | V2_hit | Corner0_hit | Corner1_hit | Corner2_hit | Corner3_hit | Object_hit | Ramp_found deriving (Eq)

data Terrain = Flat | Positive_u | Negative_u | Positive_v | Negative_v | Open deriving (Eq, Show, Read)

data Floor_grid = Floor_grid {w_ :: Float, surface :: Terrain, local_up_ramp :: (Int, Int), local_down_ramp :: (Int, Int)} deriving (Eq, Show)

data Play_state0 = Play_state0 {pos_u :: Float, pos_v :: Float, pos_w :: Float, vel :: [Float], angle :: Int, angle_ :: Float, message_ :: [(Int, [Int])], rend_mode :: Int, view_mode :: Int, view_angle :: Int,
game_clock :: (Int, Float, Int), torch_t0 :: Int, torch_t_limit :: Int, show_fps_ :: Bool, prob_seq :: UArray Int Int} deriving (Eq, Show)

data Play_state1 = Play_state1 {health :: Int, ammo :: Int, gems :: Int, torches :: Int, keys :: [Int], region :: [Int], difficulty :: ([Char], Int, Int, Int), sig_q :: [Int], next_sig_q :: [Int],
message :: [Int], state_chg :: Int, verbose_mode :: Bool, npc_states :: Array Int NPC_state} deriving (Eq)

data NPC_state = NPC_state {npc_type :: Int, c_health :: Int, ticks_left0 :: Int, ticks_left1 :: Int, node_locations :: [Int], fg_position :: (Float, Float, Float), dir_vector :: (Float, Float), direction :: Int,
last_dir :: Int, dir_list :: [Int], node_num :: Int, end_node :: Int, head_index :: Int, reversed :: Bool, target_u' :: Int, target_v' :: Int, target_w' :: Int, speed :: Float, avoid_dist :: Int, attack_mode :: Bool,
final_appr :: Bool, fire_prob :: Int} deriving (Eq, Show)

data Save_state = Save_state {is_set :: Bool, w_grid_ :: Array (Int, Int, Int) Wall_grid, f_grid_ :: Array (Int, Int, Int) Floor_grid, obj_grid_ :: Array (Int, Int, Int) (Int, [Int]), s0_ :: Play_state0, s1_ :: Play_state1}

data Io_box = Io_box {uniform_ :: UArray Int Int32, p_bind_ :: (UArray Int Word32, Int), control_ :: IORef Int}

data EngineError = Invalid_wall_flag | Invalid_obj_flag | Invalid_GPLC_opcode | Invalid_conf_reg_field | Invalid_GPLC_op_argument | Invalid_map_element | NPC_feature_not_implemented deriving (Show)

instance Exception EngineError

-- This data type is used in the control structure that determines how the application responds to user selections in the Main Menu and In Game Menu.
data User_menu = User_menu {menu_text :: [Int], action :: IO (), inner_nodes :: [User_menu]}

cpede_frames = [0, 1, 2, 3, 4, 5, 4, 3, 2, 1, 0] :: [Int]

ps0_init = Play_state0 {pos_u = 0, pos_v = 0, pos_w = 0, vel = [0, 0, 0], angle = 0, angle_ = 0, message_ = [], rend_mode = 0, view_mode = 0, view_angle = 0, game_clock = (1, 1, 1), torch_t0 = 1, torch_t_limit = 0, show_fps_ = False, prob_seq = def_prob_seq}
ps1_init = Play_state1 {health = 100, ammo = 0, gems = 0, torches = 0, keys = [63,63,63,63,63,63], region = [19,46,41,44,27,33,31,63,28,27,51,63,4], difficulty = ("Plenty of danger please", 6, 10, 14), sig_q = [], next_sig_q = [], message = [], state_chg = 0, verbose_mode = False, npc_states = empty_npc_array}

def_w_grid = Wall_grid {u1 = False, u2 = False, v1 = False, v2 = False, u1_bound = 0, u2_bound = 0, v1_bound = 0, v2_bound = 0, w_level = 0,  wall_flag = [], texture = [], obj = Nothing}

def_w_grid_arr :: Array (Int, Int, Int) Wall_grid
def_w_grid_arr = array ((-1, 0, 0), (-3, 99, 99)) [((w, u, v), def_w_grid) | w <- [(-1)..(-3)], u <- [0..99], v <- [0..99]]

def_f_grid = Floor_grid {w_ = 0, surface = Flat, local_up_ramp = (0, 0), local_down_ramp = (0, 0)}
def_f_grid_arr = array ((0, 0, 0), (2, 9, 9)) [((w, u, v), def_f_grid) | w <- [0..2], u <- [0..9], v <- [0..9]] :: Array (Int, Int, Int) Floor_grid
def_obj_grid = (0, [])

def_obj_grid_arr :: Array (Int, Int, Int) (Int, [Int])
def_obj_grid_arr = array ((0, 0, 0), (2, 99, 99)) [((w, u, v), def_obj_grid) | w <- [0..2], u <- [0..99], v <- [0..99]] :: Array (Int, Int, Int) (Int, [Int])

def_obj_place = Obj_place {ident_ = 0, u__ = 0, v__ = 0, w__ = 0, rotation = [], rotate_ = False, phase = 0, texture__ = 0, num_elem = 0, obj_flag = 0}
w_grid_flag = Wall_grid {u1 = True, u2 = True, v1 = True, v2 = True, u1_bound = 0, u2_bound = 0, v1_bound = 0, v2_bound = 0, w_level = 0,  wall_flag = [], texture = [], obj = Just def_obj_place}
f_grid_flag = Floor_grid {w_ = 3, surface = Flat, local_up_ramp = (0, 0), local_down_ramp = (0, 0)}
obj_grid_flag = (5, []) :: (Int, [Int])

def_save_state = Save_state {is_set = False, w_grid_ = def_w_grid_arr, f_grid_ = def_f_grid_arr, obj_grid_ = def_obj_grid_arr, s0_ = ps0_init, s1_ = ps1_init}
def_wall_place = Wall_place {rotate = 0, translate_u = 0, translate_v = 0, translate_w = 0, wall_flag_ = 0, texture_ = 0, isNull = True}
def_prob_seq = array (0, 239) [(i, 0) | i <- [0..239]]

def_npc_state = NPC_state {npc_type = 0, c_health = 0, ticks_left0 = 40, ticks_left1 = 0, node_locations = [], fg_position = (0, 0, 0), dir_vector = (0, 0), direction = 0, last_dir = 0,
dir_list = [], node_num = 0, end_node = 0, head_index = 0, reversed = False, target_u' = 0, target_v' = 0, target_w' = 0, speed = 0, avoid_dist = 0, attack_mode = False, final_appr = False, fire_prob = 0}

empty_npc_array = array (0, 127) [(i, def_npc_state) | i <- [0..127]]

-- This class is used in functions that filter the result of the ray tracer to avoid multiple rendering.
class Flag a where
  theFlag :: a -> Int

instance Flag Wall_place where
  theFlag a = wall_flag_ a

instance Flag Obj_place where
  theFlag a = obj_flag a

-- Deafult values used in the perspective transformation.
frustumScale = 1 :: Float; zNear = 0.5 :: Float; zFar = 100 :: Float

-- Used in preference to nullPtr as this value is explicitly defined as zero.  The type of 0 must be changed to Word32 if compiling for systems with 32 bit pointers.
zero_ptr = (unsafeCoerce (0 :: Word64) :: Ptr a)

-- These functions process vertex and map data, which is either loaded into GPU memory or held in the (CPU memory) environment map respectively.
proc_floats :: [[Char]] -> [Float]
proc_floats [] = []
proc_floats (x:xs) = (read x :: Float) : proc_floats xs

proc_elements :: [[Char]] -> [GLushort]
proc_elements [] = []
proc_elements (x:xs) = (read x :: GLushort) : proc_elements xs

proc_ints :: [[Char]] -> [Int]
proc_ints [] = []
proc_ints (x:xs) = (read x :: Int) : proc_ints xs

proc_fname :: [[Char]] -> [Ptr CChar] -> IO [Ptr CChar]
proc_fname [] acc = return acc
proc_fname (x:xs) acc = do
  p <- newCString x
  proc_fname xs (acc ++ [p])

proc_marker :: [Int] -> [[Int]]
proc_marker [] = []
proc_marker (x0:x1:x2:x3:xs) = [x0, x1, x2, x3] : proc_marker xs

pad_vertex :: [Float] -> [Float]
pad_vertex [] = []
pad_vertex (x0:x1:x2:xs) = [x0, x1, x2, 1] ++ pad_vertex xs

-- The vector transforms generated here are used both to transform vertices in the shaders and in parts of the game logic.
transform :: Floating a => [a] -> Matrix a -> [a]
transform [] mat = []
transform (v0:v1:v2:v3:vs) mat = toList (multStd mat (fromList 4 1 [v0, v1, v2, v3])) ++ transform vs mat

translation :: Floating a => a -> a -> a -> Matrix a
translation u v w = fromList 4 4 [1, 0, 0, u, 0, 1, 0, v, 0, 0, 1, w, 0, 0, 0, 1]

rotation_u :: Int -> UArray (Int, Int) Float -> Matrix Float
rotation_u a look_up = fromList 4 4 [1, 0, 0, 0, 0, look_up ! (2, a), - look_up ! (1, a), 0, 0, look_up ! (1, a), look_up ! (2, a), 0, 0, 0, 0, 1]

rotation_v :: Int -> UArray (Int, Int) Float -> Matrix Float
rotation_v a look_up = fromList 4 4 [look_up ! (2, a), 0, - look_up ! (1, a), 0, 0, 1, 0, 0, look_up ! (1, a), 0, look_up ! (2, a), 0, 0, 0, 0, 1]

rotation_w :: Int -> UArray (Int, Int) Float -> Matrix Float
rotation_w a look_up = fromList 4 4 [look_up ! (2, a), - look_up ! (1, a), 0, 0, look_up ! (1, a), look_up ! (2, a), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]

model_to_world :: Float -> Float -> Float -> Int -> Bool -> UArray (Int, Int) Float -> Matrix Float
model_to_world u v w aw r look_up =
  if r == True then multStd (translation u v w) (rotation_w aw look_up)
  else (translation u v w)

world_to_model :: Float -> Float -> Float -> Int -> Bool -> UArray (Int, Int) Float -> Matrix Float
world_to_model u v w aw r look_up =
  if r == True then multStd (rotation_w (628 - aw) look_up) (translation (-u) (-v) (-w))
  else (translation (-u) (-v) (-w))

world_to_camera :: Float -> Float -> Float -> Int -> UArray (Int, Int) Float -> Matrix Float
world_to_camera u v w a look_up = multStd (rotation_v a look_up) (multStd (fromList 4 4 [0, -1, 0, 0, 0, 0, 1, 0, -1, 0, 0, 0, 0, 0, 0, 1]) (translation u v w))

-- The mod_angle functions are used to handle changes to values that encode angles in the engine's look up table reference format.
mod_angle :: Int -> Int -> Int
mod_angle a b =
  if a + b < 0 then a + b + 629
  else if a + b > 628 then a + b - 629
  else a + b

mod_angle' a b = b

-- This function was introduced as the transition to variable frame rate management was made.  It handles updates to a floating point representation of angle, which allows
-- for fine grain frame rate dependent changes to the player angle to be made.
mod_angle_ :: Float -> Float -> Bool -> Float
mod_angle_ a f_rate clockwise =
  let angle_step = 6 / (f_rate / 40)
  in
  if clockwise == True then
    if a - angle_step < 0 then 200 * pi + (a - angle_step)
    else a - angle_step
  else
    if a + angle_step > 200 * pi then angle_step - (200 * pi - a)
    else a + angle_step

-- These functions implement a ray tracing algorhythm, which is part of the visible surface determination (VSD) system and is used for line of sight checks by the non - player character logic.
-- The Ray_test class exists so that the ray tracer can conveniently provide differing functionality when called from the VSD system or game logic.
bound_check :: Int -> Int -> ((Int, Int, Int), (Int, Int, Int)) -> Bool
bound_check block axis ((a, b, c), (w_max, u_max, v_max)) =
  if axis == 0 && block > u_max then False
  else if axis == 1 && block > v_max then False
  else True

f_block :: Int -> Int -> Int -> Terrain -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> Int
f_block w u v f_target0 f_grid obj_grid =
  let f_target1 = surface (f_grid ! (w - 1, div u 2, div v 2))
  in
  if f_target0 == Open && f_target1 == Positive_u && fst (obj_grid ! (w - 1, u, v)) == 0 then -1
  else if f_target0 == Open && f_target1 == Negative_u && fst (obj_grid ! (w - 1, u, v)) == 0 then -2
  else if f_target0 == Open && f_target1 == Positive_v && fst (obj_grid ! (w - 1, u, v)) == 0 then -3
  else if f_target0 == Open && f_target1 == Negative_v && fst (obj_grid ! (w - 1, u, v)) == 0 then -4
  else if f_target0 == Positive_u && fst (obj_grid ! (w, u, v)) == 0 then -5
  else if f_target0 == Negative_u && fst (obj_grid ! (w, u, v)) == 0 then -6
  else if f_target0 == Positive_v && fst (obj_grid ! (w, u, v)) == 0 then -7
  else if f_target0 == Negative_v && fst (obj_grid ! (w, u, v)) == 0 then -8
  else 0

class Ray_test a where
  intersect :: Ray_hit -> Array (Int, Int, Int) a -> Array (Int, Int, Int) Floor_grid -> Int -> Int -> Int -> Int -> Int -> (Ray_hit, Int)

  scan_cube :: Array (Int, Int, Int) a -> Int -> Int -> Int -> [Obj_place]

instance Ray_test (Int, [Int]) where
  intersect U1 obj_grid f_grid w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (u_block - 1) 2, div v_block 2))
        f_block_ = f_block w_block (u_block - 1) v_block f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if fst (obj_grid ! (w_block, u_block - 1, v_block)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block - 1) 2, div v_block 2)) /= Flat then (Object_hit, 0)
      else (U1, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if fst (obj_grid ! (w_block, u_block - 1, v_block)) > 0 then (Object_hit, 0)
      else (U1, 0)
  intersect U2 obj_grid f_grid w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (u_block + 1) 2, div v_block 2))
        f_block_ = f_block w_block (u_block + 1) v_block f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if fst (obj_grid ! (w_block, u_block + 1, v_block)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block + 1) 2, div v_block 2)) /= Flat then (Object_hit, 0)
      else (U2, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if fst (obj_grid ! (w_block, u_block + 1, v_block)) > 0 then (Object_hit, 0)
      else (U2, 0)
  intersect V1 obj_grid f_grid w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div u_block 2, div (v_block - 1) 2))
        f_block_ = f_block w_block u_block (v_block - 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if fst (obj_grid ! (w_block, u_block, v_block - 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div u_block 2, div (v_block - 1) 2)) /= Flat then (Object_hit, 0)
      else (V1, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if fst (obj_grid ! (w_block, u_block, v_block - 1)) > 0 then (Object_hit, 0)
      else (V1, 0)
  intersect V2 obj_grid f_grid w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div u_block 2, div (v_block + 1) 2))
        f_block_ = f_block w_block u_block (v_block + 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if fst (obj_grid ! (w_block, u_block, v_block + 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div u_block 2, div (v_block + 1) 2)) /= Flat then (Object_hit, 0)
      else (V2, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if fst (obj_grid ! (w_block, u_block, v_block + 1)) > 0 then (Object_hit, 0)
      else (V2, 0)
  intersect Corner0 obj_grid f_grid w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (u_block - 1) 2, div (v_block + 1) 2))
        f_block_ = f_block w_block (u_block - 1) (v_block + 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if fst (obj_grid ! (w_block, u_block - 1, v_block + 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block - 1) 2, div (v_block + 1) 2)) /= Flat then (Object_hit, 0)
      else (Corner0, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if fst (obj_grid ! (w_block, u_block - 1, v_block + 1)) > 0 then (Object_hit, 0)
      else (Corner0, 0)
  intersect Corner1 obj_grid f_grid w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (u_block + 1) 2, div (v_block + 1) 2))
        f_block_ = f_block w_block (u_block + 1) (v_block + 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if fst (obj_grid ! (w_block, u_block + 1, v_block + 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block + 1) 2, div (v_block + 1) 2)) /= Flat then (Object_hit, 0)
      else (Corner1, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if fst (obj_grid ! (w_block, u_block + 1, v_block + 1)) > 0 then (Object_hit, 0)
      else (Corner1, 0)
  intersect Corner2 obj_grid f_grid w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (u_block + 1) 2, div (v_block - 1) 2))
        f_block_ = f_block w_block (u_block + 1) (v_block - 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if fst (obj_grid ! (w_block, u_block + 1, v_block - 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block + 1) 2, div (v_block - 1) 2)) /= Flat then (Object_hit, 0)
      else (Corner2, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if fst (obj_grid ! (w_block, u_block + 1, v_block - 1)) > 0 then (Object_hit, 0)
      else (Corner2, 0)
  intersect Corner3 obj_grid f_grid w_block u_block v_block seek_mode c =
    let f_target0 = surface (f_grid ! (w_block, div (u_block - 1) 2, div (v_block - 1) 2))
        f_block_ = f_block w_block (u_block - 1) (v_block - 1) f_target0 f_grid obj_grid
    in
    if seek_mode < 3 then
      if fst (obj_grid ! (w_block, u_block - 1, v_block - 1)) > 0 then (Object_hit, 0)
      else if surface (f_grid ! (w_block, div (u_block - 1) 2, div (v_block - 1) 2)) /= Flat then (Object_hit, 0)
      else (Corner3, 0)
    else
      if c == 1 && f_target0 /= Flat then
        if f_block_ == 0 then (Object_hit, 0)
        else (Ramp_found, f_block_)
      else if fst (obj_grid ! (w_block, u_block - 1, v_block - 1)) > 0 then (Object_hit, 0)
      else (Corner3, 0)

  scan_cube obj_grid w u v = []

instance Ray_test Wall_grid where
  intersect U1 w_grid f_grid w_block u_block v_block seek_mode c =
    if u1 (w_grid ! (w_block, u_block, v_block)) == True then (U1_hit, 0)
    else (U1, 0)
  intersect U2 w_grid f_grid w_block u_block v_block seek_mode c =
    if u2 (w_grid ! (w_block, u_block, v_block)) == True then (U2_hit, 0)
    else (U2, 0)
  intersect V1 w_grid f_grid w_block u_block v_block seek_mode c =
    if v1 (w_grid ! (w_block, u_block, v_block)) == True then (V1_hit, 0)
    else (V1, 0)
  intersect V2 w_grid f_grid w_block u_block v_block seek_mode c =
    if v2 (w_grid ! (w_block, u_block, v_block)) == True then (V2_hit, 0)
    else (V2, 0)
  intersect Corner0 w_grid f_grid w_block u_block v_block seek_mode c =
    if v2 (w_grid ! (w_block, u_block, v_block)) == True || u1 (w_grid ! (w_block, u_block, v_block)) == True then (Corner0_hit, 0)
    else (Corner0, 0)
  intersect Corner1 w_grid f_grid w_block u_block v_block seek_mode c =
    if u2 (w_grid ! (w_block, u_block, v_block)) == True || v2 (w_grid ! (w_block, u_block, v_block)) == True then (Corner1_hit, 0)
    else (Corner1, 0)
  intersect Corner2 w_grid f_grid w_block u_block v_block seek_mode c =
    if v1 (w_grid ! (w_block, u_block, v_block)) == True || u2 (w_grid ! (w_block, u_block, v_block)) == True then (Corner2_hit, 0)
    else (Corner2, 0)
  intersect Corner3 w_grid f_grid w_block u_block v_block seek_mode c =
    if u1 (w_grid ! (w_block, u_block, v_block)) == True || v1 (w_grid ! (w_block, u_block, v_block)) == True then (Corner3_hit, 0)
    else (Corner3, 0)

  scan_cube w_grid w u v = maybeToList (obj (w_grid ! (w, u, v))) ++ maybeToList (obj (w_grid ! (-w - 1, u, v)))

ray_trace1 :: Float -> Float -> Float -> Float -> Float -> Float -> Int -> Bool -> Bool -> UArray (Int, Int) Float -> Ray_hit
ray_trace1 u v u1_bound u2_bound v1_bound v2_bound a True True look_up =
  if (v2_bound - v) / (u2_bound - u) == look_up ! (3, a) then Corner1
  else if (v2_bound - v) / (u2_bound - u) > look_up ! (3, a) then U2
  else V2
ray_trace1 u v u1_bound u2_bound v1_bound v2_bound a True False look_up =
  if (v - v1_bound) / (u2_bound - u) == look_up ! (3, a) then Corner2
  else if (v - v1_bound) / (u2_bound - u) > look_up ! (3, a) then U2
  else V1
ray_trace1 u v u1_bound u2_bound v1_bound v2_bound a False True look_up =
  if (v2_bound - v) / (u - u1_bound) == look_up ! (3, a) then Corner0
  else if (v2_bound - v) / (u - u1_bound) > look_up ! (3, a) then U1
  else V2
ray_trace1 u v u1_bound u2_bound v1_bound v2_bound a False False look_up =
  if (v - v1_bound) / (u - u1_bound) == look_up ! (3, a) then Corner3
  else if (v - v1_bound) / (u - u1_bound) > look_up ! (3, a) then U1
  else V1

ray_trace0 :: Ray_test a => Float -> Float -> Int -> Bool -> Bool -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) a -> UArray (Int, Int) Float -> Int -> [Obj_place] -> Int -> Int -> Int -> Int -> (Wall_place, [Obj_place], Int)
{-# SPECIALISE ray_trace0 :: Float -> Float -> Int -> Bool -> Bool -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) Wall_grid -> UArray (Int, Int) Float -> Int -> [Obj_place] -> Int -> Int -> Int -> Int -> (Wall_place, [Obj_place], Int) #-}
ray_trace0 u v a u_positive v_positive u_block v_block w_grid f_grid grid look_up w_block acc target_u target_v seek_mode c =
  let grid_i = w_grid ! (w_block, u_block, v_block)
      result = intersect (ray_trace1 u v (u1_bound grid_i) (u2_bound grid_i) (v1_bound grid_i) (v2_bound grid_i) a u_positive v_positive look_up) grid f_grid w_block u_block v_block seek_mode c
      found = scan_cube grid w_block u_block v_block ++ acc
  in
  if seek_mode == 1 && c == 2 then (def_wall_place, [], 0)
  else if seek_mode > 1 && u_block == target_u && v_block == target_v then (def_wall_place, [], 0)
  else if fst result == U1 && v_positive == True then ray_trace0 (u1_bound grid_i) (v + (look_up ! (3, a)) * (u - u1_bound grid_i)) a u_positive v_positive (u_block - 1) v_block w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)
  else if fst result == U1 && v_positive == False then ray_trace0 (u1_bound grid_i) (v - (look_up ! (3, a)) * (u - u1_bound grid_i)) a u_positive v_positive (u_block - 1) v_block w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)
  else if fst result == U2 && v_positive == True then ray_trace0 (u2_bound grid_i) (v + (look_up ! (3, a)) * ((u2_bound grid_i) - u)) a u_positive v_positive (u_block + 1) v_block w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)
  else if fst result == U2 && v_positive == False then ray_trace0 (u2_bound grid_i) (v - (look_up ! (3, a)) * ((u2_bound grid_i) - u)) a u_positive v_positive (u_block + 1) v_block w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)
  else if fst result == V1 && u_positive == True then ray_trace0 (u + (1 / look_up ! (3, a)) * (v - v1_bound grid_i)) (v1_bound grid_i) a u_positive v_positive u_block (v_block - 1) w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)
  else if fst result == V1 && u_positive == False then ray_trace0 (u - (1 / look_up ! (3, a)) * (v - v1_bound grid_i)) (v1_bound grid_i) a u_positive v_positive u_block (v_block - 1) w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)
  else if fst result == V2 && u_positive == True then ray_trace0 (u + (1 / look_up ! (3, a)) * ((v2_bound grid_i) - v)) (v2_bound grid_i) a u_positive v_positive u_block (v_block + 1) w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)
  else if fst result == V2 && u_positive == False then ray_trace0 (u - (1 / look_up ! (3, a)) * ((v2_bound grid_i) - v)) (v2_bound grid_i) a u_positive v_positive u_block (v_block + 1) w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)
  else if fst result == Corner0 then ray_trace0 (u1_bound grid_i) (v2_bound grid_i) a u_positive v_positive (u_block - 1) (v_block + 1) w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)
  else if fst result == Corner1 then ray_trace0 (u2_bound grid_i) (v2_bound grid_i) a u_positive v_positive (u_block + 1) (v_block + 1) w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)
  else if fst result == Corner2 then ray_trace0 (u2_bound grid_i) (v1_bound grid_i) a u_positive v_positive (u_block + 1) (v_block - 1) w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)
  else if fst result == U1_hit then (Wall_place {rotate = 0, translate_u = u1_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i, wall_flag_ = wall_flag grid_i !! 3, texture_ = texture grid_i !! 3, isNull = False}, found, 0)
  else if fst result == U2_hit then (Wall_place {rotate = 1, translate_u = u2_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i, wall_flag_ = wall_flag grid_i !! 1, texture_ = texture grid_i !! 1, isNull = False}, found, 0)
  else if fst result == V1_hit then (Wall_place {rotate = 2, translate_u = u1_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i, wall_flag_ = wall_flag grid_i !! 2, texture_ = texture grid_i !! 2, isNull = False}, found, 0)
  else if fst result == V2_hit then (Wall_place {rotate = 3, translate_u = u1_bound grid_i, translate_v = v2_bound grid_i, translate_w = w_level grid_i, wall_flag_ = wall_flag grid_i !! 0, texture_ = texture grid_i !! 0, isNull = False}, found, 0)
  else if fst result == Corner0_hit then (Wall_place {rotate = 3, translate_u = u1_bound grid_i, translate_v = v2_bound grid_i, translate_w = w_level grid_i, wall_flag_ = wall_flag grid_i !! 0, texture_ = texture grid_i !! 0, isNull = False}, found, 0)
  else if fst result == Corner1_hit then (Wall_place {rotate = 1, translate_u = u2_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i, wall_flag_ = wall_flag grid_i !! 1, texture_ = texture grid_i !! 1, isNull = False}, found, 0)
  else if fst result == Corner2_hit then (Wall_place {rotate = 2, translate_u = u1_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i, wall_flag_ = wall_flag grid_i !! 2, texture_ = texture grid_i !! 2, isNull = False}, found, 0)
  else if fst result == Corner3_hit then (Wall_place {rotate = 0, translate_u = u1_bound grid_i, translate_v = v1_bound grid_i, translate_w = w_level grid_i, wall_flag_ = wall_flag grid_i !! 3, texture_ = texture grid_i !! 3, isNull = False}, found, 0)
  else if fst result == Object_hit then (def_wall_place, [], c)
  else if fst result == Ramp_found then (def_wall_place, [], snd result)
  else ray_trace0 (u1_bound grid_i) (v1_bound grid_i) a u_positive v_positive (u_block - 1) (v_block - 1) w_grid f_grid grid look_up w_block found target_u target_v seek_mode (c + 1)

proc_angle :: Int -> (Int, Bool, Bool)
{-# INLINE proc_angle #-}
proc_angle a =
  if a < 158 then (a, True, True)
  else if a < 315 then (157 - (a - 157), False, True)
  else if a < 472 then (a - 314, False, False)
  else (157 - (a - 471), True, False)

-- These two functions handle the tracing of rays over the range of the field of view, returning a list of the wall sections that border the visible region and a list of any objects visible within that region.
survey_view :: Int -> Int -> Int -> Float -> Float -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> UArray (Int, Int) Float -> Int -> [Wall_place] -> [Obj_place] -> ([Wall_place], [Obj_place])
survey_view a da limit u v u_block v_block w_grid f_grid look_up w_block acc0 acc1 =
  let proc_angle_ = proc_angle a
      ray = ray_trace0 u v (fst__ proc_angle_) (snd__ proc_angle_) (third_ proc_angle_) u_block v_block w_grid f_grid w_grid look_up w_block [] 0 0 0 0
  in
  if da > limit then (acc0, acc1)
  else survey_view (mod_angle a 2) (da + 2) limit u v u_block v_block w_grid f_grid look_up w_block (acc0 ++ [fst__ ray]) (acc1 ++ snd__ ray)

multi_survey :: Int -> Int -> Float -> Float -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> UArray (Int, Int) Float -> Int -> Int -> [Wall_place] -> [Obj_place] -> ([Wall_place], [Obj_place])
multi_survey a a_limit u v u_block v_block w_grid f_grid obj_grid look_up w_limit w_block acc0 acc1 =
  let survey = (survey_view a 0 a_limit u v u_block v_block w_grid f_grid look_up w_block [] [])
  in
  if w_block > w_limit then (acc0, acc1)
  else multi_survey a a_limit u v u_block v_block w_grid f_grid obj_grid look_up w_limit (w_block + 1) (acc0 ++ fst survey) (acc1 ++ snd survey)

-- This function filters the output of the ray tracer to avoid multiple rendering.  It has been implemented using direct memory access because I couldn't find a way to write this algorhythm using an array
-- form of the flag table, which didn't appear to result in the array being re - computed every frame (leading to disasterous performance).
filter_surv :: Flag a => [a] -> [a] -> Ptr Int -> Int -> IO [a]
filter_surv [] acc p_table game_t = return acc
filter_surv (x:xs) acc p_table game_t = do
  test <- peekElemOff p_table (theFlag x)
  if test == game_t then filter_surv xs acc p_table game_t
  else do
    pokeElemOff p_table (theFlag x) game_t
    filter_surv xs (acc ++ [x]) p_table game_t

-- These functions process the wall and floor grid data from the level map file before it is used to form the environment map.
load_grid1 :: [Char] -> Bool
load_grid1 i =
  if (read i :: Int) == 0 then False
  else True

load_grid0 :: [[Char]] -> [Wall_grid]
load_grid0 [] = []
load_grid0 (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:x14:x15:x16:x17:xs) =
  if (read x17 :: Int) == 0 then Wall_grid {u1 = load_grid1 x0, u2 = load_grid1 x1, v1 = load_grid1 x2, v2 = load_grid1 x3, u1_bound = read x4, u2_bound = read x5, v1_bound = read x6, v2_bound = read x7, w_level = read x8, wall_flag = proc_ints [x9, x10, x11, x12], texture = proc_ints [x13, x14, x15, x16], obj = Nothing} : load_grid0 xs
  else Wall_grid {u1 = load_grid1 x0, u2 = load_grid1 x1, v1 = load_grid1 x2, v2 = load_grid1 x3, u1_bound = read x4, u2_bound = read x5, v1_bound = read x6, v2_bound = read x7, w_level = read x8, wall_flag = proc_ints [x9, x10, x11, x12], texture = proc_ints [x13, x14, x15, x16], obj =  Just Obj_place {ident_ = read (xs !! 0), u__ = read (xs !! 1), v__ = read (xs !! 2), w__ = read (xs !! 3), rotation = proc_ints (take 3 (drop 4 xs)), rotate_ = load_grid1 (xs !! 7), phase = read (xs !! 8), texture__ = read (xs !! 9), num_elem = read (xs !! 10), obj_flag = read (xs !! 11)}} : load_grid0 (drop 12 xs)

sort_grid1 :: [[Char]] -> [[Wall_grid]]
sort_grid1 [] = []
sort_grid1 (x:xs) = load_grid0 (splitOn ", " x) : sort_grid1 xs

sort_grid0 :: [[Char]] -> [[[Wall_grid]]]
sort_grid0 [] = []
sort_grid0 (x:xs) = sort_grid1 (splitOn ":" x) : sort_grid0 xs

make_array2 :: Int -> Int -> Int
make_array2 i0 i1 = (i1 - i0)

make_array1 :: [[[a]]] -> Int -> Int -> Int -> Array (Int, Int, Int) a
make_array1 grid u_max v_max w_max = array ((0, 0, 0), (w_max, u_max, v_max)) [((w, u, v), (((grid !! w) !! u) !! v)) |w <- [0..w_max], u <- [0..u_max], v <- [0..v_max]]

make_array0 :: [[[a]]] -> Int -> Int -> Int -> Array (Int, Int, Int) a
make_array0 grid u_max v_max w_max = array ((-w_max - 1, 0, 0), (w_max, u_max, v_max)) [((w, u, v), (((grid !! (make_array2 (-w_max - 1) w)) !! u) !! v)) |w <- [-w_max - 1..w_max], u <- [0..u_max], v <- [0..v_max]]

load_floor2 :: [[Char]] -> [Floor_grid]
load_floor2 [] = []
load_floor2 (x0:x1:x2:x3:x4:x5:xs) =
  if x1 == "0" then Floor_grid {w_ = read x0, surface = Flat, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)} : load_floor2 xs
  else if x1 == "1" then Floor_grid {w_ = read x0, surface = Positive_u, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)} : load_floor2 xs
  else if x1 == "2" then Floor_grid {w_ = read x0, surface = Negative_u, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)} : load_floor2 xs
  else if x1 == "3" then Floor_grid {w_ = read x0, surface = Positive_v, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)} : load_floor2 xs
  else if x1 == "4" then Floor_grid {w_ = read x0, surface = Negative_v, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)} : load_floor2 xs
  else Floor_grid {w_ = read x0, surface = Open, local_up_ramp = (read x2, read x3), local_down_ramp = (read x4, read x5)} : load_floor2 xs

load_floor1 :: [[Char]] -> [[Floor_grid]]
load_floor1 [] = []
load_floor1 (x:xs) = load_floor2 (splitOn ", " x) : load_floor1 xs

load_floor0 :: [[Char]] -> [[[Floor_grid]]]
load_floor0 [] = []
load_floor0 (x:xs) = load_floor1 (splitOn ":" x) : load_floor0 xs

-- These two functions process object description data, which is used to set up OpenGL vertex array and texture objects.
load_object1 :: [[Char]] -> Object
load_object1 (x0:x1:x2:x3:x4:xs) = Object {ident = read x0, att_offset = read x1, num_tex = read x2, tex_w = read x3, tex_h = read x4, behaviours = proc_ints xs}

load_object0 :: [[Char]] -> [Object]
load_object0 [] = []
load_object0 (x:xs) = load_object1 (splitOn ", " x) : load_object0 xs

-- These functions are also used to genarate the environment map from a map file.
empty_obj_grid :: Int -> Int -> Int -> Array (Int, Int, Int) (Int, [Int])
empty_obj_grid u_max v_max w_max = array ((0, 0, 0), (w_max, u_max, v_max)) [((w, u, v), (0, [])) | w <- [0..w_max], u <- [0..u_max], v <- [0..v_max]]

load_obj_grid :: [[Char]] -> [((Int, Int, Int), (Int, [Int]))]
load_obj_grid [] = []
load_obj_grid (x0:x1:x2:x3:x4:xs) = ((read x0, read x1, read x2), (read x3, proc_ints (take (read x4) xs))) : load_obj_grid (drop (read x4) xs)

empty_w_grid :: Int -> Int -> Int -> Array (Int, Int, Int) Wall_grid
empty_w_grid u_max v_max w_max = array ((0, 0, 0), (w_max, u_max, v_max)) [((w, u, v), Wall_grid {u1 = False, u2 = False, v1 = False, v2 = False, u1_bound = 0, u2_bound = 0, v1_bound = 0, v2_bound = 0, w_level = 0,  wall_flag = [], texture = [], obj = Nothing}) | w <- [0..w_max], u <- [0..u_max], v <- [0..v_max]]

build_table1 :: [[Char]] -> Array (Int, Int, Int) Wall_grid -> Int -> Array (Int, Int, Int) Wall_grid
build_table1 [] w_grid c = w_grid
build_table1 (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:xs) w_grid c =
  if c > 37499 then throw Invalid_obj_flag
  else build_table1 xs (w_grid // [((read x0, read x1, read x2), Wall_grid {u1 = False, u2 = False, v1 = False, v2 = False, u1_bound = 0, u2_bound = 0, v1_bound = 0, v2_bound = 0, w_level = 0,  wall_flag = [], texture = [], obj = Just Obj_place {ident_ = read x3, u__ = read x4, v__ = read x5, w__ = read x6, rotation = proc_ints [x7, x8, x9], rotate_ = load_grid1 x10, phase = read x11, texture__ = read x12, num_elem = read x13, obj_flag = c}})]) (c + 1)

build_table0 :: [Wall_grid] -> Int -> Int -> Int -> [[[Wall_grid]]]
build_table0 w_grid u_max v_max w_max = reverse (map (splitEvery (v_max + 1)) (splitEvery ((u_max + 1) * (v_max + 1)) w_grid))

-- Used to generate the set of allowed camera positions in third person rendering mode.
view_circle :: Float -> Float -> Float -> Int -> UArray (Int, Int) Float -> (Float, Float)
view_circle a b r t look_up = (a + r * look_up ! (2, t), b + r * look_up ! (1, t))

-- These five functions are currently unused but are intended to later form part of the game state saving system.
patch_w_grid :: [[Char]] -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Wall_grid
patch_w_grid [] w_grid = w_grid
patch_w_grid (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:xs) w_grid =
  if x0 == "0" then w_grid // [((read x0, read x1, read x2), def_w_grid)]
  else patch_w_grid xs (w_grid // [((read x1, read x2, read x3), Wall_grid {u1 = False, u2 = False, v1 = False, v2 = False, u1_bound = 0, u2_bound = 0, v1_bound = 0, v2_bound = 0, w_level = 0,  wall_flag = [], texture = [], obj = Just Obj_place {ident_ = read x4, u__ = read x5, v__ = read x6, w__ = read x7, rotation = [0, 0, 0], rotate_ = False, phase = 0, texture__ = read x8, num_elem = read x9, obj_flag = read x10}})])

patch_f_grid :: [[Char]] -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) Floor_grid
patch_f_grid [] f_grid = f_grid
patch_f_grid (x0:x1:x2:x3:x4:xs) f_grid = patch_f_grid xs (f_grid // [((read x0, read x1, read x2), Floor_grid {w_ = read x3, surface = read x4})])

patch_obj_grid :: [[Char]] -> Array (Int, Int, Int) (Int, [Int]) -> Array (Int, Int, Int) (Int, [Int])
patch_obj_grid [] obj_grid = obj_grid
patch_obj_grid (x0:x1:x2:x3:x4:xs) obj_grid =
  let dest = (read x0, read x1, read x2)
  in
  patch_obj_grid (drop (read x4) xs) (obj_grid // [(dest, (read x3, proc_ints (take (read x4) xs)))])

set_play_state0 :: [[Char]] -> Play_state0
set_play_state0 (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:xs) = Play_state0 {pos_u = read x0, pos_v = read x1, pos_w = read x2, vel = [read x3, read x4, read x5], angle = read x6, message_ = [], rend_mode = read x7, view_mode = read x8, view_angle = read x9, game_clock = (1, 1, 1), torch_t0 = read x11, torch_t_limit = read x12, show_fps_ = False}

set_play_state1 :: Int -> [[Char]] -> Play_state1 -> Play_state1
set_play_state1 0 (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:xs) s1 = set_play_state1 1 (drop (read x10) xs) (s1 {health = read x0, ammo = read x1, gems = read x2, torches = read x3, keys = [read x4, read x5, read x6, read x7, read x8, read x9], region = proc_ints (take (read x10) xs)})
set_play_state1 1 (x0:xs) s1 = set_play_state1 2 (drop (read x0) xs) (s1 {sig_q = proc_ints (take (read x0) xs)})
set_play_state1 2 (x0:xs) s1 = set_play_state1 3 (drop (read x0) xs) (s1 {message = proc_ints (take (read x0) xs)})
set_play_state1 3 (x0:xs) s1 = s1 {state_chg = read x0}

-- Used to query the conf_reg array, which holds startup parameters passed at the command line or from the engine's configuration file.
cfg :: Array Int [Char] -> Int -> [Char] -> [Char]
cfg conf_reg i query =
  if i > 51 then throw Invalid_conf_reg_field
  else if conf_reg ! i == query then conf_reg ! (i + 1)
  else cfg conf_reg (i + 2) query

-- Used to initialise the p_bind array, which contains references to all the OpenGL vertex array objects and texture objects used in the current map.
buffer_to_array :: Ptr Word32 -> UArray Int Word32 -> Int -> Int -> Int -> IO (UArray Int Word32)
buffer_to_array p arr i0 i1 limit = do
  if i0 > limit then return arr
  else do
    val <- peekElemOff p i0
    buffer_to_array p (arr // [(i1, val)]) (i0 + 1) (i1 + 1) limit

check_map_layer :: Eq a => Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) a -> a -> Array (Int, Int, Int) a
check_map_layer w u v u_limit v_limit grid flag =
  if w == 2 && u > u_limit then grid
  else if u > u_limit then check_map_layer (w + 1) 0 0 u_limit v_limit grid flag
  else if v > v_limit then check_map_layer w (u + 1) 0 u_limit v_limit grid flag
  else
    if grid ! (w, u, v) == flag then throw Invalid_map_element
    else check_map_layer w u (v + 1) u_limit v_limit grid flag
