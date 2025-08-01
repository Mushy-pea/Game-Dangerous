-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

{-# LANGUAGE CPP #-}
#define EXTRA_GPLC_DEBUG 1

module GameLogic where

import Prelude hiding ((!!))
import IndexWrapper1
import System.IO
import System.IO.Unsafe
import System.Exit
import Graphics.GL.Core33
import Graphics.UI.GLUT hiding (Flat, texture, GLfloat, None)
import Foreign
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Maybe
import Data.List hiding ((!!))
import Data.List.Split
import Data.Fixed
import Data.IORef
import qualified Data.Sequence as SEQ
import Control.Concurrent
import Control.Exception
import qualified Data.Matrix as MAT
import Unsafe.Coerce
import Data.Coerce
import System.Clock
import BuildModel
import GameSound
import PauseMenu
#if EXTRA_GPLC_DEBUG == 1
import ExtraGPLC_Debug
#endif

-- Used to load C style arrays, which are used with certain OpenGL functions.
loadArray :: Storable a => [a] -> Ptr a -> Int -> IO ()
loadArray [] p i = return ()
loadArray (x:xs) p i = do
  pokeElemOff p i x
  loadArray xs p (i + 1)

-- These two functions are used to patch a problem that can happen within projectUpdate and cause an engine shutdown.
filterSigQ :: [Signal] -> (Int, Int, Int) -> (Int, Int, Int) -> [Signal]
filterSigQ [] (i0, i1, i2) (i3, i4, i5) = []
filterSigQ (sig:xs) (i0, i1, i2) (i3, i4, i5) =
  if target sig == (i0, i1, i2) || target sig == (i3, i4, i5) then filterSigQ xs (i0, i1, i2) (i3, i4, i5)
  else sig : filterSigQ xs (i0, i1, i2) (i3, i4, i5)

subI :: Int -> Array (Int, Int, Int) Obj_grid -> (Int, Int, Int) -> (Int, Int, Int) -> Play_state1 -> (Obj_grid, Play_state1)
subI location arr (i0, i1, i2) (i3, i4, i5) s1 =
  let bd = bounds arr
      error_string0 = "\nThe engine will be allowed to continue due to a designed exception for when the error arises from project_update."
      error_string1 = "\n\nError in array index.  location: " ++ show location ++ " index: " ++ show (i0, i1, i2) ++ " bounds: " ++ show bd ++ error_string0
  in
  if i0 < fst__ (fst bd) || i0 > fst__ (snd bd) || i1 < snd__ (fst bd) || i1 > snd__ (snd bd) || i2 < third_ (fst bd) || i2 > third_ (snd bd) then
    unsafePerformIO (putStr error_string1 >> return (Obj_grid {objType = 2, program = [], programName = []}, s1 {next_sig_q = filterSigQ (next_sig_q s1) (i0, i1, i2) (i3, i4, i5)}))
  else (arr ! (i0, i1, i2), s1)

-- Encodings of set piece on screen messages used in the tile display system.
--     <          Health:     >  <        Ammo:         >   <        Gems:          >  <          Torches:               >  <         Keys:          >  <            Region:           >
msg1 = [8,31,27,38,46,34,69,63]; msg2 = [1,39,39,41,69,63]; msg3 = [7,31,39,45,69,63]; msg4 = [20,41,44,29,34,31,45,69,63]; msg5 = [11,31,51,45,69,63]; msg6 = [18,31,33,35,41,40,69,63]
--     <                              Success!  You have discovered a new region.                                >
msg7 = [19,47,29,29,31,45,45,73,63,63,25,41,47,63,34,27,48,31,63,30,35,45,29,41,48,31,44,31,30,63,27,63,40,31,49,63,44,31,33,35,41,40,66]
--     <                GAME OVER!  Health: 0                    >  <            GAME PAUSED          >  <        Resume           >          <                   Return to main menu                  >
msg8 = [7,1,13,5,63,15,22,5,18,73,63,63,8,31,27,38,46,34,69,63,53]; msg9 = [7,1,13,5,63,16,1,21,19,5,4]; msg10 = [18,31,45,47,39,31]; msg11 = [18,31,46,47,44,40,63,46,41,63,39,27,35,40,63,39,31,40,47]
--      <   Exit   >          <                             Ouch!  The player fell.             >  <            Start game               >  <          Settings             >
msg12 = [5,50,35,46]; msg13 = [25,15,47,29,34,73,63,20,34,31,63,42,38,27,51,31,44,63,32,31,38,38,66,2,4,16]; msg14 = [19,46,27,44,46,63,33,27,39,31]; msg15 = [19,31,46,46,35,40,33,45]
--      <       MAIN MENU       >          <         Save game        >          <         Load game        >          <            New save file             >          <     < Back     >
msg16 = [13,1,9,14,63,13,5,14,21]; msg17 = [19,27,48,31,63,33,27,39,31]; msg18 = [12,41,27,30,63,33,27,39,31]; msg19 = [14,31,49,63,45,27,48,31,63,32,35,38,31]; msg20 = [74,63,2,27,29,37]
--        <     Ouch!    >         <                      Fatal fall. 0 days since last accident.                                                     >
msg25 = [5,15,47,29,34,73,2,2,17]; msg26 = [39,6,27,46,27,38,63,32,27,38,38,66,63,53,63,30,27,51,45,63,45,35,40,29,31,63,38,27,45,46,63,27,29,29,35,30,31,40,46,66]
--      <                                Player shredded by a bullet. What a blood bath!                                                             >
msg27 = [47,16,38,27,51,31,44,63,45,34,44,31,30,30,31,30,63,28,51,63,27,63,28,47,38,38,31,46,66,63,23,34,27,46,63,27,63,28,38,41,41,30,63,28,27,46,34,73]
--      <                                Player was eaten by a centipede. Tasty!                                             >
msg28 = [39,16,38,27,51,31,44,63,49,27,45,63,31,27,46,31,40,63,28,51,63,27,63,29,31,40,46,35,42,31,30,31,66,63,20,27,45,46,51,73]
--      <                    Ouch...Centipede bite!                                           >
msg29 = [2, 4, 17, 0, 15, 47, 29, 34, 66, 66, 66, 3, 31, 40, 46, 35, 42, 31, 30, 31, 63, 28, 35, 46, 31, 73]
--      <                  Other items:                    >
msg30 = [15, 46, 34, 31, 44, 63, 35, 46, 31, 39, 45, 69, 63]
-- <                               Choose which game to load                                                     >    <               Game state:                               >          <                         Game time:                       >
choose_game_text = [3, 34, 41, 41, 45, 31, 63, 49, 34, 35, 29, 34, 63, 33, 27, 39, 31, 63, 46, 41, 63, 38, 41, 27, 30]; game_state_text = [7, 27, 39, 31, 63, 45, 46, 27, 46, 31, 69, 63] :: [Int]; game_time_text = [63, 7, 27, 39, 31, 63, 46, 35, 39, 31, 69, 63] :: [Int]
load_game_menu_header = [(0, choose_game_text), (0, [])] :: [(Int, [Int])]; no_game_states_header = [(0, [14, 41, 63, 33, 27, 39, 31, 63, 45, 46, 27, 46, 31, 45, 63, 32, 41, 47, 40, 30, 66]), (0, [63]), (1, [63]), (2, [63]), (3, [63]), (4, [63]), (5, [63]), (6, [63]), (7, msg12)] :: [(Int, [Int])]
error_opening_file_text = [(0, [20, 34, 31, 44, 31, 63, 49, 27, 45, 63, 27, 40, 63, 31, 44, 44, 41, 44, 63, 41, 42, 31, 40, 35, 40, 33, 63, 46, 34, 35, 45, 63, 32, 35, 38, 31, 66, 63, 63, 19, 41, 44, 44, 51, 66]), (0, []), (1, msg12)] :: [(Int, [Int])]

mainMenuText :: [(Int, [Int])]
mainMenuText = [(0, msg16), (0, []), (1, msg14), (2, msg18), (3, msg12)]

-- The following code implements a bytecode interpreter of the Game Programmable Logic Controller (GPLC) language, used for game logic scripting.
upd' x y = x + y
upd'' x y = y
upd''' x y = x - y
upd_ x y = x
upd 0 = upd'
upd 1 = upd''
upd 2 = upd'''
upd 3 = upd_
upd_a 0 = modAngle
upd_a 1 = mod_angle'
upd_b 0 = False
upd_b 1 = True

int_to_surface 0 = Flat
int_to_surface 1 = Positive_u
int_to_surface 2 = Negative_u
int_to_surface 3 = Positive_v
int_to_surface 4 = Negative_v
int_to_surface 5 = Open

intToFloat :: Int -> Float
intToFloat x = (fromIntegral x) * 0.000001

int_to_float_v x y z = ((fromIntegral x) * 0.000001, (fromIntegral y) * 0.000001, (fromIntegral z) * 0.000001)

flToInt :: Float -> Int
flToInt x = truncate (x * 1000000)

bool_to_int True = 1
bool_to_int False = 0

head_ [] = 1
head_ ls = head ls
tail_ [] = []
tail_ ls = tail ls

head__ [] = error "Invalid Obj_grid structure detected."
head__ ls = head ls

edgeCheck :: Int -> Int -> ((Int, Int, Int), (Int, Int, Int)) -> Bool
edgeCheck block axis ((a, b, c), (w_max, u_max, v_max)) =
  if axis == 0 && block > u_max then False
  else if axis == 1 && block > v_max then False
  else True

-- These three functions perform GPLC conditional expression folding, evaluating conditional op - codes at the start of a GPLC program run
-- to yield unconditional code.
onSignal :: [Int] -> [Int] -> Int -> [Int]
onSignal [] code sig = []
onSignal (x0:x1:x2:xs) code sig =
  if x0 == sig then take x2 (drop x1 code)
  else onSignal xs code sig

if1 :: Int -> Int -> Int -> [Int] -> [Int] -> [Int] -> [Int]
if1 0 arg0 arg1 code0 code1 d_list =
  if d_list !! arg0 == d_list !! arg1 then code0
  else code1
if1 1 arg0 arg1 code0 code1 d_list =
  if d_list !! arg0 < d_list !! arg1 then code0
  else code1
if1 v arg0 arg1 code0 code1 d_list =
  if d_list !! arg0 > (d_list !! arg1) + (v - 2) then code0
  else code1

if0 :: [Int] -> [Int] -> [Int]
if0 [] d_list = []
if0 code d_list =
  let code_x = \i -> code !! (i :: Int)
  in
  if code !! (0 :: Int) == 1 then
    if0 (if1 (code_x 1) (code_x 2) (code_x 3) (take (code_x 4) (drop 6 code)) (take (code_x 5) (drop (6 + (code_x 4)) code)) d_list) d_list
  else code

-- The remaining GPLC op - codes are implemented here.  The GPLC specification document explains their functions in the context of a game logic virtual machine.
chgState :: [Int] -> (Int, Int, Int) -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> UArray Int Int -> [((Int, Int, Int), Wall_grid)] -> [Int]
            -> ([((Int, Int, Int), Wall_grid)], [Int])
chgState (2:x1:x2:x3:x4:x5:x6:x7:x8:x9:xs) (i0, i1, i2) (i3, i4, i5) w_grid update w_grid_upd d_list =
  if d_list !! x1 == 0 then
    chgState xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(0, d_list !! x2), (1, d_list !! x3)]) w_grid_upd d_list
  else if d_list !! x1 == 1 then
    chgState xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(2, d_list !! x2), (3, d_list !! x3)]) w_grid_upd d_list
  else if d_list !! x1 == 2 then
    chgState xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(4, d_list !! x2), (5, d_list !! x3)]) w_grid_upd d_list
  else if d_list !! x1 == 3 then
    chgState xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(6, d_list !! x2), (7, d_list !! x3)]) w_grid_upd d_list
  else if d_list !! x1 == 9 then
    chgState xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(8, d_list !! x2), (9, d_list !! x3)]) w_grid_upd d_list
  else if d_list !! x1 == 10 then
    chgState xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(10, d_list !! x2), (11, d_list !! x3)]) w_grid_upd d_list
  else if d_list !! x1 == 11 then
    chgState xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(12, d_list !! x2), (13, d_list !! x3)]) w_grid_upd d_list
  else throw Invalid_GPLC_op_argument
chgState code (i0, i1, i2) (i3, i4, i5) w_grid update w_grid_upd d_list =
  let source = (d_list !! i0, d_list !! i1, d_list !! i2)
      dest = (d_list !! i3, d_list !! i4, d_list !! i5)
      grid_i = fromJust (obj (w_grid ! source))
      grid_i' = (obj (w_grid ! source))
      ident_' = upd (update ! 0) (ident_ grid_i) (update ! 1)
      u__' = upd (update ! 2) (u__ grid_i) (intToFloat (update ! 3))
      v__' = upd (update ! 4) (v__ grid_i) (intToFloat (update ! 5))
      w__' = upd (update ! 6) (w__ grid_i) (intToFloat (update ! 7))
      texture__' = upd (update ! 8) (texture__ grid_i) (update ! 9)
      num_elem' = upd (update ! 10) (num_elem grid_i) (fromIntegral (update ! 11))
      obj_flag' = upd (update ! 12) (obj_flag grid_i) (update ! 13)
  in 
  if isNothing grid_i' == True then (w_grid_upd, code)
  else ((dest, (w_grid ! source) {obj = Just (grid_i {ident_ = ident_', u__ = u__', v__ = v__', w__ = w__', texture__ = texture__', num_elem = num_elem',
                                                      obj_flag = obj_flag'})}) : w_grid_upd, code)

chgGrid :: GPLC_flag -> (GPLC_int, GPLC_int, GPLC_int) -> (GPLC_int, GPLC_int, GPLC_int) -> Array (Int, Int, Int) Wall_grid -> Wall_grid
           -> [((Int, Int, Int), Wall_grid)] -> [Int] -> [((Int, Int, Int), Wall_grid)]
chgGrid mode (i0, i1, i2) (i3, i4, i5) w_grid def w_grid_upd d_list
  | not (isNothing boundsCheck0) = error (fromJust boundsCheck0)
  | not (isNothing boundsCheck1) = error (fromJust boundsCheck1)
  | d_list !! mode == 0 = (dest0, def) : w_grid_upd
  | d_list !! mode == 1 = [(dest1, w_grid ! dest0), (dest0, def)] ++ w_grid_upd
  | otherwise = (dest1, w_grid ! dest0) : w_grid_upd
  where dest0 = (d_list !! i0, d_list !! i1, d_list !! i2)
        dest1 = (d_list !! i3, d_list !! i4, d_list !! i5)
        boundsCheck0 = boundsCheck w_grid (coerce dest0 :: (GPLC_int, GPLC_int, GPLC_int)) "chg_grid(0)"
        boundsCheck1 = boundsCheck w_grid (coerce dest1 :: (GPLC_int, GPLC_int, GPLC_int)) "chg_grid(1)"

chgGrid_ :: GPLC_flag -> (GPLC_int, GPLC_int, GPLC_int) -> (GPLC_int, GPLC_int, GPLC_int) -> Array (Int, Int, Int) Obj_grid
            ->[((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
chgGrid_ mode (i0, i1, i2) (i3, i4, i5) obj_grid obj_grid_upd d_list
  | not (isNothing boundsCheck0) = error (fromJust boundsCheck0)
  | not (isNothing boundsCheck1) = error (fromJust boundsCheck1)
  | d_list !! mode == 0 = (source, (-1, [])) : obj_grid_upd
  | d_list !! mode == 1 = (source, (-2, [])) : (dest, (-2, [])) : obj_grid_upd
  | otherwise = (source, (-3, [])) : (dest, (-3, [])) : obj_grid_upd
  where source = (d_list !! i0, d_list !! i1, d_list !! i2)
        dest = (d_list !! i3, d_list !! i4, d_list !! i5)
        boundsCheck0 = boundsCheck obj_grid (coerce source :: (GPLC_int, GPLC_int, GPLC_int)) "chg_grid_(0)"
        boundsCheck1 = boundsCheck obj_grid (coerce dest :: (GPLC_int, GPLC_int, GPLC_int)) "chg_grid_(1)"

chgFloor :: GPLC_int -> GPLC_flag -> Int -> (GPLC_int, GPLC_int, GPLC_int) -> Array (Int, Int, Int) Floor_grid -> [Int]
            -> Array (Int, Int, Int) Floor_grid
chgFloor state_val abs v (i0, i1, i2) f_grid d_list
  | not (isNothing boundsCheck_) = error (fromJust boundsCheck_)
  | d_list !! state_val == 0 = 
    f_grid // [(index, (f_grid ! index) {w_ = upd (d_list !! abs) (w_ (f_grid ! index)) (intToFloat (d_list !! v))})]
  | otherwise = f_grid // [(index, (f_grid ! index) {surface = int_to_surface (d_list !! v)})]
  where index = (d_list !! i0, d_list !! i1, d_list !! i2)
        boundsCheck_ = boundsCheck f_grid (coerce index :: (GPLC_int, GPLC_int, GPLC_int)) "chg_floor"

chgValue :: GPLC_int -> GPLC_flag -> GPLC_int -> (GPLC_int, GPLC_int, GPLC_int) -> [Int] -> Array (Int, Int, Int) Obj_grid
            -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
chgValue (GPLC_int val) abs (GPLC_int v) (i0, i1, i2) d_list obj_grid obj_grid_upd
  | not (isNothing boundsCheck_) = error (fromJust boundsCheck_)
  | val == 536870910 = ((d_list !! i0, d_list !! i1, d_list !! i2), (objType target, [(0, v)])) : obj_grid_upd
  | otherwise = ((d_list !! i0, d_list !! i1, d_list !! i2),
                 (objType target, [(val, upd (d_list !! abs) ((program target) !! val) (d_list !! v))])) : obj_grid_upd
  where index = (d_list !! i0, d_list !! i1, d_list !! i2)
        target = obj_grid ! index
        boundsCheck_ = boundsCheck obj_grid (coerce index :: (GPLC_int, GPLC_int, GPLC_int)) "chg_value"

chgPs0 :: GPLC_int -> GPLC_flag -> GPLC_int -> [Int] -> Play_state0 -> Play_state0
chgPs0 state_val abs v d_list s0
  | d_list !! state_val == 1 = s0 {pos_w = intToFloat (d_list !! v)}
  | d_list !! state_val == 2 = s0 {pos_u = intToFloat (d_list !! v)}
  | d_list !! state_val == 3 = s0 {pos_v = intToFloat (d_list !! v)}
  | d_list !! state_val == 4 = s0 {rend_mode = d_list !! v}
  | d_list !! state_val == 5 = s0 {torch_t0 = d_list !! v}
  | d_list !! state_val == 6 = s0 {torch_t_limit = d_list !! v}
  | d_list !! state_val == 7 = s0 {currentMap = d_list !! v}
  | otherwise = error ("\nchg_ps0: Invalid value passed for argument state_val: " ++ show (d_list !! state_val))

chgPs1 :: GPLC_int -> GPLC_int -> GPLC_int -> [Int] -> Play_state1 -> Play_state1
chgPs1 state_val abs v d_list s1
  | d_list !! state_val == 0 =
    if playerClass s1 == [2, 31, 40, 63, 4, 27, 48, 35, 31, 45] && newHealth > 100 then
      s1 {health = 100, state_chg = 1}
    else if playerClass s1 == [19, 27, 44, 27, 34, 63, 19, 34, 35, 31, 38, 30, 45] && newHealth > 80 then
      s1 {health = 80, state_chg = 1}
    else s1 {health = upd (d_list !! abs) (health s1) (d_list !! v), state_chg = 1}
  | d_list !! state_val == 1 = s1 {ammo = upd (d_list !! abs) (ammo s1) (d_list !! v), state_chg = 2}
  | d_list !! state_val == 2 = s1 {gems = upd (d_list !! abs) (gems s1) (d_list !! v), state_chg = 3}
  | d_list !! state_val == 3 = s1 {torches = upd (d_list !! abs) (torches s1) (d_list !! v), state_chg = 4}
  | d_list !! state_val == 4 =
    s1 {keys = (take (d_list !! abs) (keys s1)) ++ [d_list !! v] ++ drop ((d_list !! abs) + 1) (keys s1), state_chg = 5}
  | d_list !! state_val == 5 = s1 {difficulty = ("Hey, not too risky!", 9, 12, 15)}
  | d_list !! state_val == 6 = s1 {difficulty = ("Plenty of danger please.", 12, 16, 20)}
  | d_list !! state_val == 7 = s1 {difficulty = ("Ultra danger.", 15, 20, 25)}
  | otherwise = s1 {difficulty = ("Health and safety nightmare!", 18, 24, 30)}
  where newHealth = upd (d_list !! abs) (health s1) (d_list !! v)

copyPs0 :: GPLC_int -> (GPLC_int, GPLC_int, GPLC_int) -> Play_state0 -> Array (Int, Int, Int) Obj_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int]
           -> [((Int, Int, Int), (Int, [(Int, Int)]))]
copyPs0 (GPLC_int offset) (i0, i1, i2) s0 obj_grid obj_grid_upd d_list
  | not (isNothing boundsCheck_) = error (fromJust boundsCheck_)
  | otherwise = ((d_list !! i0, d_list !! i1, d_list !! i2), (objType target, [v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10])) : obj_grid_upd
  where index = (d_list !! i0, d_list !! i1, d_list !! i2)
        target = obj_grid ! index
        boundsCheck_ = boundsCheck obj_grid (coerce index :: (GPLC_int, GPLC_int, GPLC_int)) "copy_ps0"
        v0 = (offset, flToInt (pos_u s0))
        v1 = (offset + 1, flToInt (pos_v s0))
        v2 = (offset + 2, flToInt (pos_w s0))
        v3 = (offset + 3, flToInt ((vel s0) !! (0 :: Int)))
        v4 = (offset + 4, flToInt ((vel s0) !! (1 :: Int)))
        v5 = (offset + 5, flToInt ((vel s0) !! (2 :: Int)))
        v6 = (offset + 6, angle s0)
        v7 = (offset + 7, rend_mode s0)
        v8 = (offset + 8, fst__ (gameClock s0))
        v9 = (offset + 9, torch_t0 s0)
        v10 = (offset + 10, torch_t_limit s0)

copyPs1 :: GPLC_int -> (GPLC_int, GPLC_int, GPLC_int) -> Play_state1 -> Array (Int, Int, Int) Obj_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int]
           -> [((Int, Int, Int), (Int, [(Int, Int)]))]
copyPs1 (GPLC_int offset) (i0, i1, i2) s1 obj_grid obj_grid_upd d_list
  | not (isNothing boundsCheck_) = error (fromJust boundsCheck_)
  | otherwise = (index, (objType target, [health_, ammo_, gems_, torches_, red_key, green_key, blue_key, yellow_key, purple_key, white_key, player_class]))
                : obj_grid_upd
  where index = (d_list !! i0, d_list !! i1, d_list !! i2)
        target = obj_grid ! index
        boundsCheck_ = boundsCheck obj_grid (coerce index :: (GPLC_int, GPLC_int, GPLC_int)) "copy_ps1"
        health_ = (offset, health s1)
        ammo_ = (offset + 1, ammo s1)
        gems_ = (offset + 2, gems s1)
        torches_ = (offset + 3, torches s1)
        red_key = (offset + 4, (keys s1) !! (0 :: Int))
        green_key = (offset + 5, (keys s1) !! (1 :: Int))
        blue_key = (offset + 6, (keys s1) !! (2 :: Int))
        yellow_key = (offset + 7, (keys s1) !! (3 :: Int))
        purple_key = (offset + 8, (keys s1) !! (4 :: Int))
        white_key = (offset + 9, (keys s1) !! (5 :: Int))
        player_class = if playerClass s1 == [2, 31, 40, 63, 4, 27, 48, 35, 31, 45] then (offset + 10, 1)
                       else (offset + 10, 2)

objType_ :: Int -> Int -> Int -> Array (Int, Int, Int) Obj_grid -> Int
objType_ w u v obj_grid =
  if u < 0 || v < 0 then 2
  else if edgeCheck u 0 (bounds obj_grid) == False then 2
  else if edgeCheck v 1 (bounds obj_grid) == False then 2
  else objType (obj_grid ! (w, u, v))

copyLstate :: GPLC_int -> (GPLC_int, GPLC_int, GPLC_int) -> (GPLC_int, GPLC_int, GPLC_int) -> Array (Int, Int, Int) Wall_grid
              -> Array (Int, Int, Int) Obj_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
copyLstate (GPLC_int offset) (i0, i1, i2) (i3, i4, i5) w_grid obj_grid obj_grid_upd d_list
  | not (isNothing boundsCheck_) = error (fromJust boundsCheck_)
  | otherwise =
    ((d_list !! i0, d_list !! i1, d_list !! i2),
     (objType target, [c0, c1, c2, c3, c4, c5, c6, c7, c8, w_conf_u1, w_conf_u2, w_conf_v1, w_conf_v2])) : obj_grid_upd
  where index = (d_list !! i0, d_list !! i1, d_list !! i2)
        target = obj_grid ! index
        boundsCheck_ = boundsCheck obj_grid (coerce index :: (GPLC_int, GPLC_int, GPLC_int)) "copy_lstate"
        w = (d_list !! i3)
        u = (d_list !! i4)
        v = (d_list !! i5)
        u' = (d_list !! i4) - 1
        u'' = (d_list !! i4) + 1
        v' = (d_list !! i5) - 1
        v'' = (d_list !! i5) + 1
        w_conf_u1 = (offset + 9, bool_to_int (u1 (w_grid ! (d_list !! i3, d_list !! i4, d_list !! i5))))
        w_conf_u2 = (offset + 10, bool_to_int (u2 (w_grid ! (d_list !! i3, d_list !! i4, d_list !! i5))))
        w_conf_v1 = (offset + 11, bool_to_int (v1 (w_grid ! (d_list !! i3, d_list !! i4, d_list !! i5))))
        w_conf_v2 = (offset + 12, bool_to_int (v2 (w_grid ! (d_list !! i3, d_list !! i4, d_list !! i5))))
        c0 = (offset, objType_ w u v obj_grid)
        c1 = (offset + 1, objType_ w u' v obj_grid)
        c2 = (offset + 2, objType_ w u'' v obj_grid)
        c3 = (offset + 3, objType_ w u v' obj_grid)
        c4 = (offset + 4, objType_ w u v'' obj_grid)
        c5 = (offset + 5, objType_ w u' v' obj_grid)
        c6 = (offset + 6, objType_ w u'' v' obj_grid)
        c7 = (offset + 7, objType_ w u' v'' obj_grid)
        c8 = (offset + 8, objType_ w u'' v'' obj_grid)

chgObjType :: GPLC_int -> (GPLC_int, GPLC_int, GPLC_int) -> [Int] -> Array (Int, Int, Int) Obj_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))]
              -> [((Int, Int, Int), (Int, [(Int, Int)]))]
chgObjType v (i0, i1, i2) d_list obj_grid obj_grid_upd
  | not (isNothing boundsCheck_) = error (fromJust boundsCheck_)
  | otherwise = ((d_list !! i0, d_list !! i1, d_list !! i2), (d_list !! v, [])) : obj_grid_upd
  where index = (d_list !! i0, d_list !! i1, d_list !! i2)
        boundsCheck_ = boundsCheck obj_grid (coerce index :: (GPLC_int, GPLC_int, GPLC_int)) "chg_obj_type"

passMsg :: GPLC_int -> [Int] -> Play_state1 -> [Int] -> ([Int], Play_state1)
passMsg len msg s1 d_list =
  (drop (d_list !! len) msg,
   s1 {message = message s1 ++ [head msg] ++ [(d_list !! len) - 1] ++ take ((d_list !! len) - 1) (tail msg)})

sendSignal :: Int -> GPLC_int -> (GPLC_int, GPLC_int, GPLC_int) -> Array (Int, Int, Int) Obj_grid -> Play_state0 -> Play_state1 -> [Int]
              -> (Array (Int, Int, Int) Obj_grid, Play_state1)
sendSignal mode (GPLC_int sig) (GPLC_int i0, GPLC_int i1, GPLC_int i2) obj_grid s0 s1 d_list
  | mode == 0 && currentMap s0 == previousMap s0 =
    if not (isNothing boundsCheck0) then error (fromJust boundsCheck0)
    else (obj_grid,
          s1 {next_sig_q = Signal {sigNum = d_list !! sig, originGameT = fst__ (gameClock s0), target = (d_list !! i0, d_list !! i1, d_list !! i2)}
              : next_sig_q s1})
  | mode == 1 =
    if not (isNothing boundsCheck1) then error (fromJust boundsCheck1)
    else (obj_grid // [(index1, obj {program = (head prog) : sig : drop 2 prog})], s1)
  | otherwise = (obj_grid, s1)
  where index0 = (d_list !! i0, d_list !! i1, d_list !! i2)
        index1 = (i0, i1, i2)
        boundsCheck0 = boundsCheck obj_grid (coerce index0 :: (GPLC_int, GPLC_int, GPLC_int)) ("send_signal(0)")
        boundsCheck1 = boundsCheck obj_grid (coerce index1 :: (GPLC_int, GPLC_int, GPLC_int)) ("send_signal(1)")
        obj = obj_grid ! index1
        prog = program obj

projectInit :: GPLC_float -> GPLC_float -> GPLC_float -> GPLC_int -> GPLC_float -> (GPLC_int, GPLC_int, GPLC_int) -> (GPLC_int, GPLC_int, GPLC_int) -> GPLC_int
               -> GPLC_int -> Array (Int, Int, Int) Obj_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> UArray (Int, Int) Float
               -> [((Int, Int, Int), (Int, [(Int, Int)]))]
projectInit (GPLC_float u) (GPLC_float v) (GPLC_float w) (GPLC_int a) (GPLC_float vel) (GPLC_int i0, GPLC_int i1, GPLC_int i2)
            (GPLC_int i3, GPLC_int i4, GPLC_int i5) (GPLC_int offset) (GPLC_int obj_flag) obj_grid obj_grid_upd d_list lookUp =
  let source = (d_list !! i0, d_list !! i1, d_list !! i2)
      dest = (d_list !! i3, d_list !! i4, d_list !! i5)
      v0 = (offset, d_list !! i3)
      v1 = (offset + 1, d_list !! i4)
      v2 = (offset + 2, d_list !! i5)
      v3 = (offset + 3, d_list !! u)
      v4 = (offset + 4, d_list !! v)
      v5 = (offset + 5, - (div (d_list !! w) 1000000) - 1)
      v6 = (offset + 6, truncate ((lookUp ! (2, d_list !! a)) * fromIntegral (d_list !! vel)))
      v7 = (offset + 7, truncate ((lookUp ! (1, d_list !! a)) * fromIntegral (d_list !! vel)))
      v8 = (offset + 8, div (d_list !! u) 1000000)
      v9 = (offset + 9, div (d_list !! v) 1000000)
      v10 = (offset + 10, d_list !! obj_flag)
      v11 = (offset + 24, (d_list !! w))
  in (source, (-3, [])) : (dest, (-3, [v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11])) : obj_grid_upd

-- This function tests if a projectile has collided with a wall, object or the player when it crosses into another voxel.
detectProjectColl :: Int -> Int -> Int -> Int -> UArray Int Bool -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Obj_grid -> Play_state0 -> Int
detectProjectColl intersection w_block u_block' v_block' wall_struct w_grid obj_grid s0 =
  let no_collision = 0
      wall_collision = 1
      object_collision = 2
      player_collision = 3
  in
  if wall_struct ! intersection == True then wall_collision
  else if objType (obj_grid ! (w_block, u_block', v_block')) > 0 && objType (obj_grid ! (w_block, u_block', v_block')) < 4 then object_collision
  else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block, u_block', v_block') then player_collision
  else no_collision

-- This function checks which voxel a projectile has crossed into and then calls detectProjectColl to test which kind of collision (if any)
-- has occured as a result.
projectUpdate1 :: Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Obj_grid -> Play_state0 -> Int
projectUpdate1 w_block u_block v_block u_block' v_block' w_grid obj_grid s0 =
  let index = (w_block, u_block, v_block)
      wall_struct = array (0, 7) [(0, u2 (w_grid ! index)), (1, u2 (w_grid ! index)), (2, v2 (w_grid ! index)), (3, v2 (w_grid ! index))
                                 , (4, u1 (w_grid ! index)), (5, u1 (w_grid ! index)), (6, v1 (w_grid ! index)), (7, v1 (w_grid ! index))]
  in
  if u_block' == u_block + 1 && v_block' == v_block then detectProjectColl 0 w_block u_block' v_block' wall_struct w_grid obj_grid s0
  else if u_block' == u_block + 1 && v_block' == v_block + 1 then detectProjectColl 1 w_block u_block' v_block' wall_struct w_grid obj_grid s0
  else if u_block' == u_block && v_block' == v_block + 1 then detectProjectColl 2 w_block u_block' v_block' wall_struct w_grid obj_grid s0
  else if u_block' == u_block - 1 && v_block' == v_block + 1 then detectProjectColl 3 w_block u_block' v_block' wall_struct w_grid obj_grid s0
  else if u_block' == u_block - 1 && v_block' == v_block then detectProjectColl 4 w_block u_block' v_block' wall_struct w_grid obj_grid s0
  else if u_block' == u_block - 1 && v_block' == v_block - 1 then detectProjectColl 5 w_block u_block' v_block' wall_struct w_grid obj_grid s0
  else if u_block' == u_block && v_block' == v_block - 1 then detectProjectColl 6 w_block u_block' v_block' wall_struct w_grid obj_grid s0
  else detectProjectColl 7 w_block u_block' v_block' wall_struct w_grid obj_grid s0

-- This function is the entry point for the implementation of the project_update op - code.  The rest of the required logic
-- is handled by the two functions above.
projectUpdate0 :: GPLC_int -> GPLC_int -> (GPLC_int, GPLC_int, GPLC_int) -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)]
                  -> Array (Int, Int, Int) Obj_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> [Int]
                  -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
projectUpdate0 (GPLC_int p_state) (GPLC_int p_state') (GPLC_int i0, GPLC_int i1, GPLC_int i2) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 d_list =
  let location = (d_list !! i0, d_list !! i1, d_list !! i2)
      target = obj_grid ! location
      u = d_list !! p_state
      v = d_list !! (p_state + 1)
      vel_u = d_list !! (p_state + 3)
      vel_v = d_list !! (p_state + 4)
      u' = u + vel_u
      v' = v + vel_v
      u_block = div u 1000000
      v_block = div v 1000000
      u_block' = div u' 1000000
      v_block' = div v' 1000000
      w_block = d_list !! (p_state + 2)
      w_block_ = (- w_block) - 1
      index = (w_block, u_block, v_block)
      index_ = (w_block_, u_block, v_block)
      index' = (w_block, u_block', v_block')
      grid_i = fromJust (obj (w_grid ! index))
      grid_i' = (obj (w_grid ! index))
      s1_ = \index -> snd (subI 8 obj_grid index (i0, i1, i2) s1)
      collision = projectUpdate1 w_block_ u_block v_block u_block' v_block' w_grid obj_grid s0
      itf = \x -> intToFloat x
  in
  if isNothing grid_i' == True then ((index, def_w_grid) : w_grid_upd, (location, (objType target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
  else if ident_ grid_i /= 128 then
    (w_grid_upd, (location, (objType target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block), (p_state' + 11, v_block)])) : obj_grid_upd, s1)
  else if u_block' == u_block && v_block' == v_block then
    ((index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + intToFloat vel_u, v__ = v__ grid_i + intToFloat vel_v})}) : w_grid_upd,
     (location, (objType target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1_ (w_block_, u_block, v_block))
  else
    if collision == 1 then ((index, def_w_grid) : w_grid_upd, (location, (objType target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if collision == 2 then
      ((index, def_w_grid) : w_grid_upd,
       (location, (objType target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block'), (p_state' + 11, v_block')])) : obj_grid_upd,
       s1_ (w_block_, u_block, v_block'))
    else if collision == 3 && d_list !! i0 /= 0 then
      if health s1 - detDamage (difficulty s1) True s0 s1 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (objType target, [(p_state' + 8, 1)])) : obj_grid_upd,
            s1 {health = health s1 - detDamage (difficulty s1) True s0 s1, state_chg = 1, message = 0 : msg25})
    else ([(index', (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + itf vel_u, v__ = v__ grid_i + itf vel_v})}), (index, def_w_grid)] ++ w_grid_upd,
          (location, (objType target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)

-- The player holding gems has the side effect of negating some of the damage caused by adverse events, with the damage scaling implemented in this function.
scaleDamage :: Int -> Float
scaleDamage gems =
  let scaling_factor = 1.145521
      damage_negation = 1 - (((2.718282 ** ((fromIntegral gems) / 50)) - 1) / scaling_factor) / 4.5
  in damage_negation

-- Called from project_update, npcMove and npc_damage.  Used to determine the damage taken by the player and non - player characters from adverse events.
detDamage :: ([Char], Int, Int, Int) -> Bool -> Play_state0 -> Play_state1 -> Int
detDamage (d, low, med, high) player_damage s0 s1 =
  let adjusted_damage = \base_damage -> if player_damage then truncate (fromIntegral base_damage * scaleDamage (gems s1))
                                                         else base_damage
  in
  if (prob_seq s0) ! (mod (fst__ (gameClock s0)) 240) < 33 then adjusted_damage low
  else if (prob_seq s0) ! (mod (fst__ (gameClock s0)) 240) > 66 then adjusted_damage high
  else adjusted_damage med

binaryDice :: GPLC_int -> GPLC_int -> (GPLC_int, GPLC_int, GPLC_int) -> GPLC_int -> Play_state0 -> Array (Int, Int, Int) Obj_grid
              -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
binaryDice (GPLC_int prob) (GPLC_int diff) (GPLC_int i0, GPLC_int i1, GPLC_int i2) (GPLC_int offset) s0 obj_grid obj_grid_upd d_list =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  if (prob_seq s0) ! (mod (fst__ (gameClock s0) + (d_list !! diff)) 240) < d_list !! prob then
    ((d_list !! i0, d_list !! i1, d_list !! i2), (objType target, [(offset, 1)])) : obj_grid_upd
  else ((d_list !! i0, d_list !! i1, d_list !! i2), (objType target, [(offset, 0)])) : obj_grid_upd

binaryDice_ :: Int -> Play_state0 -> Bool
binaryDice_ prob s0 =
  if (prob_seq s0) ! (mod (fst__ (gameClock s0)) 240) < prob then True
  else False  

-- The GPLC op - codes init_npc, npc_damage, npc_decision, npcMove and cpedeMove form the entry point for scripts to drive non - player character (NPC)
-- behaviour.  As these op - codes are responsible for more complex state changes than the others, their entry point functions call a substantial number of
-- supporting functions.  There are two NPC behavioural models, namely type 1 and type 2 (also known as centipedes).
initNpc :: GPLC_int -> GPLC_int -> Play_state1 -> [Int] -> Play_state1
initNpc (GPLC_int offset) (GPLC_int i) s1 d_list =
  let char_state = (npc_states s1) ! i
      i_npc_type = d_list !! offset
      i_c_health = d_list !! (offset + 1)
      i_node_locations = take 6 (drop (offset + 2) d_list)
      i_fg_position = int_to_float_v (d_list !! (offset + 8)) (d_list !! (offset + 9)) (d_list !! (offset + 10))
      i_dir_vector = (intToFloat (d_list !! (offset + 11)), intToFloat (d_list !! (offset + 12)))
      i_direction = d_list !! (offset + 13)
      i_last_dir = d_list !! (offset + 14)
      i_speed = intToFloat (d_list !! (offset + 15))
      i_avoid_dist = d_list !! (offset + 16)
      i_attack_mode = intToBool (d_list !! (offset + 17))
      i_fire_prob = d_list !! (offset + 18)
      i_dir_list = take 6 (drop (offset + 19) d_list)
      i_node_num = d_list !! (offset + 25)
      i_end_node = d_list !! (offset + 26)
      i_head_index = d_list !! (offset + 27)
  in 
  s1 {npc_states = (npc_states s1) // [(d_list !! i, char_state {npc_type = i_npc_type, c_health = i_c_health, node_locations = i_node_locations,
      fg_position = i_fg_position, dir_vector = i_dir_vector, direction = i_direction, lastDir = i_last_dir, speed = i_speed, avoid_dist = i_avoid_dist,
      attack_mode = i_attack_mode, fire_prob = i_fire_prob, dir_list = i_dir_list, node_num = i_node_num, end_node = i_end_node, head_index = i_head_index})]}

-- Used to determine a pseudorandom target destination for an NPC, so it can wander around when not set on attacking the player.
detRandTarget :: Play_state0 -> Int -> Int -> (Int, Int, Int)
detRandTarget s0 u_bound v_bound =
  let n = \i -> (prob_seq s0) ! (mod (fst__ (gameClock s0) + i) 240)
  in (mod (n 0) 2, mod (((n 1) + 1) * ((n 2) + 1)) u_bound, mod (((n 1) + 1) * ((n 2) + 1)) v_bound)

-- The NPC path finding is based around line of sight checks, which use the Obj_grid instance of the ray tracer.
chkLineSight :: Int -> Int -> Int -> Int -> Int -> (Float, Float) -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
                -> Array (Int, Int, Int) Obj_grid -> UArray (Int, Int) Float -> Int
chkLineSight mode a w_block u_block v_block (fg_u, fg_v) target_u target_v w_grid f_grid obj_grid lookUp =
  let a' = procAngle a
  in
  third_ (rayTrace0 fg_u fg_v (fst__ a') (snd__ a') (third_ a') u_block v_block w_grid f_grid obj_grid lookUp w_block [] target_u target_v mode 1)

-- Type 1 NPCs can move in 8 directions and type 2 in a subset of 4 of these.  This function maps these directions (encoded as 1 - 8) to centiradians,
-- which is the angle representation used elsewhere in the engine.
npcDirTable :: Bool -> Int -> Int
npcDirTable shift_flag dir =
  if shift_flag == True then truncate (78.54 * fromIntegral (dir - 1)) + 6
  else truncate (78.54 * fromIntegral (dir - 1))

-- When a type 1 NPC is ascending or descending a ramp its direction is encoded as a negative integer from -1 to -8.  This function maps these directions to the
-- other encoding described above and is used when an NPC reaches the top or bottom of a ramp.
npc_dir_remap (-1) = 5
npc_dir_remap (-2) = 1
npc_dir_remap (-3) = 7
npc_dir_remap (-4) = 3
npc_dir_remap (-5) = 1
npc_dir_remap (-6) = 5
npc_dir_remap (-7) = 3
npc_dir_remap (-8) = 7

-- Determine an alternative viable direction if an NPC is blocked from following its primary choice of direction.
anotherDir :: Bool -> [Int] -> Int -> Int -> Int -> Int -> (Float, Float) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
              -> Array (Int, Int, Int) Obj_grid -> UArray (Int, Int) Float -> Play_state0 -> Int
anotherDir shift_flag [] c w_block u_block v_block (fg_u, fg_v) w_grid f_grid obj_grid lookUp s0 = 0
anotherDir shift_flag poss_dirs c w_block u_block v_block (fg_u, fg_v) w_grid f_grid obj_grid lookUp s0 =
  let choice = poss_dirs !! (mod ((prob_seq s0) ! (mod (fst__ (gameClock s0) + c) 240)) c)
  in
  if chkLineSight 1 (npcDirTable shift_flag choice) w_block u_block v_block (fg_u, fg_v) 0 0 w_grid f_grid obj_grid lookUp == 0 then choice
  else anotherDir shift_flag (delete choice poss_dirs) (c - 1) w_block u_block v_block (fg_u, fg_v) w_grid f_grid obj_grid lookUp s0

-- This function is involved in implementing the restriction to the number of possible directions for an NPC.
quantiseAngle :: Int -> (Int, Int)
quantiseAngle a =
  if a < 79 then (6, 1)
  else if a < 157 then (85, 2)
  else if a < 236 then (163, 3)
  else if a < 314 then (239, 4)
  else if a < 393 then (320, 5)
  else if a < 471 then (399, 6)
  else if a < 550 then (477, 7)
  else (556, 8)

-- Type 2 (centipede) NPCs can't reverse so that they don't trample their own tails.
-- This function is involved in implementing that restriction.
cpede_reverse 1 = 5
cpede_reverse 3 = 7
cpede_reverse 5 = 1
cpede_reverse 7 = 3

-- This function contains the decision logic specific to the type 2 (centipede) behavioural model and is called from the more general npcDecision function.
cpedeDecision :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
                 -> Array (Int, Int, Int) Obj_grid -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> (Int, Bool)
cpedeDecision 0 choice i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp =
  let char_state = (npc_states s1) ! i
  in
  if target_u == u && target_v > v then
    if direction char_state == 7 then cpedeDecision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
    else cpedeDecision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
  else if target_u == u && target_v < v then
    if direction char_state == 3 then cpedeDecision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
    else cpedeDecision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
  else if target_v == v && target_u > u then
    if direction char_state == 5 then cpedeDecision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
    else cpedeDecision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
  else if target_v == v && target_u < u then
    if direction char_state == 1 then cpedeDecision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
    else cpedeDecision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
  else if abs (target_u - u) <= abs (target_v - v) && target_u - u > 0 then
    if direction char_state == 5 then
      if target_v - v > 0 then cpedeDecision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
      else cpedeDecision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
    else cpedeDecision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
  else if abs (target_u - u) <= abs (target_v - v) && target_u - u < 0 then
    if direction char_state == 1 then
      if target_v - v > 0 then cpedeDecision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
      else cpedeDecision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
    else cpedeDecision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
  else if abs (target_u - u) >= abs (target_v - v) && target_v - v > 0 then
    if direction char_state == 7 then
      if target_u - u > 0 then cpedeDecision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
      else cpedeDecision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
    else cpedeDecision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
  else
    if direction char_state == 3 then
      if target_u - u < 0 then cpedeDecision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
      else cpedeDecision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
    else cpedeDecision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
cpedeDecision 1 choice i target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp =
  let char_state = (npc_states s1) ! i
      u_v' = ((fromIntegral u) + 0.5, (fromIntegral v) + 0.5)
      line_sight = chkLineSight 2 (npcDirTable False choice) w u v u_v' target_u target_v w_grid f_grid obj_grid lookUp
  in
  if line_sight == 0 then (choice, True && attack_mode char_state)
  else if line_sight > avoid_dist char_state then (choice, False)
  else (anotherDir False (delete (cpede_reverse choice) (delete choice [1, 3, 5, 7])) 2 w u v u_v' w_grid f_grid obj_grid lookUp s0, False)

-- Updates the list of centipede segment directions so that the tail follows the direction the head has taken.
updDirList :: Int -> [Int] -> [Int]
updDirList dir (x0:x1:x2:x3:x4:x5:xs) = [dir, x0, x1, x2, x3, x4]

-- Used in the NPC path finding to compute an angle of approach to a target from the components of an approach vector.
vectorToAngle :: Float -> Float -> Int
vectorToAngle u_comp v_comp =
  let a = truncate ((atan (abs v_comp / abs u_comp)) * 100)
  in
  if u_comp >= 0 && v_comp >= 0 then a
  else if u_comp < 0 && v_comp >= 0 then 314 - a
  else if u_comp < 0 && v_comp < 0 then 314 + a
  else 628 - a

shiftFireballPos :: Int -> Float -> Float -> (Float, Float)
shiftFireballPos dir u v =
  if dir == 1 then (u + 1.1, v)
  else if dir == 2 then (u + 1.1, v + 1.1)
  else if dir == 3 then (u, v + 1.1)
  else if dir == 4 then (u - 1.1, v + 1.1)
  else if dir == 5 then (u - 1.1, v)
  else if dir == 6 then (u - 1.1, v - 1.1)
  else if dir == 7 then (u, v - 1.1)
  else (u + 1.1, v - 1.1)

-- This function is used by npcDecision to update the Play_state1 structure.
s1' :: Play_state1 -> [Int] -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe [(Int, Int)] -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
       -> Play_state1
s1' s1 d_list finalAppr' direction' lastDir' fireball_state' ticks_left0' ticks_left1' target_w'' target_u'' target_v'' =
  let char_state = (npc_states s1) ! (d_list !! (8 :: Int))
  in
  s1 {npc_states = (npc_states s1) // [(d_list !! (8 :: Int), char_state {finalAppr = fromMaybe (finalAppr char_state) finalAppr',
      direction = fromMaybe (direction char_state) direction', lastDir = fromMaybe (lastDir char_state) lastDir',
      fireball_state = fromMaybe (fireball_state char_state) fireball_state', ticks_left0 = fromMaybe (ticks_left0 char_state) ticks_left0',
      ticks_left1 = fromMaybe (ticks_left1 char_state) ticks_left1', target_w' = fromMaybe (target_w' char_state) target_w'',
      target_u' = fromMaybe (target_u' char_state) target_u'',
      target_v' = fromMaybe (target_v' char_state) target_v''})]}

npcDecision :: Int -> Int -> GPLC_int -> Int -> Int -> Int -> [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
               -> Array (Int, Int, Int) Obj_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float
               -> ([((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
npcDecision 0 flag (GPLC_int offset) target_w target_u target_v d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 lookUp =
  let char_state = (npc_states s1) ! (d_list !! (8 :: Int))
      rand_target = detRandTarget s0 (snd__ (snd (bounds w_grid))) (third_ (snd (bounds w_grid)))
      fg_pos = fg_position char_state
      tr0 = truncate (pos_w s0)
      tr1 = truncate (pos_u s0)
      tr2 = truncate (pos_v s0)
  in
  if npc_type char_state == 2 && ticks_left0 char_state == 0 then
    if attack_mode char_state && tr0 == w then npcDecision 3 1 (GPLC_int offset) tr0 tr1 tr2 d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0
                                                           (s1' s1 d_list (j True) n n n n n n n n) lookUp
    else if ticks_left1 char_state == 0 || (u == target_u' char_state && v == target_v' char_state) then
      npcDecision 3 1 (GPLC_int offset) w (snd__ rand_target) (third_ rand_target) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0
                  (s1' s1 d_list (j False) n n n n (j 1000) n (j (snd__ rand_target)) (j (third_ rand_target))) lookUp
    else npcDecision 3 1 (GPLC_int offset) w (target_u' char_state) (target_v' char_state) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0
                     (s1' s1 d_list (j False) n n n n (j ((ticks_left1 char_state) - 1)) n (j target_u) (j target_v)) lookUp
  else if npc_type char_state < 2 && (u /= truncate (snd__ fg_pos + fst (dir_vector char_state)) || v /= truncate (third_ fg_pos + snd (dir_vector char_state))) then
    if attack_mode char_state == True then npcDecision 1 0 (GPLC_int offset) (truncate (pos_w s0)) (truncate (pos_u s0)) (truncate (pos_v s0)) d_list (w:u:v:xs) w_grid
                                                       f_grid obj_grid obj_grid_upd s0 (s1' s1 d_list n n n n (j 0) (j 0) (j 0) (j 0) (j 0)) lookUp
    else if ticks_left1 char_state < 1 || (w == target_w' char_state && u == target_u' char_state && v == target_v' char_state) then
      npcDecision 1 0 (GPLC_int offset) (fst__ rand_target) (snd__ rand_target) (third_ rand_target) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0
                  (s1' s1 d_list n n n n (j 0) (j 1000) (j (fst__ rand_target)) (j (snd__ rand_target)) (j (third_ rand_target))) lookUp
    else npcDecision 1 0 (GPLC_int offset) (target_w' char_state) (target_u' char_state) (target_v' char_state) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0
                     (s1' s1 d_list n n n n (j 0) (j ((ticks_left1 char_state) - 1)) (j (target_w' char_state)) (j (target_u' char_state)) (j (target_v' char_state))) lookUp
  else if npc_type char_state < 2 then
    (obj_grid_upd, s1' s1 d_list n n n n (j 1) (j ((ticks_left1 char_state) - 1)) (j (target_w' char_state)) (j (target_u' char_state)) (j (target_v' char_state)))
  else (obj_grid_upd,
        s1' s1 d_list n n n n (j (ticks_left0 char_state)) (j ((ticks_left1 char_state) - 1)) (j (target_w' char_state)) (j (target_u' char_state)) (j (target_v' char_state)))
npcDecision 1 flag (GPLC_int offset) target_w target_u target_v d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 lookUp =
  let char_state = (npc_states s1) ! (d_list !! (8 :: Int))
      down_ramp = local_down_ramp (f_grid ! (w, div u 2, div v 2))
      up_ramp = local_up_ramp (f_grid ! (w, div u 2, div v 2))
  in
  if w == target_w then npcDecision (2 + flag) flag (GPLC_int offset) target_w target_u target_v d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0
                                    (s1' s1 d_list (j True) n n n n n n n n) lookUp
  else if w > target_w then npcDecision (2 + flag) flag (GPLC_int offset) w ((fst down_ramp) * 2) ((snd down_ramp) * 2) d_list (w:u:v:xs) w_grid f_grid obj_grid
                                        obj_grid_upd s0 (s1' s1 d_list (j False) n n n n n n n n) lookUp
  else npcDecision (2 + flag) flag (GPLC_int offset) w ((fst up_ramp) * 2) ((snd up_ramp) * 2) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0
                   (s1' s1 d_list (j False) n n n n n n n n) lookUp
npcDecision 2 flag (GPLC_int offset) target_w target_u target_v d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 lookUp =
  let char_state = (npc_states s1) ! (d_list !! (8 :: Int))
      fg_pos = fg_position char_state
      a = vectorToAngle (((fromIntegral target_u) + 0.5) - snd__ fg_pos) (((fromIntegral target_v) + 0.5) - third_ fg_pos)
      qa = quantiseAngle a
      line_sight0 = chkLineSight 2 (fst qa) w u v (snd__ (fg_position char_state), third_ (fg_position char_state)) target_u target_v w_grid f_grid obj_grid
                                 lookUp
      line_sight1 = chkLineSight 3 (fst qa) w u v (snd__ (fg_position char_state), third_ (fg_position char_state)) target_u target_v w_grid f_grid obj_grid
                                 lookUp
      another_dir_ = anotherDir True (delete (snd qa) [1..8]) 7 w u v (snd__ fg_pos, third_ fg_pos) w_grid f_grid obj_grid lookUp s0
      fb_pos = shiftFireballPos (snd qa) (snd__ fg_pos) (third_ fg_pos)
  in
  if finalAppr char_state then
    if line_sight0 > avoid_dist char_state || line_sight0 == 0 then
      if (prob_seq s0) ! (mod (fst__ (gameClock s0)) 240) < fire_prob char_state && attack_mode char_state && npc_type ((npc_states s1) ! 127) /= fst__ (gameClock s0) then
        (obj_grid_upd,
         s1 {npc_states = (npc_states s1) // [(d_list !! (8 :: Int), char_state {direction = snd qa, lastDir = snd qa,
             fireball_state = [(offset, 1), (offset + 1, flToInt (fst__ fg_pos)), (offset + 2, flToInt (fst fb_pos)), (offset + 3, flToInt (snd fb_pos)),
                               (offset + 4, a)]}), (127, def_npc_state {npc_type = (fst__ (gameClock s0))})]})
      else (obj_grid_upd, s1' s1 d_list n (j (snd qa)) (j (snd qa)) n n n n n n)
    else (obj_grid_upd, s1' s1 d_list n (j another_dir_) (j another_dir_) n n n n n n)
  else
    if line_sight1 < 0 then (obj_grid_upd, s1' s1 d_list n (j line_sight1) (j (direction char_state)) n n n n n n)
    else if line_sight1 == 0 then (obj_grid_upd, s1' s1 d_list n (j (snd qa)) (j (snd qa)) n n n n n n)
    else if line_sight1 > avoid_dist char_state then (obj_grid_upd, s1' s1 d_list n (j (snd qa)) (j (snd qa)) n n n n n n)
    else (obj_grid_upd, s1' s1 d_list n (j another_dir_) (j another_dir_) n n n n n n)
npcDecision 3 flag (GPLC_int offset) target_w target_u target_v d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 lookUp =
  let char_state = (npc_states s1) ! (d_list !! (8 :: Int))
      choice = cpedeDecision 0 0 (d_list !! (8 :: Int)) target_u target_v w u v w_grid f_grid obj_grid s0 s1 lookUp
      prog = obj_grid ! (w, u, v)
      fg_pos = fg_position char_state
  in
  if snd choice then
    if (prob_seq s0) ! (mod (fst__ (gameClock s0)) 240) < fire_prob char_state then
      (((w, u, v), (objType prog, [(offset, 1), (offset + 1, flToInt (fst__ fg_pos)), (offset + 2, flToInt (snd__ fg_pos)), (offset + 3, flToInt (third_ fg_pos)), (offset + 4, npcDirTable False (fst choice))])) : obj_grid_upd,
       s1' s1 d_list n (j (fst choice)) n n n n n n n)
    else (obj_grid_upd, s1' s1 d_list n (j (fst choice)) n n n n n n n)
  else (obj_grid_upd, s1' s1 d_list n (j (fst choice)) n n n n n n n)

detDirVector :: Int -> Float -> UArray (Int, Int) Float -> (Float, Float)
detDirVector dir speed lookUp =
  let dir' = npcDirTable True dir
  in
  if dir == 0 then (0, 0)
  else (speed * lookUp ! (2, dir'), speed * lookUp ! (1, dir'))

charRotation :: Int -> Int -> Int -> Int
charRotation 0 dir base_id = base_id + ((dir - 1) * 2)
charRotation 1 0 base_id = base_id
charRotation 1 1 base_id = base_id + 6
charRotation 1 3 base_id = base_id
charRotation 1 5 base_id = base_id + 2
charRotation 1 7 base_id = base_id + 4

add_vel_pos (fg_w, fg_u, fg_v) (vel_u, vel_v) = (fg_w, fg_u + vel_u, fg_v + vel_v)

rampClimb :: Int -> Int -> (Float, Float, Float) -> (Float, Float, Float)
rampClimb dir c (fg_w, fg_u, fg_v) =
  if dir == -1 then (fg_w - 0.025 * fromIntegral c, fg_u - 0.05 * (fromIntegral c), fg_v)
  else if dir == -2 then (fg_w - 0.025 * fromIntegral c, fg_u + 0.05 * (fromIntegral c), fg_v)
  else if dir == -3 then (fg_w - 0.025 * fromIntegral c, fg_u, fg_v - 0.05 * (fromIntegral c))
  else if dir == -4 then (fg_w - 0.025 * fromIntegral c, fg_u, fg_v + 0.05 * (fromIntegral c))
  else if dir == -5 then (fg_w + 0.025 * fromIntegral c, fg_u + 0.05 * (fromIntegral c), fg_v)
  else if dir == -6 then (fg_w + 0.025 * fromIntegral c, fg_u - 0.05 * (fromIntegral c), fg_v)
  else if dir == -7 then (fg_w + 0.025 * fromIntegral c, fg_u, fg_v + 0.05 * (fromIntegral c))
  else (fg_w + 0.025 * fromIntegral c, fg_u, fg_v - 0.05 * (fromIntegral c))

rampFill :: Int -> Int -> Int -> Int -> a -> Terrain -> [((Int, Int, Int), a)]
rampFill w u v dir x t =
  let fill_u = if div u 2 == div (u + 1) 2 then (u + 1, 1)
               else (u - 1, -1)
      fill_v = if div v 2 == div (v + 1) 2 then (v + 1, 1)
               else (v - 1, -1)
      end_point = if dir == -1 || dir == -6 then (w, u - 2, v)
                  else if dir == -2 || dir == -5 then (w, u + 2, v)
                  else if dir == -3 || dir == -8 then (w, u, v - 1)
                  else (w, u, v + 1)
  in [((w, u, v), x), ((w, fst fill_u, v), x), ((w, u, fst fill_v), x), ((w, fst fill_u, fst fill_v), x), (end_point, x)]

convRampFill :: Int -> Int -> Int -> Int -> Int -> Terrain -> [Int]
convRampFill w u v dw dir t =
  let r = last (rampFill (w + dw) u v dir 0 t)
  in [fst__ (fst r), snd__ (fst r), third_ (fst r)]

adjustFgPosition :: Int -> Int -> Int -> Int -> (Float, Float, Float)
adjustFgPosition w u v dir =
  if dir == -2 || dir == -5 then ((fromIntegral w) + 0.1, (fromIntegral u) + 1, (fromIntegral v) + 0.5)
  else if dir == -1 || dir == -6 then ((fromIntegral w) + 0.1, fromIntegral u, (fromIntegral v) + 0.5)
  else if dir == -4 || dir == -7 then ((fromIntegral w) + 0.1, (fromIntegral u) + 0.5, (fromIntegral v) + 1)
  else ((fromIntegral w) + 0.1, (fromIntegral u) + 0.5, fromIntegral v)

-- This function is used by npcMove and cpedeMove to update the Wall_grid structure.
modWGrid :: Array (Int, Int, Int) Wall_grid -> Int -> Int -> Int -> Maybe Int -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Int -> Wall_grid
modWGrid w_grid w u v ident_' w__' u__' v__' texture__' =
  let o_target = fromMaybe def_obj_place (obj (w_grid ! (w, u, v)))
  in
  (w_grid ! (w, u, v)) {obj = Just (o_target {ident_ = fromMaybe (ident_ o_target) ident_', w__ = fromMaybe (w__ o_target) w__',
                        u__ = fromMaybe (u__ o_target) u__', v__ = fromMaybe (v__ o_target) v__', texture__ = fromMaybe (texture__ o_target) texture__'})}

-- This function is used by npcMove and cpedeMove to update the Play_state1 structure.
s1'' :: Play_state1 -> [Int] -> Maybe (Float, Float) -> Maybe [Int] -> Maybe (Float, Float, Float) -> Maybe Int -> Maybe [(Int, Int)] -> Maybe [Int]
        -> Maybe Int -> Maybe [Signal] -> Play_state1
s1'' s1 d_list dir_vector' dir_list' fg_position' ticks_left0' fireball_state' node_locations' direction' next_sig_q' =
  let char_state = (npc_states s1) ! (d_list !! (8 :: Int))
  in
  s1 {npc_states = (npc_states s1) // [(d_list !! (8 :: Int), char_state {dir_vector = fromMaybe (dir_vector char_state) dir_vector',
      dir_list = fromMaybe (dir_list char_state) dir_list',
      fg_position = fromMaybe (fg_position char_state) fg_position', ticks_left0 = fromMaybe (ticks_left0 char_state) ticks_left0',
      fireball_state = fromMaybe (fireball_state char_state) fireball_state', node_locations = fromMaybe (node_locations char_state) node_locations',
      direction = fromMaybe (direction char_state) direction'})], next_sig_q = fromMaybe (next_sig_q s1) next_sig_q'}

npcMove :: GPLC_int -> [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Floor_grid
           -> Array (Int, Int, Int) Obj_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float
           -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
npcMove (GPLC_int offset) d_list (w:u:v:w1:u1:v1:blocks) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp =
  let char_state = (npc_states s1) ! (d_list !! (8 :: Int))
      fgp = fg_position char_state
      u' = truncate ((snd__ fgp) + fst dir_vector')
      v' = truncate ((third_ fgp) + snd dir_vector')
      u'' = \x -> if x == 0 then fst (local_up_ramp (f_grid ! (w, div u 2, div v 2))) * 2
                  else fst (local_down_ramp (f_grid ! (w, div u 2, div v 2))) * 2
      v'' = \x -> if x == 0 then snd (local_up_ramp (f_grid ! (w, div u 2, div v 2))) * 2
                  else snd (local_down_ramp (f_grid ! (w, div u 2, div v 2))) * 2
      prog = obj_grid ! (w, u, v)
      prog' = \x y -> (objType prog, take offset (program prog) ++ [x, y] ++ drop (offset + 2) (program prog))
      ramp_fill' = \dw -> last (rampFill (w + dw) u v (direction char_state) (-2, [(offset, 0)]) (surface (f_grid ! (w, div u 2, div v 2))))
      dir_vector' = detDirVector (lastDir char_state) (speed char_state) lookUp
      ramp_climb_ = rampClimb (direction char_state) (41 - ticks_left0 char_state) fgp
      conv_ramp_fill0 = convRampFill w u v 1 (direction char_state) (surface (f_grid ! (w, div u 2, div v 2)))
      conv_ramp_fill1 = convRampFill w u v 0 (direction char_state) (surface (f_grid ! (w, div u 2, div v 2)))
      w_grid'1 = ((-w - 1, u, v), modWGrid w_grid (-w - 1) u v n n (j (snd__ fgp)) (j (third_ fgp)) n)
      w_grid'2 = [((-w - 1, u', v'), modWGrid w_grid (-1) 0 2 (j (charRotation 0 (direction char_state) (d_list !! (9 :: Int)))) (j (fst__ fgp)) (j (snd__ fgp))
                                              (j (third_ fgp)) n), ((-w - 1, u, v), def_w_grid)]
      w_grid'3 = ((-w - 1, u, v), modWGrid w_grid (-w - 1) u v n (j (fst__ ramp_climb_)) (j (snd__ ramp_climb_)) (j (third_ ramp_climb_)) n)
      w_grid'4 = \dw -> ([((-w - 1, u, v), def_w_grid)]
                          ++ drop 4 (rampFill (-w - 1 + dw) u v (direction char_state) w_grid'5 (surface (f_grid ! (w, div u 2, div v 2)))))
      w_grid'5 = modWGrid w_grid (-w - 1) u v n (j (fst__ ramp_climb_)) (j (snd__ ramp_climb_)) (j (third_ ramp_climb_)) n
      w_grid'6 = modWGrid w_grid (-w - 1) u v (j (charRotation 0 (npc_dir_remap (direction char_state)) (d_list !! (9 :: Int)))) n n n n
      w_grid'7 = modWGrid w_grid (-w - 1) u v (j (charRotation 0 (npc_dir_remap (direction char_state)) (d_list !! (9 :: Int)))) n n n n
      obj_grid'1 = (take 4 (rampFill w (u'' 0) (v'' 0) (direction char_state) (2, []) (surface (f_grid ! (w, div (u'' 0) 2, div (v'' 0) 2)))))
                   ++ [((w, u, v), (-2, [])), ((w, u'' 0, v'' 0), (-2, [(offset, 1)]))]
      obj_grid'2 = (take 4 (rampFill (w - 1) (u'' 1) (v'' 1) (direction char_state) (2, []) (surface (f_grid ! (w - 1, div (u'' 1) 2, div (v'' 1) 2)))))
                   ++ [((w, u, v), (-2, [])), ((w - 1, u'' 1, v'' 1), (-2, [(offset, 1)]))]
      obj_grid'3 = take 4 (rampFill w u v (direction char_state) (0, []) (surface (f_grid ! (w, div u 2, div v 2)))) ++ [((w, u, v), (-2, [])), ramp_fill' 1]
      obj_grid'4 = take 4 (rampFill w u v (direction char_state) (0, []) (surface (f_grid ! (w, div u 2, div v 2)))) ++ [((w, u, v), (-2, [])), ramp_fill' 0]
      s1_1 = s1'' s1 d_list n n (j (adjustFgPosition w (u'' 0) (v'' 0) (direction char_state))) (j 41) n (j [w, u'' 0, v'' 0, 0, 0, 0]) n
                  (j (Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u'' 0, v'' 0)} : next_sig_q s1))
      s1_2 = s1'' s1 d_list n n (j (adjustFgPosition w (u'' 1) (v'' 1) (direction char_state))) (j 41) n (j [w - 1, u'' 1, v'' 1, 0, 0, 0]) n
                  (j (Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w - 1, u'' 1, v'' 1)} : next_sig_q s1))
      s1_3 = s1'' s1 d_list n n (j (add_vel_pos fgp (dir_vector char_state))) n n n n
                  (j (Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u, v)} : next_sig_q s1))
      s1_4 = s1'' s1 d_list n n (j ramp_climb_) (j 1) n (j (conv_ramp_fill0 ++ [0, 0, 0])) (j (npc_dir_remap (direction char_state)))
                  (j (Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (conv_ramp_fill0 !! (0 :: Int), conv_ramp_fill0 !! (1 :: Int), conv_ramp_fill0 !! (2 :: Int))} : next_sig_q s1))
      s1_5 = s1'' s1 d_list n n (j ramp_climb_) (j 1) n (j (conv_ramp_fill1 ++ [0, 0, 0])) (j (npc_dir_remap (direction char_state)))
                  (j (Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (conv_ramp_fill1 !! (0 :: Int), conv_ramp_fill1 !! (1 :: Int), conv_ramp_fill1 !! (2 :: Int))} : next_sig_q s1))
      damage = detDamage (difficulty s1) True s0 s1
  in
  if ticks_left0 char_state == 0 then
    if (w, u', v') == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then
      if attack_mode char_state == True && binaryDice_ 10 s0 == True then
        if health s1 - damage <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg8})
        else (w_grid_upd, obj_grid_upd,
              s1 {health = health s1 - damage, message = 0 : msg25 ++ message s1,
                  next_sig_q = Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u, v)} : next_sig_q s1})
      else (w_grid_upd, obj_grid_upd,
            s1 {next_sig_q = Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u, v)} : next_sig_q s1})
    else if direction char_state >= 0 then
      if isNothing (obj (w_grid ! (-w - 1, u', v'))) == True || (u, v) == (u', v') then
        (w_grid'2 ++ w_grid_upd,
         ((w, u, v), (-2, [])) : ((w, u', v'), (-2, [(offset - 10, w), (offset - 9, u'), (offset - 8, v')] ++ fireball_state char_state)) : obj_grid_upd,
         s1'' s1 d_list (j (dir_vector')) n (j (add_vel_pos fgp dir_vector')) (j 1) (j []) (j [w, u', v', 0, 0, 0]) n 
              (j (Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u', v')} : next_sig_q s1)))
      else (w_grid_upd, obj_grid_upd,
            s1 {next_sig_q = Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u, v)} : next_sig_q s1})
    else if direction char_state < -4 then ([((-w - 1, u, v), def_w_grid), ((-w - 1, u'' 0, v'' 0), w_grid'6)] ++ w_grid_upd, obj_grid'1 ++ obj_grid_upd, s1_1)
    else
      if (w - 1, u', v') == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then
        (w_grid_upd, obj_grid_upd,
         s1'' s1 d_list n n n n n n (j (lastDir char_state)) (j (Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u, v)} : next_sig_q s1)))
      else ([((-w - 1, u, v), def_w_grid), ((-w, (u'' 1), (v'' 1)), w_grid'7)] ++ w_grid_upd, obj_grid'2 ++ obj_grid_upd, s1_2)
  else if ticks_left0 char_state == 1 then
    if direction char_state >= 0 then (w_grid'1 : w_grid_upd, obj_grid_upd, s1_3)
    else if direction char_state < -4 then
      if (w + 1, conv_ramp_fill0 !! (1 :: Int), conv_ramp_fill0 !! (2 :: Int)) == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then
        (w_grid_upd, obj_grid_upd,
         s1 {next_sig_q = Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u, v)} : next_sig_q s1})
      else if objType (obj_grid ! (w + 1, conv_ramp_fill0 !! (1 :: Int), conv_ramp_fill0 !! (2 :: Int))) > 0 then
        (w_grid_upd, obj_grid_upd,
         s1 {next_sig_q = Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u, v)} : next_sig_q s1})
      else (w_grid'4 (-1) ++ w_grid_upd, obj_grid'3 ++ obj_grid_upd, s1_4)
    else
      if (w, conv_ramp_fill1 !! (1 :: Int), conv_ramp_fill1 !! (2 :: Int)) == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then
        (w_grid_upd, obj_grid_upd,
         s1 {next_sig_q = Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u, v)} : next_sig_q s1})
      else if objType (obj_grid ! (w, conv_ramp_fill1 !! (1 :: Int), conv_ramp_fill1 !! (2 :: Int))) > 0 then
        (w_grid_upd, obj_grid_upd,
         s1 {next_sig_q = Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u, v)} : next_sig_q s1})
      else (w_grid'4 0 ++ w_grid_upd, obj_grid'4 ++ obj_grid_upd, s1_5)
  else if ticks_left0 char_state > 1 then
    (w_grid'3 : w_grid_upd, obj_grid_upd, s1'' s1 d_list n n n (j (ticks_left0 char_state - 1)) n n n 
                                          (j (Signal {sigNum = 129, originGameT = fst__ (gameClock s0), target = (w, u, v)} : next_sig_q s1)))
  else throw NPC_feature_not_implemented



-- The centipede NPCs have a modular design whereby a separate GPLC script drives each node, or centipede segment.  Only the head node calls npcDecision but all
-- nodes call cpede_move.  A signal relay is formed in that signals sent by the head propagate along the tail and drive script runs and thereby movement.
-- The three functions below are to support cpedeMove with segment movement, signal propagation and animation respectively.
cpedePos :: Int -> Int -> Int -> Int -> Bool -> ((Int, Int), (Float, Float))
cpedePos u v dir t reversed =
  let fg_u_base = (fromIntegral u) + 0.5
      fg_v_base = (fromIntegral v) + 0.5
      normalise = \x y -> if reversed == False then x + y
                          else x - y
  in
  if dir == 1 then ((truncate (fg_u_base + 1), truncate fg_v_base), (normalise fg_u_base ((fromIntegral (40 - t)) * 0.025), fg_v_base))
  else if dir == 3 then ((truncate fg_u_base, truncate (fg_v_base + 1)), (fg_u_base, normalise fg_v_base ((fromIntegral (40 - t)) * 0.025)))
  else if dir == 5 then ((truncate (fg_u_base - 1), truncate fg_v_base), (normalise fg_u_base (- (fromIntegral (40 - t)) * 0.025), fg_v_base))
  else if dir == 7 then ((truncate fg_u_base, truncate (fg_v_base - 1)), (fg_u_base, normalise fg_v_base (- (fromIntegral (40 - t)) * 0.025)))
  else ((u, v), (fg_u_base, fg_v_base))

cpedeSigCheck :: [Signal] -> Int -> Int -> [Signal]
cpedeSigCheck sig x y =
  if x == 0 then sig
  else if x == y then []
  else drop 1 sig

animateCpede :: Int -> Int -> Int -> Int -> Int -> [Int] -> Int
animateCpede t n base_id model_id node_num frames =
  if node_num == 0 then (frames !! (mod (div t 4) n)) + (7 - (model_id - base_id))
  else 13 - (model_id - base_id)

-- cpedeHeadSwap (and the nine functions above it) are intended to allow centipede NPCs to swap their head and tail end nodes, as a way to escape getting stuck
-- if they crawl into a dead end.
-- This imitates the functionality of centipedes in the original ZZT.  However, as of build 9_03 this mechanic is still a work in progress.
reverse_segment [] = []
reverse_segment (x:xs) = cpede_reverse x : reverse_segment xs

reverse_node_locs char_state i =
  if reversed char_state == False && i == 0 then 0
  else if reversed char_state == False then i - 1
  else if reversed char_state == True && i == 127 then 127
  else i + 1

chs0 :: Int -> Array Int NPC_state -> Array Int NPC_state
chs0 head_i c_states =
  let u = [(i, (c_states ! i) {node_num = end_node (c_states ! i) - node_num (c_states ! i)}) | i <- [head_i..head_i + end_node (c_states ! head_i)]]
  in c_states // u

chs1 :: Int -> Array Int NPC_state -> Array Int NPC_state
chs1 head_i c_states =
  let node_locations0 = \i -> take 3 (node_locations (c_states ! i))
      node_locations1 = \i -> take 3 (node_locations (c_states ! (reverse_node_locs (c_states ! i) i)))
      u = [(i, (c_states ! i) {node_locations = node_locations0 i ++ node_locations1 i}) | i <- [head_i..head_i + end_node (c_states ! head_i)]]
  in c_states // u

chs2 :: Int -> Array Int NPC_state -> Array Int NPC_state
chs2 head_i char_state_arr = char_state_arr // [(head_i, (char_state_arr ! head_i) {dir_list = reverse_segment (dir_list (char_state_arr ! head_i))})]

chs3 :: Int -> Array Int NPC_state -> Array Int NPC_state
chs3 head_i c_states =
  let u = [(i, (c_states ! i) {reversed = not (reversed (c_states ! i))}) | i <- [head_i..head_i + end_node (c_states ! head_i)]]
  in c_states // u

chs4 :: Int -> Array Int NPC_state -> Array Int NPC_state
chs4 head_i c_states =
  let tl = \i -> updTicksLeft (ticks_left0 (c_states ! i)) (reversed (c_states ! i))
      u0 = [(i, (c_states ! i) {ticks_left0 = tl i}) | i <- [head_i + 1..head_i + end_node (c_states ! head_i)]]
      u1 = [(i, (c_states ! i) {ticks_left0 = tl i}) | i <- [head_i..head_i + end_node (c_states ! head_i) - 1]]
  in
  if reversed (c_states ! head_i) == True then c_states // u0
  else c_states // u1

chs6 False = 129
chs6 True = 130

chs7 :: Array Int NPC_state -> Int -> Int -> Int -> [Int]
chs7 char_state_arr sig i c =
  let char_state = char_state_arr ! i
  in
  if c > end_node char_state then []
  else sig : take 3 (node_locations char_state) ++ chs7 char_state_arr sig (i + 1) (c + 1)

cpedeHeadSwap :: Array Int NPC_state -> Int -> Array Int NPC_state
cpedeHeadSwap char_state_arr head_i =
  let chs0' = chs0 head_i
      chs1' = chs1 head_i
      chs2' = chs2 head_i
      chs3' = chs3 head_i
      chs4' = chs4 head_i
  in chs4' $ chs3' $ chs2' $ chs1' $ chs0' $ char_state_arr

updTicksLeft :: Int -> Bool -> Int
updTicksLeft t reversed =
  if reversed == False && t == 0 then 40
  else if reversed == True && t == 40 then 0
  else if reversed == False then t - 1
  else t + 1

-- cpedeMove :: GPLC_int -> GPLC_flag -> [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Obj_grid
--              -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1
--              -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
-- cpedeMove (GPLC_int offset) (GPLC_flag mode) d_list (w:u:v:blocks) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 =
--   let char_state = (npc_states s1) ! (d_list !! (8 :: Int))
--       h_char_state = (npc_states s1) ! (head_index char_state)
--       dir_list' = if node_num char_state == 0 then updDirList (direction char_state) (dir_list char_state)
--                   else dir_list h_char_state
--       cpede_pos_ = cpedePos u v (dir_list' !! (node_num char_state)) (ticks_left0 char_state) (reversed char_state)
--       u' = fst (fst cpede_pos_)
--       v' = snd (fst cpede_pos_)
--       o_target = fromMaybe def_obj_place (obj (w_grid ! (-w - 1, u, v)))
--       char_rotation_ = charRotation 1 (dir_list' !! (node_num char_state)) (d_list !! (9 :: Int))
--       animateCpede_ = animateCpede (fst__ (gameClock s0)) 11 (d_list !! (9 :: Int)) char_rotation_ (node_num char_state) cpede_frames
--       w_grid'1 = ((-w - 1, u, v), modWGrid w_grid (-w - 1) u v n n (j (fst (snd cpede_pos_))) (j (snd (snd cpede_pos_))) (j animateCpede_))
--       w_grid'2 = [((-w - 1, u', v'), modWGrid w_grid (- 1) 0 3 (j char_rotation_) n n n n), ((-w - 1, u, v), def_w_grid)]
--       s1_1 = s1'' s1 d_list n (j dir_list') n (j (updTicksLeft (ticks_left0 char_state) (reversed char_state))) n (j [w, u', v', w, u, v]) n
--                   (j (cpedeSigCheck ([chs6 (reversed char_state), w, u', v', chs6 (reversed char_state)] ++ drop 3 (node_locations char_state))
--                                     (node_num char_state) (end_node char_state) ++ next_sig_q s1))
--       s1_2 = s1'' s1 d_list n n n (j (updTicksLeft (ticks_left0 char_state) (reversed char_state))) n n n n
--       s1_3 = s1'' s1 d_list n n (j (0, fst (snd cpede_pos_), snd (snd cpede_pos_))) (j (updTicksLeft (ticks_left0 char_state) (reversed char_state))) n n n
--                   (j (cpedeSigCheck ([chs6 (reversed char_state), w, u, v, chs6 (reversed char_state)] ++ drop 3 (node_locations char_state))
--                                  (node_num char_state) (end_node char_state) ++ next_sig_q s1))
--       damage = detDamage (difficulty s1) s0
--       npc_states' = cpedeHeadSwap (npc_states s1) (head_index char_state)
--       cpedeHeadSwap_ = cpedeHeadSwap (npc_states s1) (head_index char_state)
--       next_sig_q_ = chs7 (npc_states s1) (chs6 (not (reversed char_state))) (head_index char_state) 0 ++ next_sig_q s1
--       d_list_upd = [(offset - 11, w), (offset - 10, u'), (offset - 9, v'), (offset - 8, w), (offset - 7, u), (offset - 6, v)
--                    , (offset - 5, 15 - (char_rotation_ - (d_list !! (9 :: Int))))
--                    , (offset - 4, 15 - (char_rotation_ - (d_list !! (9 :: Int))) + 42), (offset + 38, -w - 1)]
--   in
--   if direction char_state == 0 && ticks_left0 char_state == 0 && node_num char_state == 0 then
--     (w_grid_upd, obj_grid_upd, s1 {npc_states = cpedeHeadSwap_, next_sig_q = next_sig_q_})
--   else if reversed char_state == True && mode == 0 then (w_grid_upd, obj_grid_upd, s1)
--   else if reversed char_state == False && mode == 1 then (w_grid_upd, obj_grid_upd, s1)
--   else if ticks_left0 char_state == 0 then
--     if (w, u', v') == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) && node_num char_state == 0 then
--       if attack_mode char_state == True && binaryDice_ 10 s0 == True then
--         if health s1 - damage <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg28})
--         else (w_grid_upd, obj_grid_upd,
--               s1 {health = health s1 - damage, message = 0 : msg25 ++ message s1, next_sig_q = [chs6 (reversed char_state), w, u, v] ++ next_sig_q s1})
--       else (w_grid_upd, obj_grid_upd, s1 {next_sig_q = [chs6 (reversed char_state), w, u, v] ++ next_sig_q s1})
--     else if isNothing (obj (w_grid ! (-w - 1, u', v'))) == True then
--       (w_grid'2 ++ w_grid_upd, ((w, u, v), (-2, [])) : ((w, u', v'), (-2, d_list_upd)) : obj_grid_upd, s1_1)
--     else
--       if node_num char_state == 0 then (w_grid_upd, obj_grid_upd, s1 {next_sig_q = [chs6 (reversed char_state), w, u, v] ++ next_sig_q s1})
--       else (w_grid_upd, obj_grid_upd, s1_2)
--   else (w_grid'1 : w_grid_upd, obj_grid_upd, s1_3)

npcDamage :: GPLC_flag -> [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Obj_grid
             -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> [Int]
             -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
npcDamage (GPLC_flag mode) (w:u:v:blocks) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 d_list =
  let damage = detDamage ("d", 6, 10, 14) False s0 s1
      char_state = (npc_states s1) ! (d_list !! (8 :: Int))
      nlcs = node_locations char_state
      h_char_state = (npc_states s1) ! (head_index char_state)
      o_target = fromJust (obj (w_grid ! (-w - 1, u, v)))
      s1_1 = s1 {npc_states = chs3 (head_index char_state) (npc_states s1), message = message s1 ++ [2, 4, 14],
                 next_sig_q = Signal {sigNum = 131, originGameT = fst__ (gameClock s0), target = (nlcs !! (0 :: Int), nlcs !! (1 :: Int), nlcs !! (2 :: Int))}
                              : next_sig_q s1}
      s1_2 = s1 {npc_states = (npc_states s1) // [(head_index char_state, h_char_state {c_health = (c_health h_char_state) - damage})],
                 message = message s1 ++ [2, 4, 16]}
      s1_3 = s1 {message = message s1 ++ [2, 4, 14]}
      s1_4 = s1 {npc_states = (npc_states s1) // [(d_list !! (8 :: Int), char_state {c_health = (c_health char_state) - damage})],
                 message = message s1 ++ [2, 4, 16]}
  in
  if npc_type char_state == 2 then
    if reversed char_state == False && mode == 1 then (w_grid_upd, obj_grid_upd, s1)
    else if reversed char_state == True && mode == 0 then (w_grid_upd, obj_grid_upd, s1)
    else if c_health h_char_state - damage <= 0 then (w_grid_upd, obj_grid_upd, s1_1)
    else (w_grid_upd, obj_grid_upd, s1_2)
  else
    if c_health char_state - damage <= 0 then (((-w - 1, u, v), def_w_grid) : w_grid_upd, ((w, u, v), (-1, [])) : obj_grid_upd, s1_3)
    else (w_grid_upd, obj_grid_upd, s1_4)

placeLight :: GPLC_float -> GPLC_float -> GPLC_float -> GPLC_float -> GPLC_float -> GPLC_float -> Play_state0 -> [Int] -> Play_state0
placeLight (GPLC_float colour_r) (GPLC_float colour_g) (GPLC_float colour_b) (GPLC_float u) (GPLC_float v) (GPLC_float w) s0 d_list =
  let new_colours = [intToFloat (d_list !! colour_r), intToFloat (d_list !! colour_g), intToFloat (d_list !! colour_b), 1]
      new_positions = [intToFloat (d_list !! u), intToFloat (d_list !! v), intToFloat (d_list !! w)]
  in s0 {mobile_lights = (take 16 (new_colours ++ fst (mobile_lights s0)), take 12 (new_positions ++ snd (mobile_lights s0)))}

setEventContext :: GPLC_int -> [Int] -> EventContext
setEventContext context d_list
  | d_list !! context == 1 = NewGame
  | d_list !! context == 2 = LoadGame
  | d_list !! context == 3 = SaveGame
  | d_list !! context == 4 = ReturnMainMenu
  | d_list !! context == 5 = ExitGame
  | d_list !! context == 6 = PlayerDied
  | otherwise = error ("\nsetEventContext : invalid value passed for context : " ++ show (d_list !! context))

setPlayerClass :: GPLC_int -> Play_state1 -> [Int] -> Play_state1
setPlayerClass player_class s1 d_list
  | d_list !! player_class == 1 = s1 {playerClass = [2, 31, 40, 63, 4, 27, 48, 35, 31, 45]}
  | d_list !! player_class == 2 = s1 {playerClass = [19, 27, 44, 27, 34, 63, 19, 34, 35, 31, 38, 30, 45]}
  | otherwise = error ("\nsetPlayerClass : invalid value passed for player_class : " ++ show (d_list !! player_class))

-- This function is part of the system used to make per GPLC opcode status reports to the console when verbose_mode is on.
showGplcArgs :: [Char] -> [(Int, Int)] -> [Int] -> Int -> [Char]
showGplcArgs opcode [] d_list c = []
showGplcArgs opcode (x:xs) d_list (-1) = "\n" ++ opcode ++ " run with arguments " ++ showGplcArgs opcode (x:xs) d_list 0
showGplcArgs opcode (x:xs) d_list c =
  if fst x == 0 then show c ++ ": " ++ show (d_list !! (snd x)) ++ " " ++ showGplcArgs opcode xs d_list (c + 1)
  else show c ++ ": " ++ show (snd x) ++ " " ++ showGplcArgs opcode xs d_list (c + 1)

data GPLC_Output = GPLC_Output {event_context_ :: EventContext, w_grid_upd_ :: [((Int, Int, Int), Wall_grid)], f_grid__ :: Array (Int, Int, Int) Floor_grid,
                                obj_grid_upd_ :: [((Int, Int, Int), (Int, [(Int, Int)]))], s0__ :: Play_state0, s1__ :: Play_state1}

-- Branch on each GPLC op - code to call the corresponding function, with optional per op - code status reports for debugging.
runGplc :: [Int] -> [Int] -> EventContext -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Floor_grid
           -> Array (Int, Int, Int) Obj_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> Int
           -> IO GPLC_Output
runGplc [] d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp c =
  return GPLC_Output {event_context_ = context, w_grid_upd_ = w_grid_upd, f_grid__ = f_grid, obj_grid_upd_ = obj_grid_upd, s0__ = s0, s1__ = s1}
runGplc code d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 0 =
  let location_block = ((splitOn [536870911] code) !! (2 :: Int))
      location = (location_block !! (0 :: Int), location_block !! (1 :: Int), location_block !! (2 :: Int))
  in do
  reportState (debugGplc s1) 2 [] [] "\non_signal run.  Initial state is..." []
  reportState (debugGplc s1) 0 (program (obj_grid ! location)) ((splitOn [536870911] code) !! (2 :: Int)) [] (programName (obj_grid ! location))
  runGplc (onSignal (drop 2 ((splitOn [536870911] code) !! (0 :: Int))) ((splitOn [536870911] code) !! (1 :: Int)) (code !! (1 :: Int)))
          ((splitOn [536870911] code) !! (2 :: Int)) context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 1
runGplc code d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 1 =
  let if0' = if0 code d_list
  in do
  reportState (debugGplc s1) 2 [] [] ("\nIf expression folding run.  Branch selected: " ++ show if0') []
  runGplc (tail_ if0') d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp (head_ if0')
runGplc xs d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 2 =
  let update_arr = array (0, 13) [(0, 3), (1, 0), (2, 3), (3, 0), (4, 3), (5, 0), (6, 3), (7, 0), (8, 3), (9, 0), (10, 3), (11, 0), (12, 3), (13, 0)]
      chg_state_ = chgState (2 : xs) (0, 0, 0) (0, 0, 0) w_grid update_arr w_grid_upd d_list
  in do
  runGplc (tail_ (snd chg_state_)) d_list context w_grid (fst chg_state_) f_grid obj_grid obj_grid_upd s0 s1 lookUp (head_ (snd chg_state_))
runGplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 3 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "chgGrid" [(0, x0), (0, x1), (0, x2), (0, x3), (0, x4), (0, x5), (0, x6)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid
          (chgGrid (GPLC_flag x0) (GPLC_int x1, GPLC_int x2, GPLC_int x3) (GPLC_int x4, GPLC_int x5, GPLC_int x6) w_grid def_w_grid w_grid_upd d_list) f_grid
          obj_grid obj_grid_upd s0 s1 lookUp (head_ xs)
runGplc (x0:x1:x2:x3:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 4 =
  let sig = sendSignal 0 (GPLC_int x0) (GPLC_int x1, GPLC_int x2, GPLC_int x3) obj_grid s0 s1 d_list
  in do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "send_signal" [(0, x0), (0, x1), (0, x2), (0, x3)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid (fst sig) obj_grid_upd s0 (snd sig) lookUp (head_ xs)
runGplc (x0:x1:x2:x3:x4:x5:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 5 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "chg_value" [(1, x0), (0, x1), (0, x2), (0, x3), (0, x4), (0, x5)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid
          (chgValue (GPLC_int x0) (GPLC_flag x1) (GPLC_int x2) (GPLC_int x3, GPLC_int x4, GPLC_int x5) d_list obj_grid obj_grid_upd) s0 s1 lookUp (head_ xs)
runGplc (x0:x1:x2:x3:x4:x5:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 6 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "chg_floor" [(0, x0), (0, x1), (0, x2), (0, x3), (0, x4), (0, x5)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd (chgFloor (GPLC_int x0) (GPLC_flag x1) x2 (GPLC_int x3, GPLC_int x4, GPLC_int x5) f_grid d_list)
          obj_grid obj_grid_upd s0 s1 lookUp (head_ xs)
runGplc (x0:x1:x2:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 7 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "chg_ps1" [(0, x0), (0, x1), (0, x2)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (chgPs1 (GPLC_int x0) (GPLC_int x1) (GPLC_int x2) d_list s1) lookUp (head_ xs)
runGplc (x0:x1:x2:x3:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 8 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "chg_obj_type" [(0, x0), (0, x1), (0, x2), (0, x3)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid
          (chgObjType (GPLC_int x0) (GPLC_int x1, GPLC_int x2, GPLC_int x3) d_list obj_grid obj_grid_upd)
          s0 s1 lookUp (head_ xs)
runGplc (x0:x1:x2:x3:x4:x5:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 9 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "place_light" [(0, x0), (0, x1), (0, x2), (0, x3), (0, x4), (0, x5)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd
          (placeLight (GPLC_float x0) (GPLC_float x1) (GPLC_float x2) (GPLC_float x3) (GPLC_float x4) (GPLC_float x5) s0 d_list) s1 lookUp (head_ xs)
runGplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 10 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "chg_grid_" [(0, x0), (0, x1), (0, x2), (0, x3), (0, x4), (0, x5), (0, x6)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid
          (chgGrid_ (GPLC_flag x0) (GPLC_int x1, GPLC_int x2, GPLC_int x3) (GPLC_int x4, GPLC_int x5, GPLC_int x6) obj_grid obj_grid_upd d_list) s0 s1 lookUp (head_ xs)
runGplc (x0:x1:x2:x3:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 11 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "copy_ps1" [(1, x0), (0, x1), (0, x2), (0, x3)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid (copyPs1 (GPLC_int x0) (GPLC_int x1, GPLC_int x2, GPLC_int x3) s1 obj_grid obj_grid_upd d_list)
          s0 s1 lookUp (head_ xs)
runGplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 12 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "copy_lstate" [(1, x0), (0, x1), (0, x2), (0, x3), (0, x4), (0, x5), (0, x6)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid
          (copyLstate (GPLC_int x0) (GPLC_int x1, GPLC_int x2, GPLC_int x3) (GPLC_int x4, GPLC_int x5, GPLC_int x6) w_grid obj_grid obj_grid_upd d_list) s0 s1
          lookUp (head_ xs)
runGplc (x:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 13 =
  let pass_msg' = passMsg (GPLC_int x) xs s1 d_list
  in do
  reportState (debugGplc s1) 2 [] [] []
              ("\npass_msg run with arguments " ++ "msg_length: " ++ show (d_list !! x) ++ " message data: " ++ show (take (d_list !! x) xs))
  runGplc (tail_ (fst pass_msg')) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (snd pass_msg') lookUp (head_ (fst pass_msg'))
runGplc (x0:x1:x2:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 14 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "chg_ps0" [(0, x0), (0, x1), (0, x2)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd (chgPs0 (GPLC_int x0) (GPLC_flag x1) (GPLC_int x2) d_list s0) s1 lookUp (head_ xs)
runGplc (x0:x1:x2:x3:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 15 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "copy_ps0" [(1, x0), (0, x1), (0, x2), (0, x3)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid (copyPs0 (GPLC_int x0) (GPLC_int x1, GPLC_int x2, GPLC_int x3) s0 obj_grid obj_grid_upd d_list)
          s0 s1 lookUp (head_ xs)
runGplc (x0:x1:x2:x3:x4:x5:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 16 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "binary_dice" [(0, x0), (0, x1), (0, x2), (0, x3), (0, x4), (1, x5)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid
          (binaryDice (GPLC_int x0) (GPLC_int x1) (GPLC_int x2, GPLC_int x3, GPLC_int x4) (GPLC_int x5) s0 obj_grid obj_grid_upd d_list) s0 s1 lookUp (head_ xs)
runGplc (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 17 =
  let arg_report = [(0, x0), (0, x1), (0, x2), (0, x3), (0, x4), (0, x5), (0, x6), (0, x7), (0, x8), (0, x9), (0, x10), (1, x11), (0, x12)]
  in do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "project_init" arg_report d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid
          (projectInit (GPLC_float x0) (GPLC_float x1) (GPLC_float x2) (GPLC_int x3) (GPLC_float x4) (GPLC_int x5, GPLC_int x6, GPLC_int x7) (GPLC_int x8, GPLC_int x9, GPLC_int x10) (GPLC_int x11) (GPLC_int x12) obj_grid obj_grid_upd d_list lookUp)
          s0 s1 lookUp (head_ xs)
runGplc (x0:x1:x2:x3:x4:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 18 =
  let project_update' = projectUpdate0 (GPLC_int x0) (GPLC_int x1) (GPLC_int x2, GPLC_int x3, GPLC_int x4) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 d_list
  in do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "project_update" [(0, x0), (1, x1), (0, x2), (0, x3), (0, x4)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid (fst__ project_update') f_grid obj_grid (snd__ project_update') s0 (third_ project_update') lookUp (head_ xs)
runGplc (x0:x1:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 19 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "init_npc" [(0, x0), (0, x1)] d_list (-1)) []
  reportNpcState (debugGplc s1) s1 (d_list !! (8 :: Int))
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (initNpc (GPLC_int x0) (GPLC_int x1) s1 d_list) lookUp (head_ xs)
runGplc (x0:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 20 =
  let npc_decision_ = npcDecision 0 0 (GPLC_int x0) 0 0 0 d_list (node_locations ((npc_states s1) ! (d_list !! (8 :: Int)))) w_grid f_grid obj_grid
                                  obj_grid_upd s0 s1 lookUp
  in do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "npc_decision" [(1, x0)] d_list (-1)) []
  reportNpcState (debugGplc s1) s1 (d_list !! (8 :: Int))
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid (fst npc_decision_) s0 (snd npc_decision_) lookUp (head_ xs)
runGplc (x0:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 21 =
  let npc_move_ = npcMove (GPLC_int x0) d_list (node_locations ((npc_states s1) ! (d_list !! (8 :: Int)))) w_grid w_grid_upd f_grid obj_grid obj_grid_upd
                          s0 s1 lookUp
  in do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "npc_move" [(1, x0)] d_list (-1)) []
  reportNpcState (debugGplc s1) s1 (d_list !! (8 :: Int))
  runGplc (tail_ xs) d_list context w_grid (fst__ npc_move_) f_grid obj_grid (snd__ npc_move_) s0 (third_ npc_move_) lookUp (head_ xs)
runGplc (x0:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 22 =
  let npc_damage_ = npcDamage (GPLC_flag x0) (node_locations ((npc_states s1) ! (d_list !! (8 :: Int)))) w_grid w_grid_upd obj_grid obj_grid_upd
                              s0 s1 d_list
  in do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "npc_damage" [(1, x0)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid (fst__ npc_damage_) f_grid obj_grid (snd__ npc_damage_) s0 (third_ npc_damage_) lookUp (head_ xs)
-- runGplc (x0:x1:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 23 =
--   let cpede_move_ = cpedeMove (GPLC_int x0) (GPLC_flag x1) d_list (node_locations ((npc_states s1) ! (d_list !! (8 :: Int)))) w_grid w_grid_upd obj_grid
--                               obj_grid_upd s0 s1
--   in do
--   reportState (debugGplc s1) 2 [] [] (showGplcArgs "cpede_move" [(1, x0), (1, x1)] d_list (-1)) []
--   reportNpcState (debugGplc s1) s1 (d_list !! (8 :: Int))
--   runGplc (tail_ xs) d_list context w_grid (fst__ cpede_move_) f_grid obj_grid (snd__ cpede_move_) s0 (third_ cpede_move_) lookUp (head_ xs)
runGplc (x0:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 24 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "set_event_context" [(0, x0)] d_list (-1)) []
  runGplc (tail_ xs) d_list (setEventContext (GPLC_int x0) d_list) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp (head_ xs)
runGplc (x0:xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp 25 = do
  reportState (debugGplc s1) 2 [] [] (showGplcArgs "set_player_class" [(0, x0)] d_list (-1)) []
  runGplc (tail_ xs) d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (setPlayerClass (GPLC_int x0) s1 d_list) lookUp (head_ xs)
runGplc code d_list context w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 lookUp c = do
  putStr ("\nInvalid opcode: " ++ show c)
  putStr ("\nremaining code block: " ++ show code)
  throw Invalid_GPLC_opcode

-- These functions deal with GPLC debugging output and error reporting.
#if EXTRA_GPLC_DEBUG == 0
reportState :: Bool -> Int -> [Int] -> [Int] -> [Char] -> [Char] -> IO ()
reportState False mode prog d_list message prog_name = return ()
reportState True 0 prog d_list message prog_name = do
  putStr ("\nProgram list: " ++ show prog)
  putStr ("\nData list: " ++ show d_list)
reportState True 1 prog d_list message prog_name = putStr ("\n\nProgram list: " ++ show prog)
reportState True 2 prog d_list message prog_name = putStr message
#endif

#if EXTRA_GPLC_DEBUG == 1
reportState :: Bool -> Int -> [Int] -> [Int] -> [Char] -> [Char] -> IO ()
reportState False mode prog d_list message prog_name = return ()
reportState True 0 prog d_list message prog_name = do
  putStr ("\nProgram list: " ++ show prog)
  putStr ("\nData state : " ++ recoverGplcSymbols d_list prog_name 0)
reportState True 1 prog d_list message prog_name = putStr ("\n\nProgram list: " ++ show prog)
reportState True 2 prog d_list message prog_name = putStr message
#endif

reportNpcState :: Bool -> Play_state1 -> Int -> IO ()
reportNpcState False s1 i = return ()
reportNpcState True s1 i = putStr ("\n" ++ show ((npc_states s1) ! i))

gplcError :: [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Floor_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1
             -> SomeException
             -> IO GPLC_Output
gplcError w_grid_upd f_grid obj_grid_upd s0 s1 e = do
  putStr ("\nA GPLC program has had a runtime exception and Game :: Dangerous engine is designed to shut down in this case.  Exception thrown: " ++ show e)
  putStr "\nPlease see the readme.txt file for details of how to report this bug."
  exitSuccess
  return GPLC_Output {event_context_ = None, w_grid_upd_ = w_grid_upd, f_grid__ = f_grid, obj_grid_upd_ = obj_grid_upd, s0__ = s0, s1__ = s1}

-- These two functions are to fix a major space leak, which occured when an NPC was active but not in view.  This appears to have been caused by an accumulation
-- of pending updates to Wall_grid, delayed due to laziness.  Testing showed that nothing short of forcing the update list elements to
-- normal form was sufficient.
forceUpdate1 :: ((Int, Int, Int), Wall_grid) -> Int
forceUpdate1 ((w, u, v), voxel) =
  let c0 = \x -> if x == True then 1
                 else 0
      c1 = \x -> flToInt x
      c2 = \x -> if x == [] then 0
                 else 1
      c3 = ident_ (fromMaybe def_obj_place (obj voxel))
      c4 = c1 (u__ (fromMaybe def_obj_place (obj voxel)))
      c5 = c1 (v__ (fromMaybe def_obj_place (obj voxel)))
      c6 = c1 (w__ (fromMaybe def_obj_place (obj voxel)))
      c7 = c2 (rotation (fromMaybe def_obj_place (obj voxel)))
      c8 = c0 (rotate_ (fromMaybe def_obj_place (obj voxel)))
      c9 = c1 (phase (fromMaybe def_obj_place (obj voxel)))
      c10 = texture__ (fromMaybe def_obj_place (obj voxel))
      c11 = fromIntegral (num_elem (fromMaybe def_obj_place (obj voxel)))
      c12 = obj_flag (fromMaybe def_obj_place (obj voxel))
      c13 = w + u + v
      c14 = (c0 (u1 voxel)) + (c0 (u2 voxel)) + (c0 (v1 voxel)) + (c0 (v2 voxel))
      c15 = (c1 (u1_bound voxel)) + (c1 (u2_bound voxel)) + (c1 (v1_bound voxel)) + (c1 (v2_bound voxel))
      c16 = (c1 (w_level voxel)) + (c2 (wall_flag voxel)) + (c2 (texture voxel))
  in c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10 + c11 + c12 + c13 + c14 + c15 + c16

forceUpdate0 :: [((Int, Int, Int), Wall_grid)] -> [((Int, Int, Int), Wall_grid)] -> Int -> IO [((Int, Int, Int), Wall_grid)]
forceUpdate0 [] acc c = do
  if c == 3141593 then putStr "\nA message appears in the console.  Mysterious!"
  else return ()
  return acc
forceUpdate0 (x:xs) acc c = forceUpdate0 xs (x : acc) (c + forceUpdate1 x)

-- Due to the complexity characteristics of the array update operator ( // ) it was decided at some point to replace all such operations on Wall_grid and
-- Obj_grid in the GPLC op - code functions with accumulations to lists.  Each element of the respective list would encode for an element update that would
-- be performed by the list being passed to a single ( // ) operation at the end of each game time tick.  This proved fairly simple for Wall_grid, although
-- some updates that previously relied on sequential applications of ( // ) were made to work by making these updates atomic in the op - code
-- functions.  Solving the same problem for Obj_grid was less simple and the solution chosen involves encoding updates in a list of one type and mapping that to
-- the type taken by the ( // ) operation.  This mapping is done by the function below.
atomiseObjGridUpd :: Int -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [(Int, Int)] -> Array (Int, Int, Int) Obj_grid -> [((Int, Int, Int), Obj_grid)]
atomiseObjGridUpd m [] acc obj_grid = []
atomiseObjGridUpd m (x:xs) acc obj_grid =
  let source = (obj_grid ! (fst x))
      prog = program source
      new_prog0 = if length prog > 0 then elems ((listArray (0, (length prog) - 1) prog :: UArray Int Int) // (snd (snd (xs !! (0 :: Int)))))
                  else []
      new_prog1 = if length prog > 0 then elems ((listArray (0, (length prog) - 1) prog :: UArray Int Int) // (acc ++ snd (snd x)))
                  else []
  in
  if m == 0 then
    if fst (snd x) >= 0 then atomiseObjGridUpd 1 (x:xs) acc obj_grid
    else if fst (snd x) == -1 then (fst x, def_obj_grid) : atomiseObjGridUpd 0 xs acc obj_grid
    else if fst (snd x) == -2 then
      if fst x == fst (xs !! (0 :: Int)) then (fst x, source {program = new_prog0}) : atomiseObjGridUpd 0 (drop 1 xs) acc obj_grid
      else (fst (xs !! (0 :: Int)), source {program = new_prog0}) : (fst x, def_obj_grid) : atomiseObjGridUpd 0 (drop 1 xs) acc obj_grid
    else (fst (xs !! (0 :: Int)), source {program = new_prog0}) : atomiseObjGridUpd 0 (drop 1 xs) acc obj_grid
  else
    if xs == [] then [(fst x, source {program = new_prog1})]
    else if fst x == fst (xs !! (0 :: Int)) then atomiseObjGridUpd 1 xs (acc ++ snd (snd x)) obj_grid
    else (fst x, source {program = new_prog1}) : atomiseObjGridUpd 0 xs [] obj_grid

-- This function is used to optionally filter the GPLC virtual machine debug output to programs specified on the command line.
filterDebug :: Play_state1 -> [Char] -> Int -> Bool
filterDebug s1 program_name i
  | verbose_mode s1 == "n" = False
  | verbose_mode s1 == "y" = True
  | i > snd (bounds (debugSet s1)) = False
  | (debugSet s1) ! i == program_name = True
  | otherwise = filterDebug s1 program_name (i + 1)

-- Dynamic lights placed in the world by GPLC op - code place_light should only persist for 1 GPLC interpreter tick unless
-- they are replaced at the next tick.  This function ensures that is the case.
clearMobileLights :: Bool -> Play_state0 -> Play_state0
clearMobileLights True s0 = s0 {mobile_lights = ([], [])}
clearMobileLights False s0 = s0

-- These three functions (together with send_signal) implement the signalling system that drives GPLC program runs.  This involves signalling programs in
-- response to player object collisions and handling the signal queue, which allows programs to signal each other.  The phase_flag argument of linkGplc0
-- is used by updatePlay to limit the speed of the GPLC interpreter to 40 ticks per second, independent of the variable frame rate.  The exception to this
-- limit is if a player object collision needs to be handled, in which case an additional interpreter tick is allowed as a special case.
linkGplc0 :: Bool -> Bool -> Bool -> [Float] -> [Int] -> Game_state -> [((Int, Int, Int), Wall_grid)]
             -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> UArray (Int, Int) Float
             -> IO Game_state
linkGplc0 phase_flag init_flag queue_start (x0:x1:xs) (z0:z1:z2:zs) game_state w_grid_upd obj_grid_upd lookUp =
  let target0 = linkGplc2 x0 (z0, z1, z2)
      target1 = target (head (sig_q (s1_ game_state)))
      game_t = fst__ (gameClock (s0_ game_state))
      object = (obj_grid_ game_state) ! target1
      prog = program object
      signal = head (sig_q (s1_ game_state))
      obj_grid' = sendSignal 1 (GPLC_int 1) (GPLC_int (fst__ target0), GPLC_int (snd__ target0), GPLC_int (third_ target0)) (obj_grid_ game_state)
                             (s0_ game_state) (s1_ game_state) []
      obj_grid'' = (obj_grid_ game_state) // [(target1, Obj_grid {objType = objType object,
                                              program = (head__ prog) : sigNum signal : drop 2 prog,
                                              programName = programName object})]
      debug_enabled = \mode -> if mode == 0 then filterDebug (s1_ game_state) (programName ((obj_grid_ game_state) ! target0)) 0
                               else filterDebug (s1_ game_state) (programName object) 0
      s1' = (s1_ game_state) {sig_q = drop 1 (sig_q (s1_ game_state)), debugGplc = debug_enabled 1}
      s1'' = (s1_ game_state) {debugGplc = debug_enabled 0}
  in do
  if init_flag == True then do
    if (x1 == 1 || x1 == 3) && head (program ((obj_grid_ game_state) ! target0)) == 0 then do
      reportState (debug_enabled 0) 2 [] []
                  ("\nPlayer starts GPLC program [" ++ programName ((obj_grid_ game_state) ! target0) ++ "] at Obj_grid " ++ show target0) []
      run_gplc' <- catch (runGplc (program ((fst obj_grid') ! target0)) [] (event_context game_state) (w_grid_ game_state) w_grid_upd (f_grid_ game_state)
                                  (fst obj_grid') obj_grid_upd (s0_ game_state) s1'' lookUp 0)
                         (\e -> gplcError w_grid_upd (f_grid_ game_state) obj_grid_upd (s0_ game_state) (s1_ game_state) e)
      linkGplc0 phase_flag False True (x0:x1:xs) (z0:z1:z2:zs)
                (game_state {event_context = event_context_ run_gplc', f_grid_ = f_grid__ run_gplc', s0_ = s0__ run_gplc', s1_ = s1__ run_gplc'})
                (w_grid_upd_ run_gplc') (obj_grid_upd_ run_gplc') lookUp
    else linkGplc0 phase_flag False True (x0:x1:xs) (z0:z1:z2:zs) game_state w_grid_upd obj_grid_upd lookUp
  else if phase_flag == True then do
    if sig_q (s1_ game_state) == [] then do
      update <- forceUpdate0 w_grid_upd [] 0
      return game_state {w_grid_ = w_grid_ game_state // update,
                         obj_grid_ = obj_grid_ game_state // (atomiseObjGridUpd 0 obj_grid_upd [] (obj_grid_ game_state)),
                         s1_ = (s1_ game_state) {sig_q = next_sig_q (s1_ game_state), next_sig_q = []}}
    else do
      reportState ((debug_enabled 1) && sig_q (s1_ game_state) /= []) 2 [] []
                  ("\n\ngame_t = " ++ show game_t ++ "\n----------------\n\nsignal queue: " ++ show (sig_q (s1_ game_state)) ++ "\n") []
      if objType object == 1 || objType object == 3 then do
        reportState (debug_enabled 1) 2 [] []
                    ("\nGPLC program [" ++ programName object ++ "] run at Obj_grid " ++ show target1) []
        run_gplc' <- catch (runGplc (program (obj_grid'' ! target1)) [] (event_context game_state) (w_grid_ game_state) w_grid_upd (f_grid_ game_state)
                                    obj_grid'' obj_grid_upd (clearMobileLights queue_start (s0_ game_state)) s1' lookUp 0)
                           (\e -> gplcError w_grid_upd (f_grid_ game_state) obj_grid_upd (s0_ game_state) (s1_ game_state) e)
        linkGplc0 True False False (x0:x1:xs) (z0:z1:z2:zs)
                  (game_state {event_context = event_context_ run_gplc', f_grid_ = f_grid__ run_gplc', s0_ = s0__ run_gplc', s1_ = s1__ run_gplc'})
                  (w_grid_upd_ run_gplc') (obj_grid_upd_ run_gplc') lookUp
      else do
        putStr ("\nSignal addressed to Obj_grid " ++ show target1 ++ " but this element is not set to run programs from.")
        linkGplc0 True False False (x0:x1:xs) (z0:z1:z2:zs) (game_state {s1_ = (s1_ game_state) {sig_q = tail (sig_q (s1_ game_state))}}) w_grid_upd
                  obj_grid_upd lookUp
  else do
    update <- forceUpdate0 w_grid_upd [] 0
    return game_state {w_grid_ = w_grid_ game_state // update, obj_grid_ = obj_grid_ game_state // (atomiseObjGridUpd 0 obj_grid_upd [] (obj_grid_ game_state))}

linkGplc1 :: Play_state0 -> Play_state1 -> Array (Int, Int, Int) Obj_grid -> Int -> IO Play_state1
linkGplc1 s0 s1 obj_grid mode =
  let dest0 = (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0))
      dest1 = (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0))
  in do
  if mode == 0 then 
    if objType (obj_grid ! dest0) == 1 || objType (obj_grid ! dest0) == 3 then
      return s1 {sig_q = sig_q s1 ++ [Signal {sigNum = 1, originGameT = fst__ (gameClock s0), target = dest1}]}
    else return s1
  else
    if objType (obj_grid ! dest0) == 1 || objType (obj_grid ! dest0) == 3 then do
      if health s1 <= detDamage (difficulty s1) True s0 s1 then return s1 {health = 0, state_chg = 1, message = 0 : msg26}
      else return s1 {sig_q = sig_q s1 ++ [Signal {sigNum = 1, originGameT = fst__ (gameClock s0), target = dest1}], health = (health s1) - detDamage (difficulty s1) True s0 s1,
                      state_chg = 1, message = 0 : msg13}
    else do
      if health s1 <= detDamage (difficulty s1) True s0 s1 then return s1 {health = 0, state_chg = 1, message = 0 : msg26}
      else return s1 {health = health s1 - detDamage (difficulty s1) True s0 s1, state_chg = 1, message = 0 : msg13}

linkGplc2 :: Float -> (Int, Int, Int) -> (Int, Int, Int)
linkGplc2 0 (w, u, v) = (w, u, v + 1)
linkGplc2 1 (w, u, v) = (w, u + 1, v)
linkGplc2 2 (w, u, v) = (w, u, v - 1)
linkGplc2 3 (w, u, v) = (w, u - 1, v)

-- These four functions perform game physics and geometry computations.
-- These include player collision detection, thrust, friction, gravity and floor surface modelling.
detectColl :: Int -> (Float, Float) -> (Float, Float) -> Array (Int, Int, Int) Obj_grid -> Array (Int, Int, Int) Wall_grid -> [Float]
detectColl w_block (u, v) (step_u, step_v) obj_grid w_grid =
  let u' = u + step_u
      v' = v + step_v
      grid_i = w_grid ! (w_block, truncate u, truncate v)
      grid_o0 = objType (obj_grid ! (w_block, truncate u, (truncate v) + 1))
      grid_o1 = objType (obj_grid ! (w_block, (truncate u) + 1, truncate v))
      grid_o2 = objType (obj_grid ! (w_block, truncate u, (truncate v) - 1))
      grid_o3 = objType (obj_grid ! (w_block, (truncate u) - 1, truncate v))
  in
  if v' > v2_bound grid_i && v2 grid_i == True then
    if (u' < u1_bound grid_i || u' > u2_bound grid_i) && (u1 grid_i == True || u2 grid_i == True) then [u, v, 1, 1, 0, 0]
    else [u', v, 0, 1, 0, 0]
  else if v' > v2_bound grid_i && grid_o0 > 1 then
    if (u' < u1_bound grid_i || u' > u2_bound grid_i) && (grid_o1 > 1 || grid_o3 > 1) then [u, v, 1, 1, 0, (fromIntegral grid_o0)]
    else [u', v, 0, 1, 0, (fromIntegral grid_o0)]
  else if v' > v2_bound grid_i && grid_o0 == 1 then [u', v', 0, 0, 0, 1]
  else if u' > u2_bound grid_i && u2 grid_i == True then
    if (v' < v1_bound grid_i || v' > v2_bound grid_i) && (v1 grid_i == True || v2 grid_i == True) then [u, v, 1, 1, 0, 0]
    else [u, v', 1, 0, 0, 0]
  else if u' > u2_bound grid_i && grid_o1 > 1 then
    if (v' < v1_bound grid_i || v' > v2_bound grid_i) && (grid_o0 > 1 || grid_o2 > 1) then [u, v, 1, 1, 1, (fromIntegral grid_o1)]
    else [u, v', 1, 0, 1, (fromIntegral grid_o1)]
  else if u' > u2_bound grid_i && grid_o1 == 1 then [u', v', 0, 0, 1, 1]
  else if v' < v1_bound grid_i && v1 grid_i == True then
    if (u' < u1_bound grid_i || u' > u2_bound grid_i) && (u1 grid_i == True || u2 grid_i == True) then [u, v, 1, 1, 0, 0]
    else [u', v, 0, 1, 0, 0]
  else if v' < v1_bound grid_i && grid_o2 > 1 then
    if (u' < u1_bound grid_i || u' > u2_bound grid_i) && (grid_o1 > 1 || grid_o3 > 1) then [u, v, 1, 1, 2, (fromIntegral grid_o2)]
    else [u', v, 0, 1, 2, (fromIntegral grid_o2)]
  else if v' < v1_bound grid_i && grid_o2 == 1 then [u', v', 0, 0, 2, 1]
  else if u' < u1_bound grid_i && u1 grid_i == True then
    if (v' < v1_bound grid_i || v' > v2_bound grid_i) && (v1 grid_i == True || v2 grid_i == True) then [u, v, 1, 1, 0, 0]
    else [u, v', 1, 0, 0, 0]
  else if u' < u1_bound grid_i && grid_o3 > 1 then
    if (v' < v1_bound grid_i || v' > v2_bound grid_i) && (grid_o0 > 1 || grid_o2 > 1) then [u, v, 1, 1, 3, (fromIntegral grid_o3)]
    else [u, v', 1, 0, 3, (fromIntegral grid_o3)]
  else if u' < u1_bound grid_i && grid_o3 == 1 then [u', v', 0, 0, 3, 1]
  else [u', v', 0, 0, 0, 0]

thrust :: Int -> Int -> Float -> UArray (Int, Int) Float -> [Float]
thrust dir a force lookUp =
  if dir == 3 then transform [force / 40, 0, 0, 1] (rotationW a lookUp)
  else if dir == 4 then transform [force / 40, 0, 0, 1] (rotationW (modAngle a 471) lookUp)
  else if dir == 5 then transform [force / 40, 0, 0, 1] (rotationW (modAngle a 314) lookUp)
  else transform [force / 40, 0, 0, 1] (rotationW (modAngle a 157) lookUp)

floorSurf1 :: Float -> Float -> Float -> Array (Int, Int, Int) Floor_grid -> Float
floorSurf1 u v w f_grid =
  let f_tile0 = f_grid ! (truncate w, truncate (u / 2), truncate (v / 2))
      f_tile1 = f_grid ! ((truncate w) - 1, truncate (u / 2), truncate (v / 2))
  in
  if surface f_tile0 == Open then
    if surface f_tile1 == Positive_u then (w_ f_tile1) + (mod' u 2) / 2 + 0.1
    else if surface f_tile1 == Negative_u then 1.1 + (w_ f_tile1) - (mod' u 2) / 2
    else if surface f_tile1 == Positive_v then (w_ f_tile1) + (mod' v 2) / 2 + 0.1
    else if surface f_tile1 == Negative_v then 1.1 + (w_ f_tile1) - (mod' v 2) / 2
    else if surface f_tile1 == Flat then w_ f_tile1 + 0.1
    else 0
  else
    if surface f_tile0 == Positive_u then (w_ f_tile0) + (mod' u 2) / 2 + 0.1
    else if surface f_tile0 == Negative_u then 1.1 + (w_ f_tile0) - (mod' u 2) / 2
    else if surface f_tile0 == Positive_v then (w_ f_tile0) + (mod' v 2) / 2 + 0.1
    else if surface f_tile0 == Negative_v then 1.1 + (w_ f_tile0) - (mod' v 2) / 2
    else w_ f_tile0 + 0.1

floorSurf0 :: Float -> Float -> Float -> Array (Int, Int, Int) Floor_grid -> Float
floorSurf0 u v w f_grid =
  let floorSurf1_ = floorSurf1 u v w f_grid
  in
  if floorSurf1_ > 2.1 then 2.1
  else floorSurf1_

updateVel :: [Float] -> [Float] -> [Float] -> Float -> Float -> [Float]
updateVel [] _ _ f_rate f = []
updateVel (x:xs) (y:ys) (z:zs) f_rate f =
  if z == 1 then 0 : updateVel xs ys zs f_rate f
  else (x + y / 32 + f * x / f_rate) : updateVel xs ys zs f_rate f

-- This function ensures that all signals sent to NPC GPLC programs are run before any others.  This is done to fix a corner case problem that occured when an
-- NPC and projectile tried to enter the same voxel in the same tick.
prioritiseNpcs :: [Int] -> [Int] -> [Int] -> [Int]
prioritiseNpcs [] acc0 acc1 = acc0 ++ acc1
prioritiseNpcs (x0:x1:x2:x3:xs) acc0 acc1 =
  if x0 > 127 then prioritiseNpcs xs (x0 : x1 : x2 : x3 : acc0) acc1
  else prioritiseNpcs xs acc0 (x0 : x1 : x2 : x3 : acc1)

-- This function handles preemptive ceiling collision detection (i.e. stops the player jumping if there is a ceiling directly above).
jumpAllowed :: Array (Int, Int, Int) Floor_grid -> Play_state0 -> Play_state1 -> Bool
jumpAllowed f_grid s0 s1
  | truncate (pos_w s0) == 2 = jump_enabled
  | surface f_grid_voxel == Open = jump_enabled
  | otherwise = False
  where f_grid_voxel = f_grid ! (truncate (pos_w s0) + 1, div (truncate (pos_u s0)) 2, div (truncate (pos_v s0)) 2)
        jump_enabled = if playerClass s1 == [2, 31, 40, 63, 4, 27, 48, 35, 31, 45] then False
                       else True

-- The frames per second (FPS) measurements made here are used to drive the optional on screen FPS report and to scale player movement rates in real time,
-- to allow for a variable frame rate with consistent game play speed.  It is intended that the engine will be limited to ~60 FPS
-- (set via the "min_frame_t" field of the conf_reg array) with movement scaling applied between 40 - 60 FPS.  Below 40 FPS game play slow down will be seen.
determineFps :: SEQ.Seq Integer -> Float -> Integer -> (Float, [Int], SEQ.Seq Integer)
determineFps t_seq scaling t_current =
  let frame_rate0 = 1000000000 / (fromIntegral (t_current - SEQ.index t_seq 0) / 40)
      frame_rate1 = if frame_rate0 >= 40 then frame_rate0
                    else 40
  in
  if SEQ.length t_seq < 40 then (48, [-1, 6, 16, 19, 69, 63] ++ convMsg 0, t_seq SEQ.|> t_current)
  else (frame_rate1 / scaling, [-1, 6, 16, 19, 69, 63] ++ convMsg (truncate frame_rate0), (SEQ.drop 1 (t_seq SEQ.|> t_current)))

-- Game time is now composed of game_t (GPLC interpreter ticks) and frame_num (number of the next frame to be rendered).  These two functions deal with updating
-- game time and preparing a user readable representation of it, respectively.
updateGameClock :: (Int, Float, Int) -> Float -> Float -> (Bool, (Int, Float, Int))
updateGameClock (game_t, fl_game_t, frame_num) f_rate scaling =
  let fl_game_t' = fl_game_t + (1 / f_rate) / (1 / scaling)
  in
  if truncate fl_game_t == truncate fl_game_t' then (False, (game_t, fl_game_t', frame_num + 1))
  else (True, (truncate fl_game_t', fl_game_t', frame_num + 1))

-- This function generates a report of the player position within the map using the message tile system.
showMapPos :: Play_state0 -> [Int]
showMapPos s0 =
  let pos_chars = \pos -> if pos < 1 then [53, 66] ++ convMsg (truncate (pos * 10))
                          else if pos < 10 then take 1 (convMsg (truncate (pos * 10))) ++ [66] ++ drop 1 (convMsg (truncate (pos * 10)))
                          else take 2 (convMsg (truncate (pos * 10))) ++ [66] ++ drop 2 (convMsg (truncate (pos * 10)))
  in [0, 47, 69, 63] ++ pos_chars (pos_u s0) ++ [63, 48, 69, 63] ++ pos_chars (pos_v s0) ++ [63, 49, 69, 63] ++ pos_chars (pos_w s0)

-- Used to send a set of in game metrics to the message display system, depending on the value of the "on_screen_metrics" field of the conf_reg array.
collectMetrics :: [Int] -> [Int] -> [Char] -> Play_state0 -> [(Int, [Int])]
collectMetrics fps_metric pos_metric game_t_metric s0 =
  let proc_time = \(x0:x1:x2:x3:x4:x5:xs) -> [read [x0] + 53, read [x1] + 53, 69, read [x2] + 53, read [x3] + 53, 69, read [x4] + 53, read [x5] + 53]
  in
  if on_screen_metrics s0 == 1 then [(60, fps_metric)]
  else if on_screen_metrics s0 == 2 then [(60, fps_metric), (60, pos_metric)]
  else [(60, fps_metric), (60, pos_metric), (60, proc_time game_t_metric)]

-- Restart the background music track each time a preset period has elapsed, if music is enabled.
playMusic :: Int -> Int -> Array Int Source -> IO ()
playMusic t period sound_array = do
  if period == 0 then return ()
  else if mod t period == 0 || t == 40 then play_ (sound_array ! (snd (bounds sound_array)))
  else return ()

-- This function is used by updatePlay to update the physics related fields of the Play_state0 structure.
s0' :: [Float] -> Float -> Float -> [Float] -> [Float] -> (Bool -> Float) -> (Int, Float, Int) -> Float -> Float -> Float -> Float -> Float -> UArray (Int, Int) Float
       -> SEQ.Seq Integer -> Integer -> Int -> Play_state0 -> Int -> Play_state0
s0' pos_uv pos_w0 pos_w1 vel0 vel1 angle' game_clock' mag_r mag_j scaling f_rate f look_up t_seq t_current control s0 mode
  | mode == 0 = s0 {pos_u = pos_uv !! (0 :: Int), pos_v = pos_uv !! (1 :: Int), vel = vel0, gameClock = game_clock'}
  | mode == 1 = s0 {pos_u = pos_uv !! (0 :: Int), pos_v = pos_uv !! (1 :: Int), pos_w = pos_w0,
                    vel = updateVel (vel s0) (take 3 (thrust (fromIntegral control) (angle s0) mag_r look_up)) ((drop 2 pos_uv) ++ [0]) f_rate f,
                    gameClock = game_clock'}
  | mode == 2 = s0 {pos_u = pos_uv !! (0 :: Int), pos_v = pos_uv !! (1 :: Int), pos_w = pos_w0, vel = vel0, angle = truncate (angle' False),
                    angle_ = (angle' False), gameClock = game_clock'}
  | mode == 3 = s0 {pos_u = pos_uv !! (0 :: Int), pos_v = pos_uv !! (1 :: Int), pos_w = pos_w0, vel = vel0, angle = truncate (angle' True),
                    angle_ = (angle' True), gameClock = game_clock'}
  | mode == 4 = s0 {pos_u = pos_uv !! (0 :: Int), pos_v = pos_uv !! (1 :: Int), pos_w = pos_w0 + mag_j / f_rate, vel = (take 2 vel0) ++ [mag_j],
                    gameClock = game_clock'}
  | mode == 7 = s0 {pos_u = pos_uv !! (0 :: Int), pos_v = pos_uv !! (1 :: Int), pos_w = pos_w1, vel = vel1, angle = truncate (angle' False),
                    angle_ = (angle' False), gameClock = game_clock'}
  | mode == 8 = s0 {pos_u = pos_uv !! (0 :: Int), pos_v = pos_uv !! (1 :: Int), pos_w = pos_w1, vel = vel1, angle = truncate (angle' True),
                    angle_ = (angle' True), gameClock = game_clock'}
  | mode == 9 = s0 {pos_u = pos_uv !! (0 :: Int), pos_v = pos_uv !! (1 :: Int), pos_w = pos_w1, vel = vel1, gameClock = game_clock'}
  | mode == 12 = s0 {message_ = collectMetrics (snd__ (determineFps t_seq scaling t_current)) (showMapPos s0) (showGameTime (fst__ (gameClock s0)) [] False) s0,
                     gameClock = game_clock'}
  | otherwise = s0 {pos_u = pos_uv !! (0 :: Int), pos_v = pos_uv !! (1 :: Int), pos_w = pos_w0, vel = vel0, gameClock = game_clock'}

-- updatePlay is called from Main.startGame through the two wrapper functions below.  This means that if an exception occurs
-- within the game logic thread control is returned to the rendering thread, allowing for the options of a game logic thread restart or engine shutdown.
updatePlayWrapper0 :: Io_box -> MVar Game_state -> Game_state
                      -> Bool -> Integer -> GamePhysics
                      -> UArray (Int, Int) Float -> (Array Int Source, Int) -> Integer -> MVar Integer
                      -> SEQ.Seq Integer -> Float -> IO ()
updatePlayWrapper0 io_box state_ref game_state in_flight min_frame_t physics look_up
                   sound_array t_last t_log t_seq f_rate =
  catch (updatePlay io_box state_ref game_state in_flight min_frame_t physics look_up sound_array t_last t_log t_seq f_rate)
        (\e -> updatePlayWrapper1 state_ref e)

updatePlayWrapper1 :: MVar Game_state -> SomeException -> IO ()
updatePlayWrapper1 state_ref e = do
  putStr ("\n\nGame logic thread exception: " ++ show e)
  putMVar state_ref (def_game_state {event_context = ExitGame})

-- This function recurses once for each recursion of showFrame (and rendering of that frame) and is the central branching point of the game logic thread.
updatePlay :: Io_box -> MVar Game_state -> Game_state -> Bool -> Integer
              -> GamePhysics
              -> UArray (Int, Int) Float -> (Array Int Source, Int) -> Integer -> MVar Integer -> SEQ.Seq Integer -> Float -> IO ()
updatePlay io_box state_ref game_state in_flight min_frame_t physics lookUp sound_array t_last t_log t_seq f_rate =
  let w_grid = w_grid_ game_state
      f_grid = f_grid_ game_state
      obj_grid = obj_grid_ game_state
      s0 = s0_ game_state
      s1 = s1_ game_state
      det = detectColl (truncate (pos_w s0)) (pos_u s0, pos_v s0)
                       ((vel s0) !! (0 :: Int) / f_rate, (vel s0) !! (1 :: Int) / f_rate)
                       obj_grid w_grid
      pos_w0 = floorSurf0 (det !! (0 :: Int)) (det !! (1 :: Int)) (pos_w s0) f_grid
      pos_w1 = (pos_w s0 + ((vel s0) !! (2 :: Int)) / f_rate)
      floor = pos_w0
      vel0 = updateVel (vel s0) [0, 0, 0] ((drop 2 det) ++ [0]) f_rate (friction physics)
      vel1 = updateVel (vel s0) [0, 0, gravity physics] ((drop 2 det) ++ [0]) f_rate 0
      game_clock' = updateGameClock (gameClock (s0_ game_state)) (f_rate * speedScaling physics) (30 * speedScaling physics)
      angle' = modAngle_ (angle_ s0) f_rate
      det_fps = \t_current -> determineFps t_seq (speedScaling physics) t_current
      s0'_ = s0' det pos_w0 pos_w1 vel0 vel1 angle' (snd game_clock') (magRun physics) (magJump physics) (speedScaling physics) f_rate (friction physics)
                 lookUp t_seq
      player_voxel = [truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)]
  in do
  mainLoopEvent
  control <- readIORef (fromJust (control_ io_box))
  writeIORef (fromJust (control_ io_box)) 0
  link1 <- linkGplc1 s0 s1 obj_grid 0
  link1_ <- linkGplc1 s0 s1 obj_grid 1
  t <- getTime Monotonic
  if t_last == 0 then
    updatePlay io_box state_ref game_state in_flight min_frame_t physics lookUp sound_array (toNanoSecs t) t_log
               (third_ (det_fps (toNanoSecs t))) 60
  else do
    if toNanoSecs t - t_last < min_frame_t then do
      threadDelay (fromIntegral (div (min_frame_t - (toNanoSecs t - t_last)) 1000))
      t' <- getTime Monotonic
      putMVar t_log (toNanoSecs t')
    else putMVar t_log (toNanoSecs t)
  t'' <- takeMVar t_log
  if mod (fst__ (gameClock s0)) 40 == 0 then do
    if on_screen_metrics s0 > 0 then do
      playMusic (fst__ (gameClock s0)) (snd sound_array) (fst sound_array)
      updatePlay io_box state_ref (game_state {s0_ = s0'_ (toNanoSecs t) control s0 12}) in_flight min_frame_t physics lookUp
                 sound_array t_last t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
    else do
      playMusic (fst__ (gameClock s0)) (snd sound_array) (fst sound_array)
      updatePlay io_box state_ref (game_state {s0_ = s0 {gameClock = snd game_clock'}}) in_flight min_frame_t physics lookUp
                 sound_array t_last t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if control == 2 then do
    link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 6}) [] [] lookUp
    updatePlay io_box state_ref 
               (link0 {obj_grid_ = pauseMenu link0, s0_ = (s0_ link0) {message_ = []},
                       s1_ = (s1_ link0) {sig_q = Signal {sigNum = 16, originGameT = fst__ (gameClock s0), target = (2, 0, 0)} : sig_q (s1_ link0)}})
               in_flight min_frame_t physics lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if control == 10 then do
    link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 6}) [] [] lookUp
    updatePlay io_box state_ref
               (link0 {s0_ = (s0_ link0) {message_ = []},
                       s1_ = (s1_ link0) {sig_q = Signal {sigNum = 2, originGameT = fst__ (gameClock s0), target = (0, 0, 0)} : sig_q (s1_ link0)}})
               in_flight min_frame_t physics lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if control == 11 then do
    link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 6}) [] [] lookUp
    if view_mode s0 == 0 then
      updatePlay io_box state_ref (link0 {s0_ = (s0_ link0) {message_ = [], view_mode = 1}}) in_flight min_frame_t physics
                 lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
    else
      updatePlay io_box state_ref (link0 {s0_ = (s0_ link0) {message_ = [], view_mode = 0}}) in_flight min_frame_t physics
                 lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if control == 12 then do
    link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 6}) [] [] lookUp
    updatePlay io_box state_ref (link0 {s0_ = (s0_ link0) {message_ = [], view_angle = modAngle (view_angle (s0_ link0)) 5}}) in_flight min_frame_t
               physics lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if control == 13 then do
    link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 6}) [] [] lookUp
    updatePlay io_box state_ref
               (link0 {s0_ = (s0_ link0) {message_ = []},
                       s1_ = (s1_ link0) {sig_q = Signal {sigNum = 2, originGameT = fst__ (gameClock s0), target = (0, 0, 1)} : sig_q (s1_ link0)}})
                       in_flight min_frame_t physics lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if message s1 /= [] then do
    event <- procMsg0 (message s1) s0 s1 io_box (fst sound_array)
    putMVar state_ref (game_state {event_context = third_ event, s0_ = fst__ event})
    updatePlay io_box state_ref (game_state {s0_ = (fst__ event) {message_ = []}, s1_ = snd__ event}) in_flight min_frame_t physics lookUp
               sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else
    if in_flight == False then
      if (pos_w s0) - floor > 0.02 then do
        putMVar state_ref game_state
        link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 0}) [] [] lookUp
        updatePlay io_box state_ref link0 True min_frame_t physics
                   lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else if control > 2 && control < 7 then do
        putMVar state_ref game_state
        link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 1}) [] [] lookUp
        updatePlay io_box state_ref link0 False min_frame_t physics
                   lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else if control == 7 then do
        putMVar state_ref game_state
        link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 2}) [] [] lookUp
        updatePlay io_box state_ref link0 False min_frame_t physics
                   lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else if control == 8 then do
        putMVar state_ref game_state
        link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 3}) [] [] lookUp
        updatePlay io_box state_ref link0 False min_frame_t physics
                   lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else if control == 9 && jumpAllowed f_grid s0 s1 == True then do
        putMVar state_ref game_state
        link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 4}) [] [] lookUp
        updatePlay io_box state_ref link0 False min_frame_t physics
                   lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else do
        putMVar state_ref game_state
        link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 6}) [] [] lookUp
        updatePlay io_box state_ref link0 False min_frame_t physics
                   lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
    else if in_flight == True && (pos_w s0) > floor then
      if control == 7 then do
        putMVar state_ref game_state
        link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 7}) [] [] lookUp
        updatePlay io_box state_ref link0 True min_frame_t physics
                   lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else if control == 8 then do
        putMVar state_ref game_state
        link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 8}) [] [] lookUp
        updatePlay io_box state_ref link0 True min_frame_t physics
                   lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else do
        putMVar state_ref game_state
        link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 9}) [] [] lookUp
        updatePlay io_box state_ref link0 True min_frame_t physics
                   lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
    else do
      putMVar state_ref game_state
      link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 10}) [] [] lookUp
      if (vel s0) !! (2 :: Int) < -4 then do
        updatePlay io_box state_ref (link0 {s1_ = link1_}) False min_frame_t physics
                   lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else do
        putMVar state_ref game_state
        link0 <- linkGplc0 (fst game_clock') True True (drop 4 det) player_voxel (game_state {s0_ = s0'_ 0 control ((s0_ game_state) {message_ = []}) 11}) [] [] lookUp
        updatePlay io_box state_ref (link0 {s1_ = link1}) False min_frame_t physics
                   lookUp sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))

char_list = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 ,'.?;:+-=!()<>"

-- These four functions handle events triggered by a call to passMsg within a GPLC program.  These include on screen messages,
-- object interaction menus and sound effects.
findTile :: [Char] -> Char -> Int -> Int
findTile [] t i = (i + 1)
findTile (x:xs) t i =
  if x == t then (i + 1)
  else findTile xs t (i + 1)

convMsg_ :: [Char] -> [Int]
convMsg_ [] = []
convMsg_ (x:xs) = findTile char_list x 0 : convMsg_ xs

procMsg1 :: [[Int]] -> [(Int, [Int])]
procMsg1 [] = []
procMsg1 (x:xs) = (head x, tail x) : procMsg1 xs

procMsg0 :: [Int] -> Play_state0 -> Play_state1 -> Io_box -> Array Int Source -> IO (Play_state0, Play_state1, EventContext)
procMsg0 [] s0 s1 io_box sound_array = return (s0, s1 {state_chg = 0, message = []}, None)
procMsg0 (x0:x1:xs) s0 s1 io_box sound_array =
  let signal_ = (head (splitOn [-1] (take x1 xs)))
  in do
  if x0 == -5 then return (s0, s1, None)
  else if x0 < 0 then return (s0 {message_ = message_ s0 ++ [(x0, take x1 xs)]}, s1, None)
  else if x0 == 0 && state_chg s1 == 1 && health s1 <= 0 then do
    play_ (sound_array ! 20)
    return (s0 {message_ = [(-2, take x1 xs)]}, s1, PlayerDied)
  else if x0 == 0 && state_chg s1 == 1 then procMsg0 (drop x1 xs) (s0 {message_ = message_ s0 ++ [(600, x0 : take x1 xs ++ msg1 ++ convMsg (health s1))]}) s1
                                                     io_box sound_array
  else if x0 == 0 && state_chg s1 == 2 then procMsg0 (drop x1 xs) (s0 {message_ = message_ s0 ++ [(600, x0 : take x1 xs ++ msg2 ++ convMsg (ammo s1))]}) s1
                                                     io_box sound_array
  else if x0 == 0 && state_chg s1 == 3 then procMsg0 (drop x1 xs) (s0 {message_ = message_ s0 ++ [(600, x0 : take x1 xs ++ msg3 ++ convMsg (gems s1))]}) s1
                                                     io_box sound_array
  else if x0 == 0 && state_chg s1 == 4 then procMsg0 (drop x1 xs) (s0 {message_ = message_ s0 ++ [(600, x0 : take x1 xs ++ msg4 ++ convMsg (torches s1))]}) s1
                                                     io_box sound_array
  else if x0 == 0 then procMsg0 (drop x1 xs) (s0 {message_ = message_ s0 ++ [(600, x0 : take x1 xs)]}) s1 io_box sound_array
  else if x0 == 2 then do
    if (xs !! (0 :: Int)) == 0 then return ()
    else play_ (sound_array ! ((xs !! (0 :: Int)) - 1))
    procMsg0 (drop 1 xs) s0 s1 io_box sound_array
  else if x0 == 3 then
    procMsg0 (drop 3 xs)
             (s0 {pos_u = intToFloat (xs !! (0 :: Int)), pos_v = intToFloat (xs !! (1 :: Int)), pos_w = intToFloat (xs !! (2 :: Int))})
             s1 io_box sound_array
  else do
    choice <- runMenu (-1) (procMsg1 (tail (splitOn [-1] (take x1 xs)))) [] io_box (-0.96) (-0.2) 1 0 0 s0 (x0 - 3)
    procMsg0 (drop x1 xs) s0
             (s1 {sig_q = Signal {sigNum = choice + 1, originGameT = fst__ (gameClock s0),
                                  target = (signal_ !! (0 :: Int), signal_ !! (1 :: Int), signal_ !! (2 :: Int))} : sig_q s1})
             io_box sound_array

-- Used by the game logic thread for in game menus and by the main thread for the main menu.
runMenu :: Int -> [(Int, [Int])] -> [(Int, [Int])] -> Io_box -> Float -> Float -> Int -> Int -> Int -> Play_state0 -> Int -> IO Int
runMenu shortcut [] acc io_box x y c c_max 0 s0 background = runMenu shortcut acc [] io_box x y c c_max 2 s0 background
runMenu shortcut (n:ns) acc io_box x y c c_max 0 s0 background = do
  if shortcut /= -1 then return shortcut
  else if fst n == 0 then runMenu shortcut ns (acc ++ [n]) io_box x y c c_max 0 s0 background
  else runMenu shortcut ns (acc ++ [n]) io_box x y c (c_max + 1) 0 s0 background
runMenu shortcut [] acc io_box x y c c_max d s0 background = do
  swapBuffers
  threadDelay 16667
  mainLoopEvent
  control <- readIORef (fromJust (control_ io_box))
  writeIORef (fromJust (control_ io_box)) 0
  if control == 14 && c > 1 then do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    runMenu shortcut acc [] io_box x 0.1 (c - 1) c_max 2 s0 background
  else if control == 15 && c < c_max then do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    runMenu shortcut acc [] io_box x 0.1 (c + 1) c_max 2 s0 background
  else if control == 2 then return c
  else do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    runMenu shortcut acc [] io_box x 0.1 c c_max 2 s0 background
runMenu shortcut (n:ns) acc io_box x y c c_max d s0 background = do
  if d == 2 then do
    glBindVertexArray (unsafeCoerce ((fst (p_bind_ io_box)) ! 1027))
    glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst (p_bind_ io_box)) ! (1027 + background)))
    glUseProgram (unsafeCoerce ((fst (p_bind_ io_box)) ! ((snd (p_bind_ io_box)) - 3)))
    glUniform1i (fromIntegral ((uniform_ io_box) ! 38)) 0
    p_tt_matrix <- mallocBytes (glfloat * 16)
    loadArray [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1] p_tt_matrix 0
    glUniformMatrix4fv (fromIntegral ((uniform_ io_box) ! 36)) 1 1 p_tt_matrix
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT zero_ptr
    free p_tt_matrix
  else return ()
  glBindVertexArray (unsafeCoerce ((fst (p_bind_ io_box)) ! 933))
  p_tt_matrix <- mallocBytes ((length (snd n)) * glfloat * 16)
  if fst n == c then showText (snd n) 1 933 (uniform_ io_box) (p_bind_ io_box) x y zero_ptr
  else showText (snd n) 0 933 (uniform_ io_box) (p_bind_ io_box) x y zero_ptr
  free p_tt_matrix
  runMenu shortcut ns (acc ++ [n]) io_box x (y - 0.04) c c_max 1 s0 background

-- This function handles the drawing of message tiles (letters and numbers etc) that are used for in game messages and in menus.
showText :: [Int] -> Int -> Int -> UArray Int Int32 -> (UArray Int Word32, Int) -> Float -> Float -> Ptr GLfloat -> IO ()
showText [] mode base uniform p_bind x y p_tt_matrix = do
  glEnable GL_DEPTH_TEST
  free p_tt_matrix
showText (m:ms) mode base uniform p_bind x y p_tt_matrix = do
  if minusPtr p_tt_matrix zero_ptr == 0 then do
    p_tt_matrix_ <- mallocBytes (16 * glfloat)
    glBindVertexArray (unsafeCoerce ((fst p_bind) ! 933))
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 3)))
    glDisable GL_DEPTH_TEST
    showText (m:ms) mode base uniform p_bind x y p_tt_matrix_
  else do
    loadArray (MAT.toList (translation x y 0)) (castPtr p_tt_matrix) 0
    if mode == 0 && m < 83 then do
      glUniformMatrix4fv (unsafeCoerce (uniform ! 36)) 1 1 p_tt_matrix
      glUniform1i (unsafeCoerce (uniform ! 38)) 0
    else if mode == 1 && m < 83 then do
      glUniformMatrix4fv (unsafeCoerce (uniform ! 36)) 1 1 p_tt_matrix
      glUniform1i (unsafeCoerce (uniform ! 38)) 1
    else showText ms mode base uniform p_bind x y p_tt_matrix
    glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst p_bind) ! (base + m)))
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT zero_ptr
    showText ms mode base uniform p_bind (x + 0.04) y p_tt_matrix


