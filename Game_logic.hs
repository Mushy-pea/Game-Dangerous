-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module Game_logic where

import Prelude hiding ((!!))
import Index_wrapper
import System.IO
import System.IO.Unsafe
import System.Exit
import Graphics.GL.Core33
import Graphics.UI.GLUT hiding (Flat, texture, GLfloat)
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
import Build_model
import Game_sound

-- Used to load C style arrays, which are used with certain OpenGL functions.
load_array :: Storable a => [a] -> Ptr a -> Int -> IO ()
load_array [] p i = return ()
load_array (x:xs) p i = do
  pokeElemOff p i x
  load_array xs p (i + 1)

-- These two functions are used to patch a problem that can happen within project_update and cause an engine shutdown.
filter_sig_q :: [Int] -> (Int, Int, Int) -> (Int, Int, Int) -> [Int]
filter_sig_q [] (i0, i1, i2) (i3, i4, i5) = []
filter_sig_q (x0:x1:x2:x3:xs) (i0, i1, i2) (i3, i4, i5) =
  if (x1, x2, x3) == (i0, i1, i2) || (x1, x2, x3) == (i3, i4, i5) then filter_sig_q xs (i0, i1, i2) (i3, i4, i5)
  else [x0, x1, x2, x3] ++ filter_sig_q xs (i0, i1, i2) (i3, i4, i5)

sub_i :: Int -> Array (Int, Int, Int) (Int, [Int]) -> (Int, Int, Int) -> (Int, Int, Int) -> Play_state1 -> ((Int, [Int]), Play_state1)
sub_i location arr (i0, i1, i2) (i3, i4, i5) s1 =
  let bd = bounds arr
      error_string = ("\n\nError in array index.  location: " ++ show location ++ " index: " ++ show (i0, i1, i2) ++ " bounds: " ++ show bd ++ "\nThe engine will be allowed to continue due to a designed exception for when the error arises from project_update.\nOriginating script loop terminated with signal queue filter.")
  in
  if i0 < fst__ (fst bd) || i0 > fst__ (snd bd) || i1 < snd__ (fst bd) || i1 > snd__ (snd bd) || i2 < third_ (fst bd) || i2 > third_ (snd bd) then unsafePerformIO (putStr error_string >> return ((2, []), s1 {next_sig_q = filter_sig_q (next_sig_q s1) (i0, i1, i2) (i3, i4, i5)}))
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
msg25 = [8,15,47,29,34,73,2,4,17]; msg26 = [39,6,27,46,27,38,63,32,27,38,38,66,63,53,63,30,27,51,45,63,45,35,40,29,31,63,38,27,45,46,63,27,29,29,35,30,31,40,46,66]
--      <                                Player shredded by a bullet. What a blood bath!                                                             >
msg27 = [47,16,38,27,51,31,44,63,45,34,44,31,30,30,31,30,63,28,51,63,27,63,28,47,38,38,31,46,66,63,23,34,27,46,63,27,63,28,38,41,41,30,63,28,27,46,34,73]
--      <                                Player was eaten by a centipede. Tasty!                                             >
msg28 = [39,16,38,27,51,31,44,63,49,27,45,63,31,27,46,31,40,63,28,51,63,27,63,29,31,40,46,35,42,31,30,31,66,63,20,27,45,46,51,73]
--      <                    Ouch...Centipede bite!                                           >
msg29 = [2, 4, 17, 0, 15, 47, 29, 34, 66, 66, 66, 3, 31, 40, 46, 35, 42, 31, 30, 31, 63, 28, 35, 46, 31, 73]
-- <                               Choose which game to load                                                     >    <               Game state:                               >          <                         Game time:                       >
choose_game_text = [3, 34, 41, 41, 45, 31, 63, 49, 34, 35, 29, 34, 63, 33, 27, 39, 31, 63, 46, 41, 63, 38, 41, 27, 30]; game_state_text = [7, 27, 39, 31, 63, 45, 46, 27, 46, 31, 69, 63] :: [Int]; game_time_text = [63, 7, 27, 39, 31, 63, 46, 35, 39, 31, 69, 63] :: [Int]
load_game_menu_header = [(0, choose_game_text), (0, [])] :: [(Int, [Int])]; no_game_states_header = [(0, [14, 41, 63, 33, 27, 39, 31, 63, 45, 46, 27, 46, 31, 45, 63, 32, 41, 47, 40, 30, 66]), (0, [63]), (1, [63]), (2, [63]), (3, [63]), (4, [63]), (5, [63]), (6, [63]), (7, msg12)] :: [(Int, [Int])]
error_opening_file_text = [(0, [20, 34, 31, 44, 31, 63, 49, 27, 45, 63, 27, 40, 63, 31, 44, 44, 41, 44, 63, 41, 42, 31, 40, 35, 40, 33, 63, 46, 34, 35, 45, 63, 32, 35, 38, 31, 66, 63, 63, 19, 41, 44, 44, 51, 66]), (0, []), (1, msg12)] :: [(Int, [Int])]

main_menu_text :: [(Int, [Int])]
main_menu_text = [(0, msg16), (0, []), (1, msg14), (2, msg18), (3, msg12)]

-- The following functions implement a bytecode interpreter of the Game Programmable Logic Controller (GPLC) language, used for game logic scripting.
upd' x y = x + y
upd'' x y = y
upd''' x y = x - y
upd_ x y = x
upd 0 = upd'
upd 1 = upd''
upd 2 = upd'''
upd 3 = upd_
upd_a 0 = mod_angle
upd_a 1 = mod_angle'
upd_b 0 = False
upd_b 1 = True

int_to_surface 0 = Flat
int_to_surface 1 = Positive_u
int_to_surface 2 = Negative_u
int_to_surface 3 = Positive_v
int_to_surface 4 = Negative_v
int_to_surface 5 = Open

int_to_float :: Int -> Float
int_to_float x = (fromIntegral x) * 0.000001

int_to_float_v x y z = ((fromIntegral x) * 0.000001, (fromIntegral y) * 0.000001, (fromIntegral z) * 0.000001)

fl_to_int :: Float -> Int
fl_to_int x = truncate (x * 1000000)

bool_to_int True = 1
bool_to_int False = 0

int_to_bool 0 = False
int_to_bool 1 = True

head_ [] = 1
head_ ls = head ls
tail_ [] = []
tail_ ls = tail ls

head__ [] = error "Invalid Obj_grid structure detected."
head__ ls = head ls

show_ints :: [Int] -> [Char]
show_ints [] = []
show_ints (x:xs) = (show x) ++ ", " ++ show_ints xs

bound_check :: Int -> Int -> ((Int, Int, Int), (Int, Int, Int)) -> Bool
bound_check block axis ((a, b, c), (w_max, u_max, v_max)) =
  if axis == 0 && block > u_max then False
  else if axis == 1 && block > v_max then False
  else True

-- These three functions perform GPLC conditional expression folding, evaluating conditional op - codes at the start of a GPLC program run to yield unconditional code.
on_signal :: [Int] -> [Int] -> Int -> [Int]
on_signal [] code sig = []
on_signal (x0:x1:x2:xs) code sig =
  if x0 == sig then take x2 (drop x1 code)
  else on_signal xs code sig

if1 :: Int -> Int -> Int -> [Int] -> [Int] -> [Int] -> [Int]
if1 0 arg0 arg1 code0 code1 d_list =
  if (d_list, 106) !! arg0 == (d_list, 107) !! arg1 then code0
  else code1
if1 1 arg0 arg1 code0 code1 d_list =
  if (d_list, 108) !! arg0 < (d_list, 109) !! arg1 then code0
  else code1
if1 v arg0 arg1 code0 code1 d_list =
  if (d_list, 110) !! arg0 > ((d_list, 111) !! arg1) + (v - 2) then code0
  else code1

if0 :: [Int] -> [Int] -> [Int]
if0 [] d_list = []
if0 code d_list =
  if (code, 112) !! 0 == 1 then if0 (if1 ((code, 113) !! 1) ((code, 114) !! 2) ((code, 115) !! 3) (take ((code, 116) !! 4) (drop 6 code)) (take ((code, 117) !! 5) (drop (6 + (code, 118) !! 4) code)) d_list) d_list
  else code

-- The remaining GPLC op - codes are implemented here.  The GPLC specification document explains their functions in the context of a game logic virtual machine.
chg_state :: [Int] -> (Int, Int, Int) -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> UArray Int Int -> [((Int, Int, Int), Wall_grid)] -> [Int] -> ([((Int, Int, Int), Wall_grid)], [Int])
chg_state (2:x1:x2:x3:x4:x5:x6:x7:x8:x9:xs) (i0, i1, i2) (i3, i4, i5) w_grid update w_grid_upd d_list =
  if (d_list, 119) !! x1 == 0 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(0, (d_list, 120) !! x2), (1, (d_list, 121) !! x3)]) w_grid_upd d_list
  else if (d_list, 122) !! x1 == 1 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(2, (d_list, 123) !! x2), (3, (d_list, 124) !! x3)]) w_grid_upd d_list
  else if (d_list, 125) !! x1 == 2 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(4, (d_list, 126) !! x2), (5, (d_list, 127) !! x3)]) w_grid_upd d_list
  else if (d_list, 128) !! x1 == 3 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(6, (d_list, 129) !! x2), (7, (d_list, 130) !! x3)]) w_grid_upd d_list
  else if (d_list, 131) !! x1 == 9 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(8, (d_list, 132) !! x2), (9, (d_list, 133) !! x3)]) w_grid_upd d_list
  else if (d_list, 134) !! x1 == 10 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(10, (d_list, 135) !! x2), (11, (d_list, 136) !! x3)]) w_grid_upd d_list
  else if (d_list, 137) !! x1 == 11 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(12, (d_list, 138) !! x2), (13, (d_list, 139) !! x3)]) w_grid_upd d_list
  else throw Invalid_GPLC_op_argument
chg_state code (i0, i1, i2) (i3, i4, i5) w_grid update w_grid_upd d_list =
  let source = ((d_list, 140) !! i0, (d_list, 141) !! i1, (d_list, 142) !! i2)
      dest = ((d_list, 143) !! i3, (d_list, 144) !! i4, (d_list, 145) !! i5)
      grid_i = fromJust (obj (w_grid ! source))
      grid_i' = (obj (w_grid ! source))
  in 
  if isNothing grid_i' == True then (w_grid_upd, code)
  else ((dest, (w_grid ! source) {obj = Just (grid_i {ident_ = upd (update ! 0) (ident_ grid_i) (update ! 1), u__ = upd (update ! 2) (u__ grid_i) (int_to_float (update ! 3)), v__ = upd (update ! 4) (v__ grid_i) (int_to_float (update ! 5)), w__ = upd (update ! 6) (w__ grid_i) (int_to_float (update ! 7)), texture__ = upd (update ! 8) (texture__ grid_i) (update ! 9), num_elem = upd (update ! 10) (num_elem grid_i) (fromIntegral (update ! 11)), obj_flag = upd (update ! 12) (obj_flag grid_i) (update ! 13)})}) : w_grid_upd, code)

chg_grid :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> Wall_grid -> [((Int, Int, Int), Wall_grid)] -> [Int] -> [((Int, Int, Int), Wall_grid)]
chg_grid mode (i0, i1, i2) (i3, i4, i5) w_grid def w_grid_upd d_list =
  let dest0 = ((d_list, 146) !! i0, (d_list, 147) !! i1, (d_list, 148) !! i2)
      dest1 = ((d_list, 149) !! i3, (d_list, 150) !! i4, (d_list, 151) !! i5)
  in
  if (d_list, 152) !! mode == 0 then (dest0, def) : w_grid_upd
  else if (d_list, 153) !! mode == 1 then [(dest1, w_grid ! dest0), (dest0, def)] ++ w_grid_upd
  else (dest1, w_grid ! dest0) : w_grid_upd

chg_grid_ :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
chg_grid_ mode (i0, i1, i2) (i3, i4, i5) obj_grid_upd d_list =
  let source = ((d_list, 154) !! i0, (d_list, 155) !! i1, (d_list, 156) !! i2)
      dest = ((d_list, 157) !! i3, (d_list, 158) !! i4, (d_list, 159) !! i5)
  in
  if (d_list, 160) !! mode == 0 then (source, (-1, [])) : obj_grid_upd
  else if (d_list, 161) !! mode == 1 then (source, (-2, [])) : (dest, (-2, [])) : obj_grid_upd
  else (source, (-3, [])) : (dest, (-3, [])) : obj_grid_upd

chg_floor :: Int -> Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) Floor_grid -> [Int] -> Array (Int, Int, Int) Floor_grid
chg_floor state_val abs v (i0, i1, i2) grid d_list =
  let index = ((d_list, 162) !! i0, (d_list, 163) !! i1, (d_list, 164) !! i2)
  in
  if (d_list, 165) !! state_val == 0 then grid // [(index, (grid ! index) {w_ = upd ((d_list, 166) !! abs) (w_ (grid ! index)) (int_to_float ((d_list, 167) !! v))})]
  else grid // [(index, (grid ! index) {surface = int_to_surface ((d_list, 168) !! v)})]

chg_value :: Int -> Int -> Int -> (Int, Int, Int) -> [Int] -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
chg_value val abs v (i0, i1, i2) d_list obj_grid obj_grid_upd =
  let target = obj_grid ! ((d_list, 169) !! i0, (d_list, 170) !! i1, (d_list, 171) !! i2)
  in
  if val == 536870910 then (((d_list, 172) !! i0, (d_list, 173) !! i1, (d_list, 174) !! i2), (fst target, [(0, v)])) : obj_grid_upd
  else (((d_list, 175) !! i0, (d_list, 176) !! i1, (d_list, 177) !! i2), (fst target, [(val, upd ((d_list, 178) !! abs) (((snd target), 179) !! val) ((d_list, 180) !! v))])) : obj_grid_upd

chg_ps0 :: Int -> Int -> Int -> [Int] -> Play_state0 -> Play_state0
chg_ps0 state_val abs v d_list s0 =
  if (d_list, 181) !! state_val == 0 then s0 {pos_u = upd ((d_list, 182) !! abs) (pos_u s0) (int_to_float ((d_list, 183) !! v))}
  else if (d_list, 184) !! state_val == 1 then s0 {pos_v = upd ((d_list, 185) !! abs) (pos_v s0) (int_to_float ((d_list, 186) !! v))}
  else if (d_list, 187) !! state_val == 2 then s0 {pos_w = upd ((d_list, 188) !! abs) (pos_w s0) (int_to_float ((d_list, 189) !! v))}
  else if (d_list, 190) !! state_val == 3 then s0 {vel = [upd ((d_list, 191) !! abs) (((vel s0), 192) !! 0) (int_to_float ((d_list, 193) !! v)), upd ((d_list, 194) !! abs) (((vel s0), 195) !! 1) (int_to_float ((d_list, 196) !! (v + 1))), upd ((d_list, 197) !! abs) (((vel s0), 198) !! 2) (int_to_float ((d_list, 199) !! (v + 2)))]}
  else if (d_list, 200) !! state_val == 4 then s0 {rend_mode = (d_list, 201) !! v}
  else if (d_list, 202) !! state_val == 5 then s0 {torch_t0 = (d_list, 203) !! v}
  else s0 {torch_t_limit = (d_list, 204) !! v}

chg_ps1 :: Int -> Int -> Int -> [Int] -> Play_state1 -> Play_state1
chg_ps1 state_val abs v d_list s =
  if (d_list, 205) !! state_val == 0 then s {health = upd ((d_list, 206) !! abs) (health s) ((d_list, 207) !! v), state_chg = 1}
  else if (d_list, 208) !! state_val == 1 then s {ammo = upd ((d_list, 209) !! abs) (ammo s) ((d_list, 210) !! v), state_chg = 2}
  else if (d_list, 211) !! state_val == 2 then s {gems = upd ((d_list, 212) !! abs) (gems s) ((d_list, 213) !! v), state_chg = 3}
  else if (d_list, 214) !! state_val == 3 then s {torches = upd ((d_list, 215) !! abs) (torches s) ((d_list, 216) !! v), state_chg = 4}
  else if (d_list, 217) !! state_val == 4 then s {keys = (take ((d_list, 218) !! abs) (keys s)) ++ [(d_list, 219) !! v] ++ drop (((d_list, 220) !! abs) + 1) (keys s), state_chg = 5}
  else if (d_list, 221) !! state_val == 5 then s {difficulty = ("Hey, not too risky!", 6, 8, 10)}
  else if (d_list, 222) !! state_val == 6 then s {difficulty = ("Plenty of danger please.", 6, 10, 14)}
  else if (d_list, 223) !! state_val == 7 then s {difficulty = ("Ultra danger.", 10, 15, 20)}
  else s {difficulty = ("Health and safety nightmare!", 15, 20, 25)}

copy_ps0 :: Int -> (Int, Int, Int) -> Play_state0 -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
copy_ps0 offset (i0, i1, i2) s0 obj_grid obj_grid_upd d_list =
  let target = obj_grid ! ((d_list, 224) !! i0, (d_list, 225) !! i1, (d_list, 226) !! i2)
      v0 = (offset, fl_to_int (pos_u s0))
      v1 = (offset + 1, fl_to_int (pos_v s0))
      v2 = (offset + 2, fl_to_int (pos_w s0))
      v3 = (offset + 3, fl_to_int (((vel s0), 227) !! 0))
      v4 = (offset + 4, fl_to_int (((vel s0), 228) !! 1))
      v5 = (offset + 5, fl_to_int (((vel s0), 229) !! 2))
      v6 = (offset + 6, angle s0)
      v7 = (offset + 7, rend_mode s0)
      v8 = (offset + 8, fst__ (game_clock s0))
      v9 = (offset + 9, torch_t0 s0)
      v10 = (offset + 10, torch_t_limit s0)      
  in (((d_list, 230) !! i0, (d_list, 231) !! i1, (d_list, 232) !! i2), (fst target, [v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10])) : obj_grid_upd

copy_ps1 :: Int -> (Int, Int, Int) -> Play_state1 -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
copy_ps1 offset (i0, i1, i2) s1 obj_grid obj_grid_upd d_list =
  let target = obj_grid ! ((d_list, 233) !! i0, (d_list, 234) !! i1, (d_list, 235) !! i2)
  in
  (((d_list, 236) !! i0, (d_list, 237) !! i1, (d_list, 238) !! i2), (fst target, [(offset, health s1), (offset + 1, ammo s1), (offset + 2, gems s1), (offset + 3, torches s1), (offset + 4, ((keys s1), 239) !! 0), (offset + 5, ((keys s1), 240) !! 1), (offset + 6, ((keys s1), 241) !! 2), (offset + 7, ((keys s1), 242) !! 3), (offset + 8, ((keys s1), 243) !! 4), (offset + 9, ((keys s1), 244) !! 5)])) : obj_grid_upd

obj_type :: Int -> Int -> Int -> Array (Int, Int, Int) (Int, [Int]) -> Int
obj_type w u v obj_grid =
  if u < 0 || v < 0 then 2
  else if bound_check u 0 (bounds obj_grid) == False then 2
  else if bound_check v 1 (bounds obj_grid) == False then 2
  else fst (obj_grid ! (w, u, v))

copy_lstate :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
copy_lstate offset (i0, i1, i2) (i3, i4, i5) w_grid obj_grid obj_grid_upd d_list =
  let w = ((d_list, 245) !! i3)
      u = ((d_list, 246) !! i4)
      v = ((d_list, 247) !! i5)
      u' = ((d_list, 248) !! i4) - 1
      u'' = ((d_list, 249) !! i4) + 1
      v' = ((d_list, 250) !! i5) - 1
      v'' = ((d_list, 251) !! i5) + 1
      w_conf_u1 = (offset + 9, bool_to_int (u1 (w_grid ! ((d_list, 252) !! i3, (d_list, 253) !! i4, (d_list, 254) !! i5))))
      w_conf_u2 = (offset + 10, bool_to_int (u2 (w_grid ! ((d_list, 255) !! i3, (d_list, 256) !! i4, (d_list, 257) !! i5))))
      w_conf_v1 = (offset + 11, bool_to_int (v1 (w_grid ! ((d_list, 258) !! i3, (d_list, 259) !! i4, (d_list, 260) !! i5))))
      w_conf_v2 = (offset + 12, bool_to_int (v2 (w_grid ! ((d_list, 261) !! i3, (d_list, 262) !! i4, (d_list, 263) !! i5))))
      target = obj_grid ! ((d_list, 264) !! i0, (d_list, 265) !! i1, (d_list, 266) !! i2)
      c0 = (offset, obj_type w u v obj_grid)
      c1 = (offset + 1, obj_type w u' v obj_grid)
      c2 = (offset + 2, obj_type w u'' v obj_grid)
      c3 = (offset + 3, obj_type w u v' obj_grid)
      c4 = (offset + 4, obj_type w u v'' obj_grid)
      c5 = (offset + 5, obj_type w u' v' obj_grid)
      c6 = (offset + 6, obj_type w u'' v' obj_grid)
      c7 = (offset + 7, obj_type w u' v'' obj_grid)
      c8 = (offset + 8, obj_type w u'' v'' obj_grid)
  in (((d_list, 267) !! i0, (d_list, 268) !! i1, (d_list, 269) !! i2), (fst target, [c0, c1, c2, c3, c4, c5, c6, c7, c8, w_conf_u1, w_conf_u2, w_conf_v1, w_conf_v2])) : obj_grid_upd

chg_obj_type :: Int -> (Int, Int, Int) -> [Int] -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
chg_obj_type v (i0, i1, i2) d_list obj_grid obj_grid_upd =
  let target = obj_grid ! ((d_list, 270) !! i0, (d_list, 271) !! i1, (d_list, 272) !! i2)
  in
  (((d_list, 273) !! i0, (d_list, 274) !! i1, (d_list, 275) !! i2), ((d_list, 276) !! v, [])) : obj_grid_upd

pass_msg :: Int -> [Int] -> Play_state1 -> [Int] -> ([Int], Play_state1)
pass_msg len msg s d_list = (drop ((d_list, 277) !! len) msg, s {message = message s ++ [head msg] ++ [((d_list, 278) !! len) - 1] ++ take (((d_list, 279) !! len) - 1) (tail msg)})

send_signal :: Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) (Int, [Int]) -> Play_state1 -> [Int] -> (Array (Int, Int, Int) (Int, [Int]), Play_state1)
send_signal 0 sig (i0, i1, i2) obj_grid s1 d_list =
  let dest = ((d_list, 280) !! i0, (d_list, 281) !! i1, (d_list, 282) !! i2)
      prog = (snd (obj_grid ! dest))
  in (obj_grid, s1 {next_sig_q = [(d_list, 283) !! sig, (d_list, 284) !! i0, (d_list, 285) !! i1, (d_list, 286) !! i2] ++ next_sig_q s1})
send_signal 1 sig dest obj_grid s1 d_list =
  let prog = (snd (obj_grid ! dest))
  in
  (obj_grid // [(dest, (fst (obj_grid ! dest), (head prog) : sig : drop 2 prog))], s1)

project_init :: Int -> Int -> Int -> Int -> Int -> (Int, Int, Int) -> (Int, Int, Int) -> Int -> Int -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> UArray (Int, Int) Float -> [((Int, Int, Int), (Int, [(Int, Int)]))]
project_init u v w a vel (i0, i1, i2) (i3, i4, i5) offset obj_flag obj_grid obj_grid_upd d_list look_up =
  let source = ((d_list, 288) !! i0, (d_list, 289) !! i1, (d_list, 290) !! i2)
      dest = ((d_list, 291) !! i3, (d_list, 292) !! i4, (d_list, 293) !! i5)
      target = obj_grid ! source
      v0 = (offset, (d_list, 294) !! i3)
      v1 = (offset + 1, (d_list, 295) !! i4)
      v2 = (offset + 2, (d_list, 296) !! i5)
      v3 = (offset + 3, (d_list, 297) !! u)
      v4 = (offset + 4, (d_list, 298) !! v)
      v5 = (offset + 5, - (div ((d_list, 299) !! w) 1000000) - 1)
      v6 = (offset + 6, truncate ((look_up ! (2, (d_list, 300) !! a)) * fromIntegral ((d_list, 301) !! vel)))
      v7 = (offset + 7, truncate ((look_up ! (1, (d_list, 302) !! a)) * fromIntegral ((d_list, 303) !! vel)))
      v8 = (offset + 8, div ((d_list, 304) !! u) 1000000)
      v9 = (offset + 9, div ((d_list, 305) !! v) 1000000)
      v10 = (offset + 10, (d_list, 306) !! obj_flag)
      v11 = (offset + 24, ((d_list, 631) !! w))
  in (source, (-3, [])) : (dest, (-3, [v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11])) : obj_grid_upd

project_update :: Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> [Int] -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
project_update p_state p_state' (i0, i1, i2) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 d_list =
  let location = ((d_list, 307) !! i0, (d_list, 308) !! i1, (d_list, 309) !! i2)
      target = obj_grid ! location
      u = (d_list, 310) !! p_state
      v = (d_list, 311) !! (p_state + 1)
      vel_u = (d_list, 312) !! (p_state + 3)
      vel_v = (d_list, 313) !! (p_state + 4)
      u' = u + vel_u
      v' = v + vel_v
      u_block = div u 1000000
      v_block = div v 1000000
      u_block' = div u' 1000000
      v_block' = div v' 1000000
      w_block = (d_list, 314) !! (p_state + 2)
      w_block_ = (- w_block) - 1
      index = (w_block, u_block, v_block)
      grid_i = fromJust (obj (w_grid ! index))
      grid_i' = (obj (w_grid ! index))
      s1_ = \index -> snd (sub_i 8 obj_grid index (i0, i1, i2) s1)
  in
  if isNothing grid_i' == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
  else if ident_ grid_i /= 128 then (w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block), (p_state' + 11, v_block)])) : obj_grid_upd, s1)
  else if u_block' == u_block && v_block' == v_block then ((index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}) : w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1_ (w_block_, u_block, v_block))
  else if u_block' == u_block && v_block' == v_block + 1 then
    if v2 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (fst (sub_i 0 obj_grid (w_block_, u_block, v_block + 1) (i0, i1, i2) s1)) > 0 && fst (fst (sub_i 0 obj_grid (w_block_, u_block, v_block + 1) (i0, i1, i2) s1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block), (p_state' + 11, v_block + 1)])) : obj_grid_upd, s1_ (w_block_, u_block, v_block + 1))
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block, v_block + 1) && (d_list, 315) !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block, v_block + 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block + 1 && v_block' == v_block then
    if u2 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (fst (sub_i 1 obj_grid (w_block_, u_block + 1, v_block) (i0, i1, i2) s1)) > 0 && fst (fst (sub_i 1 obj_grid (w_block_, u_block + 1, v_block) (i0, i1, i2) s1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block + 1), (p_state' + 11, v_block)])) : obj_grid_upd, s1_ (w_block_, u_block + 1, v_block))
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block + 1, v_block) && (d_list, 316) !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block + 1, v_block), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block && v_block' == v_block - 1 then
    if v1 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (fst (sub_i 2 obj_grid (w_block_, u_block, v_block - 1) (i0, i1, i2) s1)) > 0 && fst (fst (sub_i 2 obj_grid (w_block_, u_block, v_block - 1) (i0, i1, i2) s1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block), (p_state' + 11, v_block - 1)])) : obj_grid_upd, s1_ (w_block_, u_block, v_block - 1))
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block, v_block - 1) && (d_list, 317) !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block, v_block - 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block - 1 && v_block' == v_block then
    if u1 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (fst (sub_i 3 obj_grid (w_block_, u_block - 1, v_block) (i0, i1, i2) s1)) > 0 && fst (fst (sub_i 3 obj_grid (w_block_, u_block - 1, v_block) (i0, i1, i2) s1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block - 1), (p_state' + 11, v_block)])) : obj_grid_upd, s1_ (w_block_, u_block - 1, v_block))
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block - 1, v_block) && (d_list, 318) !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block - 1, v_block), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block + 1 && v_block' == v_block + 1 then
    if u2 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (fst (sub_i 4 obj_grid (w_block_, u_block + 1, v_block + 1) (i0, i1, i2) s1)) > 0 && fst (fst (sub_i 4 obj_grid (w_block_, u_block + 1, v_block + 1) (i0, i1, i2) s1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block + 1), (p_state' + 11, v_block + 1)])) : obj_grid_upd, s1_ (w_block_, u_block + 1, v_block + 1))
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block + 1, v_block + 1) && (d_list, 319) !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block + 1, v_block + 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block + 1 && v_block' == v_block - 1 then
    if v1 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (fst (sub_i 5 obj_grid (w_block_, u_block + 1, v_block - 1) (i0, i1, i2) s1)) > 0 && fst (fst (sub_i 5 obj_grid (w_block_, u_block + 1, v_block - 1) (i0, i1, i2) s1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block + 1), (p_state' + 11, v_block - 1)])) : obj_grid_upd, s1_ (w_block_, u_block + 1, v_block - 1))
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block + 1, v_block - 1) && (d_list, 320) !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block + 1, v_block - 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block - 1 && v_block' == v_block - 1 then
    if u1 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (fst (sub_i 6 obj_grid (w_block_, u_block - 1, v_block - 1) (i0, i1, i2) s1)) > 0 && fst (fst (sub_i 6 obj_grid (w_block_, u_block - 1, v_block - 1) (i0, i1, i2) s1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block - 1), (p_state' + 11, v_block - 1)])) : obj_grid_upd, s1_ (w_block_, u_block - 1, v_block - 1))
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block - 1, v_block - 1) && (d_list, 321) !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block - 1, v_block - 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else
    if v2 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (fst (sub_i 7 obj_grid (w_block_, u_block - 1, v_block + 1) (i0, i1, i2) s1)) > 0 && fst (fst (sub_i 7 obj_grid (w_block_, u_block - 1, v_block + 1) (i0, i1, i2) s1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block - 1), (p_state' + 11, v_block + 1)])) : obj_grid_upd, s1_ (w_block_, u_block - 1, v_block + 1))
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block - 1, v_block + 1) && (d_list, 322) !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block - 1, v_block + 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)

-- Called from project_update, npc_move and npc_damage.  Used to determine the damage taken by the player and non - player characters from adverse events.
det_damage :: ([Char], Int, Int, Int) -> Play_state0 -> Int
det_damage (d, low, med, high) s0 =
  if (prob_seq s0) ! (mod (fst__ (game_clock s0)) 240) < 20 then low
  else if (prob_seq s0) ! (mod (fst__ (game_clock s0)) 240) > 70 then high
  else med

binary_dice :: Int -> Int -> (Int, Int, Int) -> Int -> Play_state0 -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
binary_dice prob diff (i0, i1, i2) offset s0 obj_grid obj_grid_upd d_list =
  let target = obj_grid ! ((d_list, 323) !! i0, (d_list, 324) !! i1, (d_list, 325) !! i2)
  in
  if (prob_seq s0) ! (mod (fst__ (game_clock s0) + ((d_list, 326) !! diff)) 240) < (d_list, 327) !! prob then (((d_list, 328) !! i0, (d_list, 329) !! i1, (d_list, 330) !! i2), (fst target, [(offset, 1)])) : obj_grid_upd
  else (((d_list, 331) !! i0, (d_list, 332) !! i1, (d_list, 333) !! i2), (fst target, [(offset, 0)])) : obj_grid_upd

binary_dice_ :: Int -> Play_state0 -> Bool
binary_dice_ prob s0 =
  if (prob_seq s0) ! (mod (fst__ (game_clock s0)) 240) < prob then True
  else False  

-- The GPLC op - codes init_npc, npc_damage, npc_decision, npc_move and cpede_move form the entry point for scripts to drive non - player character (NPC) behaviour.
-- As these op - codes are responsible for more complex state changes than the others, their entry point functions call a substantial number of supporting functions.
-- There are two NPC behavioural models, namely type 1 and type 2 (also known as centipedes).
init_npc :: Int -> Int -> Play_state1 -> [Int] -> Play_state1
init_npc offset i s1 d_list =
  let char_state = (npc_states s1) ! i
      i_npc_type = (d_list, 334) !! offset
      i_c_health = (d_list, 335) !! (offset + 1)
      i_node_locations = take 6 (drop (offset + 2) d_list)
      i_fg_position = int_to_float_v ((d_list, 336) !! (offset + 8)) ((d_list, 337) !! (offset + 9)) ((d_list, 338) !! (offset + 10))
      i_dir_vector = (int_to_float ((d_list, 339) !! (offset + 11)), int_to_float ((d_list, 340) !! (offset + 12)))
      i_direction = (d_list, 341) !! (offset + 13)
      i_last_dir = (d_list, 342) !! (offset + 14)
      i_speed = int_to_float ((d_list, 343) !! (offset + 15))
      i_avoid_dist = (d_list, 344) !! (offset + 16)
      i_attack_mode = int_to_bool ((d_list, 345) !! (offset + 17))
      i_fire_prob = (d_list, 346) !! (offset + 18)
      i_dir_list = take 6 (drop (offset + 19) d_list)
      i_node_num = (d_list, 347) !! (offset + 25)
      i_end_node = (d_list, 348) !! (offset + 26)
      i_head_index = (d_list, 349) !! (offset + 27)
  in 
  s1 {npc_states = (npc_states s1) // [((d_list, 350) !! i, char_state {npc_type = i_npc_type, c_health = i_c_health, node_locations = i_node_locations,
      fg_position = i_fg_position, dir_vector = i_dir_vector, direction = i_direction, last_dir = i_last_dir, speed = i_speed, avoid_dist = i_avoid_dist,
      attack_mode = i_attack_mode, fire_prob = i_fire_prob, dir_list = i_dir_list, node_num = i_node_num, end_node = i_end_node, head_index = i_head_index})]}

-- Used to determine a pseudorandom target destination for an NPC, so it can wander around when not set on attacking the player.
det_rand_target :: Play_state0 -> Int -> Int -> (Int, Int, Int)
det_rand_target s0 u_bound v_bound =
  let n = \i -> (prob_seq s0) ! (mod (fst__ (game_clock s0) + i) 240)
  in (mod (n 0) 2, mod (((n 1) + 1) * ((n 2) + 1)) u_bound, mod (((n 1) + 1) * ((n 2) + 1)) v_bound)

-- The NPC path finding is based around line of sight checks, which use the Obj_grid ( (Int, [Int]) ) instance of the ray tracer.
chk_line_sight :: Int -> Int -> Int -> Int -> Int -> (Float, Float) -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> UArray (Int, Int) Float -> Int
chk_line_sight mode a w_block u_block v_block (fg_u, fg_v) target_u target_v w_grid f_grid obj_grid look_up =
  let proc_angle_ = proc_angle a
  in
  third_ (ray_trace0 fg_u fg_v (fst__ proc_angle_) (snd__ proc_angle_) (third_ proc_angle_) u_block v_block w_grid f_grid obj_grid look_up w_block [] target_u target_v mode 1)

-- Type 1 NPCs can move in 8 directions and type 2 in a subset of 4 of these.  This function maps these directions (encoded as 1 - 8) to centiradians, which is the
-- angle representation used elsewhere in the engine.
npc_dir_table :: Bool -> Int -> Int
npc_dir_table shift_flag dir =
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
another_dir :: Bool -> [Int] -> Int -> Int -> Int -> Int -> (Float, Float) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> UArray (Int, Int) Float -> Play_state0 -> Int
another_dir shift_flag [] c w_block u_block v_block (fg_u, fg_v) w_grid f_grid obj_grid look_up s0 = 0
another_dir shift_flag poss_dirs c w_block u_block v_block (fg_u, fg_v) w_grid f_grid obj_grid look_up s0 =
  let choice = (poss_dirs, 351) !! (mod ((prob_seq s0) ! (mod (fst__ (game_clock s0) + c) 240)) c)
  in
  if chk_line_sight 1 (npc_dir_table shift_flag choice) w_block u_block v_block (fg_u, fg_v) 0 0 w_grid f_grid obj_grid look_up == 0 then choice
  else another_dir shift_flag (delete choice poss_dirs) (c - 1) w_block u_block v_block (fg_u, fg_v) w_grid f_grid obj_grid look_up s0

-- This function is involved in implementing the restriction to the number of possible directions for an NPC.
quantise_angle :: Int -> (Int, Int)
quantise_angle a =
  if a < 79 then (6, 1)
  else if a < 157 then (85, 2)
  else if a < 236 then (163, 3)
  else if a < 314 then (239, 4)
  else if a < 393 then (320, 5)
  else if a < 471 then (399, 6)
  else if a < 550 then (477, 7)
  else (556, 8)

-- Type 2 (centipede) NPCs can't reverse so that they don't trample their own tails (but see the explanation of cpede_head_swap below as it relates to this issue).
-- This function is involved in implementing that restriction.
cpede_reverse 1 = 5
cpede_reverse 3 = 7
cpede_reverse 5 = 1
cpede_reverse 7 = 3

-- This function contains the decision logic specific to the type 2 (centipede) behavioural model and is called from the more general npc_decision function.
cpede_decision :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> (Int, Bool)
cpede_decision 0 choice i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up =
  let char_state = (npc_states s1) ! i
  in
  if target_u == u && target_v > v then
    if direction char_state == 7 then cpede_decision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
  else if target_u == u && target_v < v then
    if direction char_state == 3 then cpede_decision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
  else if target_v == v && target_u > u then
    if direction char_state == 5 then cpede_decision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
  else if target_v == v && target_u < u then
    if direction char_state == 1 then cpede_decision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
  else if abs (target_u - u) <= abs (target_v - v) && target_u - u > 0 then
    if direction char_state == 5 then
      if target_v - v > 0 then cpede_decision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
      else cpede_decision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
  else if abs (target_u - u) <= abs (target_v - v) && target_u - u < 0 then
    if direction char_state == 1 then
      if target_v - v > 0 then cpede_decision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
      else cpede_decision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
  else if abs (target_u - u) >= abs (target_v - v) && target_v - v > 0 then
    if direction char_state == 7 then
      if target_u - u > 0 then cpede_decision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
      else cpede_decision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
  else
    if direction char_state == 3 then
      if target_u - u < 0 then cpede_decision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
      else cpede_decision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
cpede_decision 1 choice i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up =
  let char_state = (npc_states s1) ! i
      line_sight = chk_line_sight 2 (npc_dir_table False choice) w u v ((fromIntegral u) + 0.5, (fromIntegral v) + 0.5) target_u target_v w_grid f_grid obj_grid look_up
  in
  if line_sight == 0 then (choice, True && attack_mode char_state)
  else if line_sight > avoid_dist char_state then (choice, False)
  else (another_dir False (delete (cpede_reverse choice) (delete choice [1, 3, 5, 7])) 2 w u v ((fromIntegral u) + 0.5, (fromIntegral v) + 0.5) w_grid f_grid obj_grid look_up s0, False)

-- Updates the list of centipede segment directions so that the tail follows the direction the head has taken.
upd_dir_list :: Int -> [Int] -> [Int]
upd_dir_list dir (x0:x1:x2:x3:x4:x5:xs) = [dir, x0, x1, x2, x3, x4]

-- Used in the NPC path finding to compute an angle of approach to a target from the components of an approach vector.
vector_to_angle :: Float -> Float -> Int
vector_to_angle u_comp v_comp =
  let a = truncate ((atan (abs v_comp / abs u_comp)) * 100)
  in
  if u_comp >= 0 && v_comp >= 0 then a
  else if u_comp < 0 && v_comp >= 0 then 314 - a
  else if u_comp < 0 && v_comp < 0 then 314 + a
  else 628 - a

shift_fireball_pos :: Int -> Float -> Float -> (Float, Float)
shift_fireball_pos dir u v =
  if dir == 1 then (u + 1.1, v)
  else if dir == 2 then (u + 1.1, v + 1.1)
  else if dir == 3 then (u, v + 1.1)
  else if dir == 4 then (u - 1.1, v + 1.1)
  else if dir == 5 then (u - 1.1, v)
  else if dir == 6 then (u - 1.1, v - 1.1)
  else if dir == 7 then (u, v - 1.1)
  else (u + 1.1, v - 1.1)

npc_decision :: Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> ([((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
npc_decision 0 flag offset target_w target_u target_v d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 look_up =
  let char_state = (npc_states s1) ! ((d_list, 352) !! 8)
      s1' = \t0 t1 w' u' v' -> s1 {npc_states = (npc_states s1) // [((d_list, 353) !! 8, char_state {ticks_left0 = t0, ticks_left1 = t1, target_w' = w', target_u' = u', target_v' = v'})]}
      s1'' = \t1 u' v' f -> s1 {npc_states = (npc_states s1) // [((d_list, 354) !! 8, char_state {ticks_left1 = t1, target_u' = u', target_v' = v', final_appr = f})]}
      rand_target = det_rand_target s0 (snd__ (snd (bounds w_grid))) (third_ (snd (bounds w_grid)))
      fg_pos = fg_position char_state
  in
  if npc_type char_state == 2 && ticks_left0 char_state == 0 then
    if attack_mode char_state == True && (truncate (pos_w s0)) == w then npc_decision 3 1 offset (truncate (pos_w s0)) (truncate (pos_u s0)) (truncate (pos_v s0)) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1 {npc_states = (npc_states s1) // [((d_list, 355) !! 8, char_state {final_appr = True})]}) look_up
    else if ticks_left1 char_state == 0 || (u == target_u' char_state && v == target_v' char_state) then npc_decision 3 1 offset w (snd__ rand_target) (third_ rand_target) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1'' 1000 (snd__ rand_target) (third_ rand_target) False) look_up
    else npc_decision 3 1 offset w (target_u' char_state) (target_v' char_state) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1'' ((ticks_left1 char_state) - 1) target_u target_v False) look_up
  else if npc_type char_state < 2 && (u /= truncate (snd__ fg_pos + fst (dir_vector char_state)) || v /= truncate (third_ fg_pos + snd (dir_vector char_state))) then
    if attack_mode char_state == True then npc_decision 1 0 offset (truncate (pos_w s0)) (truncate (pos_u s0)) (truncate (pos_v s0)) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1' 0 0 0 0 0) look_up
    else if ticks_left1 char_state < 1 || (w == target_w' char_state && u == target_u' char_state && v == target_v' char_state) then npc_decision 1 0 offset (fst__ rand_target) (snd__ rand_target) (third_ rand_target) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1' 0 1000 (fst__ rand_target) (snd__ rand_target) (third_ rand_target)) look_up
    else npc_decision 1 0 offset (target_w' char_state) (target_u' char_state) (target_v' char_state) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1' 0 ((ticks_left1 char_state) - 1) (target_w' char_state) (target_u' char_state) (target_v' char_state)) look_up
  else if npc_type char_state < 2 then (obj_grid_upd, s1' 1 ((ticks_left1 char_state) - 1) (target_w' char_state) (target_u' char_state) (target_v' char_state))
  else (obj_grid_upd, s1' (ticks_left0 char_state) ((ticks_left1 char_state) - 1) (target_w' char_state) (target_u' char_state) (target_v' char_state))
npc_decision 1 flag offset target_w target_u target_v d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 look_up =
  let char_state = (npc_states s1) ! ((d_list, 356) !! 8)
      down_ramp = local_down_ramp (f_grid ! (w, div u 2, div v 2))
      up_ramp = local_up_ramp (f_grid ! (w, div u 2, div v 2))
  in
  if w == target_w then npc_decision (2 + flag) flag offset target_w target_u target_v d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1 {npc_states = (npc_states s1) // [((d_list, 357) !! 8, char_state {final_appr = True})]}) look_up
  else if w > target_w then npc_decision (2 + flag) flag offset w ((fst down_ramp) * 2) ((snd down_ramp) * 2) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1 {npc_states = (npc_states s1) // [((d_list, 358) !! 8, char_state {final_appr = False})]}) look_up
  else npc_decision (2 + flag) flag offset w ((fst up_ramp) * 2) ((snd up_ramp) * 2) d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1 {npc_states = (npc_states s1) // [((d_list, 359) !! 8, char_state {final_appr = False})]}) look_up
npc_decision 2 flag offset target_w target_u target_v d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 look_up =
  let char_state = (npc_states s1) ! ((d_list, 360) !! 8)
      fg_pos = fg_position char_state
      a = vector_to_angle (((fromIntegral target_u) + 0.5) - snd__ fg_pos) (((fromIntegral target_v) + 0.5) - third_ fg_pos)
      qa = quantise_angle a
      prog = obj_grid ! (w, u, v)
      line_sight0 = chk_line_sight 2 (fst qa) w u v (snd__ (fg_position char_state), third_ (fg_position char_state)) target_u target_v w_grid f_grid obj_grid look_up
      line_sight1 = chk_line_sight 3 (fst qa) w u v (snd__ (fg_position char_state), third_ (fg_position char_state)) target_u target_v w_grid f_grid obj_grid look_up
      another_dir_ = another_dir True (delete (snd qa) [1..8]) 7 w u v (snd__ fg_pos, third_ fg_pos) w_grid f_grid obj_grid look_up s0
      fb_pos = shift_fireball_pos (snd qa) (snd__ fg_pos) (third_ fg_pos)
  in
  if final_appr char_state == True then
    if line_sight0 > avoid_dist char_state || line_sight0 == 0 then
      if (prob_seq s0) ! (mod (fst__ (game_clock s0)) 240) < fire_prob char_state && attack_mode char_state == True && npc_type ((npc_states s1) ! 127) /= fst__ (game_clock s0) then
        (obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 361) !! 8, char_state {direction = snd qa, last_dir = snd qa, fireball_state = [(offset, 1), (offset + 1, fl_to_int (fst__ fg_pos)), (offset + 2, fl_to_int (fst fb_pos)), (offset + 3, fl_to_int (snd fb_pos)), (offset + 4, a)]}), (127, def_npc_state {npc_type = (fst__ (game_clock s0))})]})
      else (obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 362) !! 8, char_state {direction = snd qa, last_dir = snd qa})]})
    else (obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 364) !! 8, char_state {direction = another_dir_, last_dir = another_dir_})]})
  else
    if line_sight1 < 0 then (obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 365) !! 8, char_state {direction = line_sight1, last_dir = direction char_state})]})
    else if line_sight1 == 0 then (obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 366) !! 8, char_state {direction = snd qa, last_dir = snd qa})]})
    else if line_sight1 > avoid_dist char_state then (obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 367) !! 8, char_state {direction = snd qa, last_dir = snd qa})]})
    else (obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 368) !! 8, char_state {direction = another_dir_, last_dir = another_dir_})]})
npc_decision 3 flag offset target_w target_u target_v d_list (w:u:v:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 look_up =
  let char_state = (npc_states s1) ! ((d_list, 369) !! 8)
      choice = cpede_decision 0 0 ((d_list, 370) !! 8) target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
      prog = obj_grid ! (w, u, v)
      fg_pos = fg_position char_state
  in
  if snd choice == True then
    if (prob_seq s0) ! (mod (fst__ (game_clock s0)) 240) < fire_prob char_state then
      (((w, u, v), (fst prog, [(offset, 1), (offset + 1, fl_to_int (fst__ fg_pos)), (offset + 2, fl_to_int (snd__ fg_pos)), (offset + 3, fl_to_int (third_ fg_pos)), (offset + 4, npc_dir_table False (fst choice))])) : obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 371) !! 8, char_state {direction = fst choice})]})
    else (obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 372) !! 8, char_state {direction = fst choice})]})
  else (obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 373) !! 8, char_state {direction = fst choice})]})

det_dir_vector :: Int -> Float -> UArray (Int, Int) Float -> (Float, Float)
det_dir_vector dir speed look_up =
  let dir' = npc_dir_table True dir
  in
  if dir == 0 then (0, 0)
  else (speed * look_up ! (2, dir'), speed * look_up ! (1, dir'))

char_rotation :: Int -> Int -> Int -> Int
char_rotation 0 dir base_id = base_id + ((dir - 1) * 2)
char_rotation 1 0 base_id = base_id
char_rotation 1 1 base_id = base_id + 6
char_rotation 1 3 base_id = base_id
char_rotation 1 5 base_id = base_id + 2
char_rotation 1 7 base_id = base_id + 4

add_vel_pos (fg_w, fg_u, fg_v) (vel_u, vel_v) = (fg_w, fg_u + vel_u, fg_v + vel_v)

ramp_climb :: Int -> Int -> (Float, Float, Float) -> (Float, Float, Float)
ramp_climb dir c (fg_w, fg_u, fg_v) =
  if dir == -1 then (fg_w - 0.025 * fromIntegral c, fg_u - 0.05 * (fromIntegral c), fg_v)
  else if dir == -2 then (fg_w - 0.025 * fromIntegral c, fg_u + 0.05 * (fromIntegral c), fg_v)
  else if dir == -3 then (fg_w - 0.025 * fromIntegral c, fg_u, fg_v - 0.05 * (fromIntegral c))
  else if dir == -4 then (fg_w - 0.025 * fromIntegral c, fg_u, fg_v + 0.05 * (fromIntegral c))
  else if dir == -5 then (fg_w + 0.025 * fromIntegral c, fg_u + 0.05 * (fromIntegral c), fg_v)
  else if dir == -6 then (fg_w + 0.025 * fromIntegral c, fg_u - 0.05 * (fromIntegral c), fg_v)
  else if dir == -7 then (fg_w + 0.025 * fromIntegral c, fg_u, fg_v + 0.05 * (fromIntegral c))
  else (fg_w + 0.025 * fromIntegral c, fg_u, fg_v - 0.05 * (fromIntegral c))

ramp_fill :: Int -> Int -> Int -> Int -> a -> Terrain -> [((Int, Int, Int), a)]
ramp_fill w u v dir x t =
  let fill_u = if div u 2 == div (u + 1) 2 then (u + 1, 1)
               else (u - 1, -1)
      fill_v = if div v 2 == div (v + 1) 2 then (v + 1, 1)
               else (v - 1, -1)
      end_point = if dir == -1 || dir == -6 then (w, u - 2, v)
                  else if dir == -2 || dir == -5 then (w, u + 2, v)
                  else if dir == -3 || dir == -8 then (w, u, v - 1)
                  else (w, u, v + 1)
  in [((w, u, v), x), ((w, fst fill_u, v), x), ((w, u, fst fill_v), x), ((w, fst fill_u, fst fill_v), x), (end_point, x)]

conv_ramp_fill :: Int -> Int -> Int -> Int -> Int -> Terrain -> [Int]
conv_ramp_fill w u v dw dir t =
  let r = last (ramp_fill (w + dw) u v dir 0 t)
  in [fst__ (fst r), snd__ (fst r), third_ (fst r)]

adjust_fg_position :: Int -> Int -> Int -> Int -> (Float, Float, Float)
adjust_fg_position w u v dir =
  if dir == -2 || dir == -5 then ((fromIntegral w) + 0.1, (fromIntegral u) + 1, (fromIntegral v) + 0.5)
  else if dir == -1 || dir == -6 then ((fromIntegral w) + 0.1, fromIntegral u, (fromIntegral v) + 0.5)
  else if dir == -4 || dir == -7 then ((fromIntegral w) + 0.1, (fromIntegral u) + 0.5, (fromIntegral v) + 1)
  else ((fromIntegral w) + 0.1, (fromIntegral u) + 0.5, fromIntegral v)

npc_move :: Int -> [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
npc_move offset d_list (w:u:v:w1:u1:v1:blocks) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up =
  let char_state = (npc_states s1) ! ((d_list, 374) !! 8)
      u' = truncate ((snd__ (fg_position char_state)) + fst dir_vector')
      v' = truncate ((third_ (fg_position char_state)) + snd dir_vector')
      u'' = \x -> if x == 0 then fst (local_up_ramp (f_grid ! (w, div u 2, div v 2))) * 2
                  else fst (local_down_ramp (f_grid ! (w, div u 2, div v 2))) * 2
      v'' = \x -> if x == 0 then snd (local_up_ramp (f_grid ! (w, div u 2, div v 2))) * 2
                  else snd (local_down_ramp (f_grid ! (w, div u 2, div v 2))) * 2
      o_target = fromMaybe def_obj_place (obj (w_grid ! (-w - 1, u, v)))
      o_target_ = fromMaybe def_obj_place (obj (w_grid ! (- 1, 0, 2)))
      prog = obj_grid ! (w, u, v)
      prog' = \x y -> (fst prog, take offset (snd prog) ++ [x, y] ++ drop (offset + 2) (snd prog))
      ramp_fill' = \dw -> last (ramp_fill (w + dw) u v (direction char_state) (-2, [(offset, 0)]) (surface (f_grid ! (w, div u 2, div v 2))))
      dir_vector' = det_dir_vector (last_dir char_state) (speed char_state) look_up
      ramp_climb_ = ramp_climb (direction char_state) (41 - ticks_left0 char_state) (fg_position char_state)
      conv_ramp_fill0 = conv_ramp_fill w u v 1 (direction char_state) (surface (f_grid ! (w, div u 2, div v 2)))
      conv_ramp_fill1 = conv_ramp_fill w u v 0 (direction char_state) (surface (f_grid ! (w, div u 2, div v 2)))
      w_grid' = ((-w - 1, u, v), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {u__ = snd__ (fg_position char_state), v__ = third_ (fg_position char_state)})})
      w_grid'' = [((-w - 1, u', v'), (w_grid ! (- 1, 0, 2)) {obj = Just (o_target_ {ident_ = char_rotation 0 (direction char_state) ((d_list, 375) !! 9), w__ = fst__ (fg_position char_state), u__ = snd__ (fg_position char_state), v__ = third_ (fg_position char_state)})}), ((-w - 1, u, v), def_w_grid)]
      w_grid''' = ((-w - 1, u, v), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {w__ = fst__ ramp_climb_, u__ = snd__ ramp_climb_, v__ = third_ ramp_climb_})})
      w_grid_4 = \dw -> ([((-w - 1, u, v), def_w_grid)] ++ drop 4 (ramp_fill (-w - 1 + dw) u v (direction char_state) ((w_grid ! (-w - 1, u, v)) {obj = Just (o_target {w__ = fst__ ramp_climb_, u__ = snd__ ramp_climb_, v__ = third_ ramp_climb_})}) (surface (f_grid ! (w, div u 2, div v 2)))))
      damage = det_damage (difficulty s1) s0
  in
  if ticks_left0 char_state == 0 then
    if (w, u', v') == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then
      if attack_mode char_state == True && binary_dice_ 10 s0 == True then
        if health s1 - damage <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg8})
        else (w_grid_upd, obj_grid_upd, s1 {health = health s1 - damage, message = 0 : msg25 ++ message s1, next_sig_q = [129, w, u, v] ++ next_sig_q s1})
      else (w_grid_upd, obj_grid_upd, s1 {next_sig_q = [129, w, u, v] ++ next_sig_q s1})
    else if direction char_state >= 0 then
      if isNothing (obj (w_grid ! (-w - 1, u', v'))) == True || (u, v) == (u', v') then
        (w_grid'' ++ w_grid_upd, ((w, u, v), (-2, [])) : ((w, u', v'), (-2, [(offset - 10, w), (offset - 9, u'), (offset - 8, v')] ++ fireball_state char_state)) : obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 376) !! 8, char_state {dir_vector = dir_vector', fg_position = add_vel_pos (fg_position char_state) dir_vector', node_locations = [w, u', v', 0, 0, 0], ticks_left0 = 1, fireball_state = []})], next_sig_q = [129, w, u', v'] ++ next_sig_q s1})
      else (w_grid_upd, obj_grid_upd, s1 {next_sig_q = [129, w, u, v] ++ next_sig_q s1})
    else if direction char_state < -4 then ([((-w - 1, u, v), def_w_grid), ((-w - 1, u'' 0, v'' 0), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {ident_ = char_rotation 0 (npc_dir_remap (direction char_state)) ((d_list, 377) !! 9)})})] ++ w_grid_upd, (take 4 (ramp_fill w (u'' 0) (v'' 0) (direction char_state) (2, []) (surface (f_grid ! (w, div (u'' 0) 2, div (v'' 0) 2))))) ++ [((w, u, v), (-2, [])), ((w, u'' 0, v'' 0), (-2, [(offset, 1)]))] ++ obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 378) !! 8, char_state {node_locations = [w, u'' 0, v'' 0, 0, 0, 0], fg_position = adjust_fg_position w (u'' 0) (v'' 0) (direction char_state), ticks_left0 = 41})], next_sig_q = [129, w, u'' 0, v'' 0] ++ next_sig_q s1})
    else
      if (w - 1, u', v') == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then (w_grid_upd, obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 379) !! 8, char_state {direction = (last_dir char_state)})], next_sig_q = [129, w, u, v] ++ next_sig_q s1})
      else ([((-w - 1, u, v), def_w_grid), ((-w, (u'' 1), (v'' 1)), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {ident_ = char_rotation 0 (npc_dir_remap (direction char_state)) ((d_list, 380) !! 9)})})] ++ w_grid_upd, (take 4 (ramp_fill (w - 1) (u'' 1) (v'' 1) (direction char_state) (2, []) (surface (f_grid ! (w - 1, div (u'' 1) 2, div (v'' 1) 2))))) ++ [((w, u, v), (-2, [])), ((w - 1, u'' 1, v'' 1), (-2, [(offset, 1)]))] ++ obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 381) !! 8, char_state {node_locations = [w - 1, u'' 1, v'' 1, 0, 0, 0], fg_position = adjust_fg_position w (u'' 1) (v'' 1) (direction char_state), ticks_left0 = 41})], next_sig_q = [129, w - 1, u'' 1, v'' 1] ++ next_sig_q s1})
  else if ticks_left0 char_state == 1 then
    if direction char_state >= 0 then (w_grid' : w_grid_upd, obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 382) !! 8, char_state {fg_position = add_vel_pos (fg_position char_state) (dir_vector char_state)})], next_sig_q = [129, w, u, v] ++ next_sig_q s1})
    else if direction char_state < -4 then
      if (w + 1, (conv_ramp_fill0, 383) !! 1, (conv_ramp_fill0, 384) !! 2) == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then (w_grid_upd, obj_grid_upd, s1 {next_sig_q = [129, w, u, v] ++ next_sig_q s1})
      else if fst (obj_grid ! (w + 1, (conv_ramp_fill0, 385) !! 1, (conv_ramp_fill0, 386) !! 2)) > 0 then (w_grid_upd, obj_grid_upd, s1 {next_sig_q = [129, w, u, v] ++ next_sig_q s1})
      else (w_grid_4 (-1) ++ w_grid_upd, take 4 (ramp_fill w u v (direction char_state) (0, []) (surface (f_grid ! (w, div u 2, div v 2)))) ++ [((w, u, v), (-2, [])), ramp_fill' 1] ++ obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 387) !! 8, char_state {node_locations = conv_ramp_fill0 ++ [0, 0, 0], fg_position = ramp_climb_, direction = npc_dir_remap (direction char_state), ticks_left0 = 1})], next_sig_q = [129] ++ conv_ramp_fill0 ++ next_sig_q s1})
    else
      if (w, (conv_ramp_fill1, 388) !! 1, (conv_ramp_fill1, 389) !! 2) == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then (w_grid_upd, obj_grid_upd, s1 {next_sig_q = [129, w, u, v] ++ next_sig_q s1})
      else if fst (obj_grid ! (w, (conv_ramp_fill1, 390) !! 1, (conv_ramp_fill1, 391) !! 2)) > 0 then (w_grid_upd, obj_grid_upd, s1 {next_sig_q = [129, w, u, v] ++ next_sig_q s1})
      else (w_grid_4 0 ++ w_grid_upd, take 4 (ramp_fill w u v (direction char_state) (0, []) (surface (f_grid ! (w, div u 2, div v 2)))) ++ [((w, u, v), (-2, [])), ramp_fill' 0] ++ obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 392) !! 8, char_state {node_locations = conv_ramp_fill1 ++ [0, 0, 0], fg_position = ramp_climb_, direction = npc_dir_remap (direction char_state), ticks_left0 = 1})], next_sig_q = [129] ++ conv_ramp_fill1 ++ next_sig_q s1})
  else if ticks_left0 char_state > 1 then (w_grid''' : w_grid_upd, obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 393) !! 8, char_state {ticks_left0 = ticks_left0 char_state - 1})], next_sig_q = [129, w, u, v] ++ next_sig_q s1})
  else throw NPC_feature_not_implemented

-- The centipede NPCs have a modular design whereby a separate GPLC script drives each node, or centipede segment.  Only the head node calls npc_decision but all
-- nodes call cpede_move.  A signal relay is formed in that signals sent by the head propagate along the tail and drive script runs and thereby movement.  The three
-- functions below are to support cpede_move with segment movement, signal propagation and animation respectively.
cpede_pos :: Int -> Int -> Int -> Int -> Bool -> ((Int, Int), (Float, Float))
cpede_pos u v dir t reversed =
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

cpede_sig_check :: [Int] -> Int -> Int -> [Int]
cpede_sig_check sig x y =
  if x == 0 then sig
  else if x == y then []
  else drop 4 sig

animate_cpede :: Int -> Int -> Int -> Int -> Int -> [Int] -> Int
animate_cpede t n base_id model_id node_num frames =
  if node_num == 0 then ((frames, 394) !! (mod (div t 4) n)) + (7 - (model_id - base_id))
  else 13 - (model_id - base_id)

-- cpede_head_swap (and the nine functions above it) are intended to allow centipede NPCs to swap their head and tail end nodes, as a way to escape getting stuck if they crawl into a dead end.
-- This imitates the functionality of centipedes in the original ZZT.  However, as of build 8_10 this mechanic is still a work in progress.
reverse_segment [] = []
reverse_segment (x:xs) = cpede_reverse x : reverse_segment xs

reverse_node_locs char_state i =
  if reversed char_state == False && i == 0 then 0
  else if reversed char_state == False then i - 1
  else if reversed char_state == True && i == 127 then 127
  else i + 1

chs0 :: Int -> Array Int NPC_state -> Array Int NPC_state
chs0 head_i char_state_arr = char_state_arr // [(i, (char_state_arr ! i) {node_num = end_node (char_state_arr ! i) - node_num (char_state_arr ! i)}) | i <- [head_i..head_i + end_node (char_state_arr ! head_i)]]

chs1 :: Int -> Array Int NPC_state -> Array Int NPC_state
chs1 head_i char_state_arr = char_state_arr // [(i, (char_state_arr ! i) {node_locations = take 3 (node_locations (char_state_arr ! i)) ++ take 3 (node_locations (char_state_arr ! (reverse_node_locs (char_state_arr ! i) i)))}) | i <- [head_i..head_i + end_node (char_state_arr ! head_i)]]

chs2 :: Int -> Array Int NPC_state -> Array Int NPC_state
chs2 head_i char_state_arr = char_state_arr // [(head_i, (char_state_arr ! head_i) {dir_list = reverse_segment (dir_list (char_state_arr ! head_i))})]

chs3 :: Int -> Array Int NPC_state -> Array Int NPC_state
chs3 head_i char_state_arr = char_state_arr // [(i, (char_state_arr ! i) {reversed = not (reversed (char_state_arr ! i))}) | i <- [head_i..head_i + end_node (char_state_arr ! head_i)]]

chs4 :: Int -> Array Int NPC_state -> Array Int NPC_state
chs4 head_i char_state_arr = 
  if reversed (char_state_arr ! head_i) == True then char_state_arr // [(i, (char_state_arr ! i) {ticks_left0 = upd_ticks_left (ticks_left0 (char_state_arr ! i)) (reversed (char_state_arr ! i))}) | i <- [head_i + 1..head_i + end_node (char_state_arr ! head_i)]]
  else char_state_arr // [(i, (char_state_arr ! i) {ticks_left0 = upd_ticks_left (ticks_left0 (char_state_arr ! i)) (reversed (char_state_arr ! i))}) | i <- [head_i..head_i + end_node (char_state_arr ! head_i) - 1]]

chs6 False = 129
chs6 True = 130

chs7 :: Array Int NPC_state -> Int -> Int -> Int -> [Int]
chs7 char_state_arr sig i c =
  let char_state = char_state_arr ! i
  in
  if c > end_node char_state then []
  else sig : take 3 (node_locations char_state) ++ chs7 char_state_arr sig (i + 1) (c + 1)

cpede_head_swap :: Array Int NPC_state -> Int -> Array Int NPC_state
cpede_head_swap char_state_arr head_i =
  let chs0' = chs0 head_i
      chs1' = chs1 head_i
      chs2' = chs2 head_i
      chs3' = chs3 head_i
      chs4' = chs4 head_i
  in chs4' $ chs3' $ chs2' $ chs1' $ chs0' $ char_state_arr

upd_ticks_left :: Int -> Bool -> Int
upd_ticks_left t reversed =
  if reversed == False && t == 0 then 40
  else if reversed == True && t == 40 then 0
  else if reversed == False then t - 1
  else t + 1

cpede_move :: Int -> Int -> [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
cpede_move offset mode d_list (w:u:v:blocks) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 =
  let char_state = (npc_states s1) ! ((d_list, 395) !! 8)
      h_char_state = (npc_states s1) ! (head_index char_state)
      dir_list' = if node_num char_state == 0 then upd_dir_list (direction char_state) (dir_list char_state)
                  else dir_list h_char_state
      cpede_pos_ = cpede_pos u v ((dir_list', 396) !! (node_num char_state)) (ticks_left0 char_state) (reversed char_state)
      u' = fst (fst cpede_pos_)
      v' = snd (fst cpede_pos_)
      o_target = fromMaybe def_obj_place (obj (w_grid ! (-w - 1, u, v)))
      char_rotation_ = char_rotation 1 ((dir_list', 397) !! (node_num char_state)) ((d_list, 398) !! 9)
      w_grid' = ((-w - 1, u, v), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {u__ = fst (snd cpede_pos_), v__ = snd (snd cpede_pos_), texture__ = animate_cpede (fst__ (game_clock s0)) 11 ((d_list, 399) !! 9) char_rotation_ (node_num char_state) cpede_frames})})
      w_grid'' = [((-w - 1, u', v'), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {ident_ = char_rotation_})}), ((-w - 1, u, v), def_w_grid)]
      damage = det_damage (difficulty s1) s0
      npc_states' = cpede_head_swap (npc_states s1) (head_index char_state)
      d_list_upd = [(offset - 11, w), (offset - 10, u'), (offset - 9, v'), (offset - 8, w), (offset - 7, u), (offset - 6, v), (offset - 5, 15 - (char_rotation_ - ((d_list, 400) !! 9))), (offset - 4, 15 - (char_rotation_ - ((d_list, 401) !! 9)) + 42), (offset + 38, -w - 1)]
  in
  if direction char_state == 0 && ticks_left0 char_state == 0 && node_num char_state == 0 then (w_grid_upd, obj_grid_upd, s1 {npc_states = cpede_head_swap (npc_states s1) (head_index char_state), next_sig_q = chs7 (npc_states s1) (chs6 (not (reversed char_state))) (head_index char_state) 0 ++ next_sig_q s1})
  else if reversed char_state == True && mode == 0 then (w_grid_upd, obj_grid_upd, s1)
  else if reversed char_state == False && mode == 1 then (w_grid_upd, obj_grid_upd, s1)
  else if ticks_left0 char_state == 0 then
    if (w, u', v') == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) && node_num char_state == 0 then
      if attack_mode char_state == True && binary_dice_ 10 s0 == True then
        if health s1 - damage <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg28})
        else (w_grid_upd, obj_grid_upd, s1 {health = health s1 - damage, message = message s1 ++ msg29, next_sig_q = [chs6 (reversed char_state), w, u, v] ++ next_sig_q s1})
      else (w_grid_upd, obj_grid_upd, s1 {next_sig_q = [chs6 (reversed char_state), w, u, v] ++ next_sig_q s1})
    else if isNothing (obj (w_grid ! (-w - 1, u', v'))) == True then
      (w_grid'' ++ w_grid_upd, ((w, u, v), (-2, [])) : ((w, u', v'), (-2, d_list_upd)) : obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 402) !! 8, char_state {dir_list = dir_list', node_locations = [w, u', v', w, u, v], ticks_left0 = upd_ticks_left (ticks_left0 char_state) (reversed char_state)})], next_sig_q = cpede_sig_check ([chs6 (reversed char_state), w, u', v', chs6 (reversed char_state)] ++ drop 3 (node_locations char_state)) (node_num char_state) (end_node char_state) ++ next_sig_q s1})
    else
      if node_num char_state == 0 then (w_grid_upd, obj_grid_upd, s1 {next_sig_q = [chs6 (reversed char_state), w, u, v] ++ next_sig_q s1})
      else (w_grid_upd, obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 403) !! 8, char_state {ticks_left0 = upd_ticks_left (ticks_left0 char_state) (reversed char_state)})]})
  else (w_grid' : w_grid_upd, obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 404) !! 8, char_state {fg_position = (0, fst (snd cpede_pos_), snd (snd cpede_pos_)), ticks_left0 = upd_ticks_left (ticks_left0 char_state) (reversed char_state)})], next_sig_q = cpede_sig_check ([chs6 (reversed char_state), w, u, v, chs6 (reversed char_state)] ++ drop 3 (node_locations char_state)) (node_num char_state) (end_node char_state) ++ next_sig_q s1})

npc_damage :: Int -> [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> [Int] -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
npc_damage mode (w:u:v:blocks) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 d_list =
  let damage = det_damage ("d", 6, 10, 14) s0
      char_state = (npc_states s1) ! ((d_list, 405) !! 8)
      h_char_state = (npc_states s1) ! (head_index char_state)
      o_target = fromJust (obj (w_grid ! (-w - 1, u, v)))
  in
  if npc_type char_state == 2 then
    if reversed char_state == False && mode == 1 then (w_grid_upd, obj_grid_upd, s1)
    else if reversed char_state == True && mode == 0 then (w_grid_upd, obj_grid_upd, s1)
    else if c_health h_char_state - damage <= 0 then (w_grid_upd, obj_grid_upd, s1 {npc_states = chs3 (head_index char_state) (npc_states s1), message = message s1 ++ [2, 4, 14], next_sig_q = 131 : take 3 (node_locations char_state) ++ next_sig_q s1})
    else (w_grid_upd, obj_grid_upd, s1 {npc_states = (npc_states s1) // [(head_index char_state, h_char_state {c_health = (c_health h_char_state) - damage})], message = message s1 ++ [2, 4, 16]})
  else
  if c_health char_state - damage <= 0 then (((-w - 1, u, v), def_w_grid) : w_grid_upd, ((w, u, v), (-1, [])) : obj_grid_upd, s1 {message = message s1 ++ [2, 4, 14]})
  else (w_grid_upd, obj_grid_upd, s1 {npc_states = (npc_states s1) // [((d_list, 406) !! 8, char_state {c_health = (c_health char_state) - damage})], message = message s1 ++ [2, 4, 16]})

place_light :: Int -> Int -> Int -> Int -> Int -> Int -> Play_state0 -> [Int] -> Play_state0
place_light colour_r colour_g colour_b u v w s0 d_list = s0 {mobile_lights = (take 16 ([int_to_float ((d_list, 625) !! colour_r), int_to_float ((d_list, 626) !! colour_g), int_to_float ((d_list, 627) !! colour_b), 1] ++ fst (mobile_lights s0)), take 12 ([int_to_float ((d_list, 628) !! u), int_to_float ((d_list, 629) !! v), int_to_float ((d_list, 630) !! w)] ++ snd (mobile_lights s0)))}

-- Branch on each GPLC op - code to call the corresponding function, with optional per op - code status reports for debugging.
run_gplc :: [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> Int -> IO ([((Int, Int, Int), Wall_grid)], Array (Int, Int, Int) Floor_grid, [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state0, Play_state1)
run_gplc [] d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up c = return (w_grid_upd, f_grid, obj_grid_upd, s0, s1)
run_gplc code d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 0 =
  let location = (((splitOn [536870911] code), 407) !! 2)
  in do
  report_state (verbose_mode s1) 2 [] [] "\non_signal run.  Initial state is..."
  report_state (verbose_mode s1) 0 (snd (obj_grid ! ((location, 408) !! 0, (location, 409) !! 1, (location, 410) !! 2))) (((splitOn [536870911] code), 411) !! 2) []
  run_gplc (on_signal (drop 2 (((splitOn [536870911] code), 412) !! 0)) (((splitOn [536870911] code), 413) !! 1) ((code, 414) !! 1)) (((splitOn [536870911] code), 415) !! 2) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 1
run_gplc code d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 1 =
  let if0' = if0 code d_list
  in do
  report_state (verbose_mode s1) 2 [] [] ("\nIf expression folding run.  Branch selected: " ++ show if0')
  run_gplc (tail_ if0') d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up (head_ if0')
run_gplc xs d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 2 =
  let chg_state_ = chg_state (2 : xs) (0, 0, 0) (0, 0, 0) w_grid (array (0, 13) [(0, 3), (1, 0), (2, 3), (3, 0), (4, 3), (5, 0), (6, 3), (7, 0), (8, 3), (9, 0), (10, 3), (11, 0), (12, 3), (13, 0)]) w_grid_upd d_list
  in do
--  report_state (verbose_mode s1) 1 (snd (obj_grid ! ((d_list, 416) !! 0, (d_list, 417) !! 1, (d_list, 418) !! 2))) [] []
--  report_state (verbose_mode s1) 2 [] [] ("\nchg_state run with arguments " ++ "0: " ++ show ((d_list, 419) !! x0) ++ " 1: " ++ show ((d_list, 420) !! x1) ++ " 2: " ++ show ((d_list, 421) !! x2) ++ " 3: " ++ show ((d_list, 422) !! x3) ++ " 4: " ++ show ((d_list, 423) !! x4) ++ " 5: " ++ show ((d_list, 424) !! x5))
  run_gplc (tail_ (snd chg_state_)) d_list w_grid (fst chg_state_) f_grid obj_grid obj_grid_upd s0 s1 look_up (head_ (snd chg_state_))
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 3 = do
  report_state (verbose_mode s1) 2 [] [] ("\nchg_grid run with arguments " ++ "0: " ++ show ((d_list, 425) !! x0) ++ " 1: " ++ show ((d_list, 426) !! x1) ++ " 2: " ++ show ((d_list, 427) !! x2) ++ " 3: " ++ show ((d_list, 428) !! x3) ++ " 4: " ++ show ((d_list, 429) !! x4) ++ " 5: " ++ show ((d_list, 430) !! x5) ++ " 6: " ++ show ((d_list, 431) !! x6))
  run_gplc (tail_ xs) d_list w_grid (chg_grid x0 (x1, x2, x3) (x4, x5, x6) w_grid def_w_grid w_grid_upd d_list) f_grid obj_grid obj_grid_upd s0 s1 look_up (head_ xs)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 4 =
  let sig = send_signal 0 x0 (x1, x2, x3) obj_grid s1 d_list
  in do
  report_state (verbose_mode s1) 2 [] [] ("\nsend_signal run with arguments " ++ "0: " ++ show ((d_list, 432) !! x0) ++ " 1: " ++ show ((d_list, 433) !! x1) ++ " 2: " ++ show ((d_list, 434) !! x2) ++ " 3: " ++ show ((d_list, 435) !! x3))
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid (fst sig) obj_grid_upd s0 (snd sig) look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 5 = do
  report_state (verbose_mode s1) 2 [] [] ("\nchg_value run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show ((d_list, 436) !! x1) ++ " 2: " ++ show ((d_list, 437) !! x2) ++ " 3: " ++ show ((d_list, 438) !! x3) ++ " 4: " ++ show ((d_list, 439) !! x4) ++ " 5: " ++ show ((d_list, 440) !! x5))
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (chg_value x0 x1 x2 (x3, x4, x5) d_list obj_grid obj_grid_upd) s0 s1 look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 6 = do
  report_state (verbose_mode s1) 2 [] [] ("\nchg_floor run with arguments " ++ "0: " ++ show ((d_list, 441) !! x0) ++ " 1: " ++ show ((d_list, 442) !! x1) ++ " 2: " ++ show ((d_list, 443) !! x2) ++ " 3: " ++ show ((d_list, 444) !! x3) ++ " 4: " ++ show ((d_list, 445) !! x4) ++ " 5: " ++ show ((d_list, 446) !! x5))
  run_gplc (tail_ xs) d_list w_grid w_grid_upd (chg_floor x0 x1 x2 (x3, x4, x5) f_grid d_list) obj_grid obj_grid_upd s0 s1 look_up (head_ xs)
run_gplc (x0:x1:x2:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 7 = do
  report_state (verbose_mode s1) 2 [] [] ("\nchg_ps1 run with arguments " ++ "0: " ++ show ((d_list, 447) !! x0) ++ " 1: " ++ show ((d_list, 448) !! x1) ++ " 2: " ++ show ((d_list, 449) !! x2))
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (chg_ps1 x0 x1 x2 d_list s1) look_up (head_ xs)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 8 = do
  report_state (verbose_mode s1) 2 [] [] ("\nchg_obj_type run with arguments " ++ "0: " ++ show ((d_list, 450) !! x0) ++ " 1: " ++ show ((d_list, 451) !! x1) ++ " 2: " ++ show ((d_list, 452) !! x2) ++ " 3: " ++ show ((d_list, 453) !! x3))
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (chg_obj_type x0 (x1, x2, x3) d_list obj_grid obj_grid_upd) s0 s1 look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 9 = do
  report_state (verbose_mode s1) 2 [] [] ("\nplace_light run with arguments 0: " ++ show ((d_list, 619) !! x0) ++ " 1: " ++ show ((d_list, 620) !! x1) ++ " 2: " ++ show ((d_list, 621) !! x2) ++ " 3: " ++ show ((d_list, 622) !! x3) ++ " 4: " ++ show ((d_list, 623) !! x4) ++ " 5: " ++ show ((d_list, 624) !! x5))
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd (place_light x0 x1 x2 x3 x4 x5 s0 d_list) s1 look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 10 = do
  report_state (verbose_mode s1) 2 [] [] ("\nchg_grid_ run with arguments " ++ "0: " ++ show ((d_list, 457) !! x0) ++ " 1: " ++ show ((d_list, 458) !! x1) ++ " 2: " ++ show ((d_list, 459) !! x2) ++ " 3: " ++ show ((d_list, 460) !! x3) ++ " 4: " ++ show ((d_list, 461) !! x4) ++ " 5: " ++ show ((d_list, 462) !! x5) ++ " 6: " ++ show ((d_list, 463) !! x6))
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (chg_grid_ x0 (x1, x2, x3) (x4, x5, x6) obj_grid_upd d_list) s0 s1 look_up (head_ xs)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 11 = do
  report_state (verbose_mode s1) 2 [] [] ("\ncopy_ps1 run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show ((d_list, 464) !! x1) ++ " 2: " ++ show ((d_list, 465) !! x2) ++ " 3: " ++ show ((d_list, 466) !! x3))
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (copy_ps1 x0 (x1, x2, x3) s1 obj_grid obj_grid_upd d_list) s0 s1 look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 12 = do
  report_state (verbose_mode s1) 2 [] [] ("\ncopy_lstate run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show ((d_list, 467) !! x1) ++ " 2: " ++ show ((d_list, 468) !! x2) ++ " 3: " ++ show ((d_list, 469) !! x3) ++ " 4: " ++ show ((d_list, 470) !! x4) ++ " 5: " ++ show ((d_list, 471) !! x5) ++ " 6: " ++ show ((d_list, 472) !! x6))
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (copy_lstate x0 (x1, x2, x3) (x4, x5, x6) w_grid obj_grid obj_grid_upd d_list) s0 s1 look_up (head_ xs)
run_gplc (x:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 13 =
  let pass_msg' = pass_msg x xs s1 d_list
  in do
  report_state (verbose_mode s1) 2 [] [] ("\npass_msg run with arguments " ++ "msg_length: " ++ show ((d_list, 473) !! x) ++ " message data: " ++ show (take ((d_list, 474) !! x) xs))
  run_gplc (tail_ (fst pass_msg')) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (snd pass_msg') look_up (head_ (fst pass_msg'))
run_gplc (x0:x1:x2:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 14 = do
  report_state (verbose_mode s1) 2 [] [] ("\nchg_ps0 run with arguments " ++ "0: " ++ show ((d_list, 475) !! x0) ++ " 1: " ++ show ((d_list, 476) !! x1) ++ " 2: " ++ show ((d_list, 477) !! x2))
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd (chg_ps0 x0 x1 x2 d_list s0) s1 look_up (head_ xs)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 15 = do
  report_state (verbose_mode s1) 2 [] [] ("\ncopy_ps0 run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show ((d_list, 478) !! x1) ++ " 2: " ++ show ((d_list, 479) !! x2) ++ " 3: " ++ show ((d_list, 480) !! x3))
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (copy_ps0 x0 (x1, x2, x3) s0 obj_grid obj_grid_upd d_list) s0 s1 look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 16 = do
  report_state (verbose_mode s1) 2 [] [] ("\nbinary_dice run with arguments " ++ "0: " ++ show ((d_list, 481) !! x0) ++ " 1: " ++ show ((d_list, 482) !! x1) ++ " 2: " ++ show ((d_list, 483) !! x2) ++ " 3: " ++ show ((d_list, 484) !! x3) ++ " 4: " ++ show ((d_list, 485) !! x4) ++ " 5: " ++ show x5)
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (binary_dice x0 x1 (x2, x3, x4) x5 s0 obj_grid obj_grid_upd d_list) s0 s1 look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 17 = do
  report_state (verbose_mode s1) 2 [] [] ("\nproject_init run with arguments " ++ "0: " ++ show ((d_list, 486) !! x0) ++ " 1: " ++ show ((d_list, 487) !! x1) ++ " 2: " ++ show ((d_list, 488) !! x2) ++ " 3: " ++ show ((d_list, 489) !! x3) ++ "4: " ++ show ((d_list, 490) !! x4) ++ " 5: " ++ show ((d_list, 491) !! x5) ++ " 6: " ++ show ((d_list, 492) !! x6) ++ " 7: " ++ show ((d_list, 493) !! x7) ++ " 8: " ++ show x8)
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (project_init x0 x1 x2 x3 x4 (x5, x6, x7) (x8, x9, x10) x11 x12 obj_grid obj_grid_upd d_list look_up) s0 s1 look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 18 =
  let project_update' = project_update x0 x1 (x2, x3, x4) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 d_list
  in do
  report_state (verbose_mode s1) 2 [] [] ("\nproject_update run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show x1 ++ " 2: " ++ show ((d_list, 494) !! x2) ++ " 3: " ++ show ((d_list, 495) !! x3) ++ " 4: " ++ show ((d_list, 496) !! x4))
  run_gplc (tail_ xs) d_list w_grid (fst__ project_update') f_grid obj_grid (snd__ project_update') s0 (third_ project_update') look_up (head_ xs)
run_gplc (x0:x1:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 19 = do
  report_state (verbose_mode s1) 2 [] [] ("\ninit_npc run with arguments " ++ "0: " ++ show ((d_list, 497) !! x0) ++ " 1: " ++ show x1)
  report_npc_state (verbose_mode s1) s1 ((d_list, 498) !! 8)
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (init_npc x0 x1 s1 d_list) look_up (head_ xs)
run_gplc (x0:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 20 =
  let npc_decision_ = npc_decision 0 0 x0 0 0 0 d_list (node_locations ((npc_states s1) ! ((d_list, 499) !! 8))) w_grid f_grid obj_grid obj_grid_upd s0 s1 look_up
  in do
  report_state (verbose_mode s1) 2 [] [] ("\nnpc_decision run with arguments " ++ "0: " ++ show x0)
  report_npc_state (verbose_mode s1) s1 ((d_list, 500) !! 8)
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (fst npc_decision_) s0 (snd npc_decision_) look_up (head_ xs)
run_gplc (x0:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 21 =
  let npc_move_ = npc_move x0 d_list (node_locations ((npc_states s1) ! ((d_list, 501) !! 8))) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up
  in do
  report_state (verbose_mode s1) 2 [] [] ("\nnpc_move run with arguments " ++ "0: " ++ show x0)
  report_npc_state (verbose_mode s1) s1 ((d_list, 502) !! 8)
  run_gplc (tail_ xs) d_list w_grid (fst__ npc_move_) f_grid obj_grid (snd__ npc_move_) s0 (third_ npc_move_) look_up (head_ xs)
run_gplc (x0:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 22 =
  let npc_damage_ = npc_damage x0 (node_locations ((npc_states s1) ! ((d_list, 503) !! 8))) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 d_list
  in do
  report_state (verbose_mode s1) 2 [] [] ("\nnpc_damage run...")
  run_gplc (tail_ xs) d_list w_grid (fst__ npc_damage_) f_grid obj_grid (snd__ npc_damage_) s0 (third_ npc_damage_) look_up (head_ xs)
run_gplc (x0:x1:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 23 =
  let cpede_move_ = cpede_move x0 x1 d_list (node_locations ((npc_states s1) ! ((d_list, 504) !! 8))) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1
  in do
  report_state (verbose_mode s1) 2 [] [] ("\ncpede_move run with arguments " ++ "0: " ++ show x0)
  report_npc_state (verbose_mode s1) s1 ((d_list, 505) !! 8)
  run_gplc (tail_ xs) d_list w_grid (fst__ cpede_move_) f_grid obj_grid (snd__ cpede_move_) s0 (third_ cpede_move_) look_up (head_ xs)
run_gplc code d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up c = do
  putStr ("\nInvalid opcode: " ++ show c)
  putStr ("\nremaining code block: " ++ show code)
  throw Invalid_GPLC_opcode

-- These functions deal with GPLC debugging output and error reporting.
report_state :: Bool -> Int -> [Int] -> [Int] -> [Char] -> IO ()
report_state False mode prog d_list message = return ()
report_state True 0 prog d_list message = do
  putStr ("\nProgram list: " ++ show prog)
  putStr ("\nData list: " ++ show d_list)
report_state True 1 prog d_list message = putStr ("\n\nProgram list: " ++ show prog)
report_state True 2 prog d_list message = putStr message

report_npc_state :: Bool -> Play_state1 -> Int -> IO ()
report_npc_state False s1 i = return ()
report_npc_state True s1 i = putStr ("\n" ++ show ((npc_states s1) ! i))

gplc_error :: [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Floor_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> SomeException -> IO ([((Int, Int, Int), Wall_grid)], Array (Int, Int, Int) Floor_grid, [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state0, Play_state1)
gplc_error w_grid_upd f_grid obj_grid_upd s0 s1 e = do
  putStr ("\nA GPLC program in the map has had a runtime exception and Game :: Dangerous engine is designed to shut down in this case.  Exception thrown: " ++ show e)
  putStr "\nPlease see the readme.txt file for details of how to report this bug."
  exitSuccess
  return (w_grid_upd, f_grid, obj_grid_upd, s0, s1)

-- These two functions are to fix a major space leak, which occured when an NPC was active but not in view.  This appears to have been caused by an accumulation of pending updates to Wall_grid, delayed
-- due to laziness.  Testing showed that nothing short of forcing the update list elements to normal form was sufficient.
force_update1 :: ((Int, Int, Int), Wall_grid) -> Int
force_update1 ((w, u, v), voxel) =
  let c0 = \x -> if x == True then 1
                 else 0
      c1 = \x -> fl_to_int x
      c2 = \x -> if x == [] then 0
                 else 1
      c3 = \x -> ident_ (fromMaybe def_obj_place x) + (c1 (u__ (fromMaybe def_obj_place x))) + (c1 (v__ (fromMaybe def_obj_place x))) + (c1 (w__ (fromMaybe def_obj_place x))) + (c2 (rotation (fromMaybe def_obj_place x))) + (c0 (rotate_ (fromMaybe def_obj_place x))) + (c1 (phase (fromMaybe def_obj_place x))) + (texture__ (fromMaybe def_obj_place x)) + (fromIntegral (num_elem (fromMaybe def_obj_place x))) + obj_flag (fromMaybe def_obj_place x)
  in w + u + v + (c0 (u1 voxel)) + (c0 (u2 voxel)) + (c0 (v1 voxel)) + (c0 (v2 voxel)) + (c1 (u1_bound voxel)) + (c1 (u2_bound voxel)) + (c1 (v1_bound voxel)) + (c1 (v2_bound voxel)) + (c1 (w_level voxel)) + (c2 (wall_flag voxel)) + (c2 (texture voxel)) + c3 (obj voxel)

force_update0 :: [((Int, Int, Int), Wall_grid)] -> [((Int, Int, Int), Wall_grid)] -> Int -> IO [((Int, Int, Int), Wall_grid)]
force_update0 [] acc c = do
  if c == 3141593 then putStr "\nA message appears in the console.  Mysterious!"
  else return ()
  return acc
force_update0 (x:xs) acc c = force_update0 xs (x : acc) (c + force_update1 x)

-- Due to the complexity characteristics of the array update operator ( // ) it was decided at some point to replace all such operations on Wall_grid and Obj_grid in the GPLC op - code functions with
-- accumulations to lists.  Each element of the respective list would encode for an element update that would be performed by the list being passed to a single ( // ) operation at the end of each game
-- time tick.  This proved fairly simple for Wall_grid, although some updates that previously relied on sequential applications of ( // ) were made to work by making these updates atomic in the op - code
-- functions.  Solving the same problem for Obj_grid was less simple and the solution chosen involves encoding updates in a list of one type and mapping that to the type taken by the ( // ) operation.
-- This mapping is done by the function below.
atomise_obj_grid_upd :: Int -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [(Int, Int)] -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [Int]))]
atomise_obj_grid_upd m [] acc obj_grid = []
atomise_obj_grid_upd m (x:xs) acc obj_grid =
  let source = (obj_grid ! (fst x))
      prog = snd source
      new_prog0 = elems ((listArray (0, (length prog) - 1) prog :: UArray Int Int) // (snd (snd ((xs, 506) !! 0))))
      new_prog1 = elems ((listArray (0, (length prog) - 1) prog :: UArray Int Int) // (acc ++ snd (snd x)))
  in
  if m == 0 then
    if fst (snd x) >= 0 then atomise_obj_grid_upd 1 (x:xs) acc obj_grid
    else if fst (snd x) == -1 then (fst x, def_obj_grid) : atomise_obj_grid_upd 0 xs acc obj_grid
    else if fst (snd x) == -2 then
      if fst x == fst ((xs, 507) !! 0) then (fst x, (fst source, new_prog0)) : atomise_obj_grid_upd 0 (drop 1 xs) acc obj_grid
      else (fst ((xs, 509) !! 0), (fst source, new_prog0)) : (fst x, def_obj_grid) : atomise_obj_grid_upd 0 (drop 1 xs) acc obj_grid
    else (fst ((xs, 510) !! 0), (fst source, new_prog0)) : atomise_obj_grid_upd 0 (drop 1 xs) acc obj_grid
  else
    if xs == [] then [(fst x, (fst source, new_prog1))]
    else if fst x == fst ((xs, 511) !! 0) then atomise_obj_grid_upd 1 xs (acc ++ snd (snd x)) obj_grid
    else (fst x, (fst source, new_prog1)) : atomise_obj_grid_upd 0 xs [] obj_grid

-- These three functions (together with send_signal) implement the signalling system that drives GPLC program runs.  This involves signalling programs in response to player object collisions and handling
-- the signal queue, which allows programs to signal each other.  The phase_flag argument of link_gplc0 is used by update_play to limit the speed of the GPLC interpreter to 40 ticks per second,
-- independent of the variable frame rate.  The exception to this limit is if a player object collision needs to be handled, in which case an additional interpreter tick is allowed as a special case.
link_gplc0 :: Bool -> [Float] -> [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> Bool -> IO (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) (Int, [Int]), Play_state0, Play_state1)
link_gplc0 phase_flag (x0:x1:xs) (z0:z1:z2:zs) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up init_flag =
  let target0 = link_gplc2 x0 (z0, z1, z2)
      target1 = (((sig_q s1), 512) !! 1, ((sig_q s1), 513) !! 2, ((sig_q s1), 514) !! 3)
      prog = (snd (obj_grid ! target1))
      obj_grid' = (send_signal 1 1 target0 obj_grid s1 [])
      obj_grid'' = obj_grid // [(target1, (fst (obj_grid ! target1), (head__ prog) : (((sig_q s1), 515) !! 0) : drop 2 prog))]
  in do
  if init_flag == True then do
    if (x1 == 1 || x1 == 3) && head (snd (obj_grid ! target0)) == 0 then do
      report_state (verbose_mode s1) 2 [] [] ("\nPlayer starts GPLC program at Obj_grid " ++ show target0)
      run_gplc' <- catch (run_gplc (snd ((fst obj_grid') ! target0)) [] w_grid w_grid_upd f_grid (fst obj_grid') obj_grid_upd s0 s1 look_up 0) (\e -> gplc_error w_grid_upd f_grid obj_grid_upd s0 s1 e)
      link_gplc0 phase_flag (x0:x1:xs) (z0:z1:z2:zs) w_grid (fst_ run_gplc') (snd_ run_gplc') obj_grid (third run_gplc') (fourth run_gplc') (fifth run_gplc') look_up False
    else link_gplc0 phase_flag (x0:x1:xs) (z0:z1:z2:zs) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up False
  else if phase_flag == True then do
    if sig_q s1 == [] then do
      update <- force_update0 w_grid_upd [] 0
      return (w_grid // update, f_grid, obj_grid // (atomise_obj_grid_upd 0 obj_grid_upd [] obj_grid), s0, s1 {sig_q = next_sig_q s1, next_sig_q = []})
    else do
      report_state ((verbose_mode s1) && sig_q s1 /= []) 2 [] [] ("\n\ngame_t = " ++ show (fst__ (game_clock s0)) ++ "\n----------------\n\nsignal queue: " ++ show (sig_q s1) ++ "\n")
      if fst (obj_grid ! target1) == 1 || fst (obj_grid ! target1) == 3 then do
        report_state (verbose_mode s1) 2 [] [] ("\nGPLC program run at Obj_grid " ++ show (((sig_q s1), 516) !! 1, ((sig_q s1), 517) !! 2, ((sig_q s1), 518) !! 3))
        run_gplc' <- catch (run_gplc (snd (obj_grid'' ! target1)) [] w_grid w_grid_upd f_grid obj_grid'' obj_grid_upd s0 (s1 {sig_q = drop 4 (sig_q s1)}) look_up 0) (\e -> gplc_error w_grid_upd f_grid obj_grid_upd s0 s1 e)
        link_gplc0 True (x0:x1:xs) (z0:z1:z2:zs) w_grid (fst_ run_gplc') (snd_ run_gplc') obj_grid (third run_gplc') (fourth run_gplc') (fifth run_gplc') look_up False
      else do
        putStr ("\nSignal addressed to Obj_grid " ++ show (((sig_q s1), 519) !! 1, ((sig_q s1), 520) !! 2, ((sig_q s1), 521) !! 3) ++ " but this element is not set to run programs from.")
        link_gplc0 True (x0:x1:xs) (z0:z1:z2:zs) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (s1 {sig_q = drop 4 (sig_q s1)}) look_up False
  else do
    update <- force_update0 w_grid_upd [] 0
    return (w_grid // update, f_grid, obj_grid // (atomise_obj_grid_upd 0 obj_grid_upd [] obj_grid), s0, s1)

link_gplc1 :: Play_state0 -> Play_state1 -> Array (Int, Int, Int) (Int, [Int]) -> Int -> IO Play_state1
link_gplc1 s0 s1 obj_grid mode =
  let dest0 = (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0))
      dest1 = [truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)]
  in do
  if mode == 0 then 
    if fst (obj_grid ! dest0) == 1 || fst (obj_grid ! dest0) == 3 then return s1 {sig_q = sig_q s1 ++ [1] ++ dest1}
    else return s1
  else
    if fst (obj_grid ! dest0) == 1 || fst (obj_grid ! dest0) == 3 then do
      if health s1 <= det_damage (difficulty s1) s0 then return s1 {health = 0, state_chg = 1, message = 0 : msg26}
      else return s1 {sig_q = sig_q s1 ++ [1] ++ dest1, health = (health s1) - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg13}
    else do
      if health s1 <= det_damage (difficulty s1) s0 then return s1 {health = 0, state_chg = 1, message = 0 : msg26}
      else return s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg13}

link_gplc2 0 (z0, z1, z2) = (z0, z1, z2 + 1)
link_gplc2 1 (z0, z1, z2) = (z0, z1 + 1, z2)
link_gplc2 2 (z0, z1, z2) = (z0, z1, z2 - 1)
link_gplc2 3 (z0, z1, z2) = (z0, z1 - 1, z2)

-- These four functions perform game physics and geometry computations.  These include player collision detection, thrust, friction, gravity and floor surface modelling.
detect_coll :: Int -> (Float, Float) -> (Float, Float) -> Array (Int, Int, Int) (Int, [Int]) -> Array (Int, Int, Int) Wall_grid -> [Float]
detect_coll w_block (u, v) (step_u, step_v) obj_grid w_grid =
  let u' = u + step_u
      v' = v + step_v
      grid_i = w_grid ! (w_block, truncate u, truncate v)
      grid_o0 = fst (obj_grid ! (w_block, truncate u, (truncate v) + 1))
      grid_o1 = fst (obj_grid ! (w_block, (truncate u) + 1, truncate v))
      grid_o2 = fst (obj_grid ! (w_block, truncate u, (truncate v) - 1))
      grid_o3 = fst (obj_grid ! (w_block, (truncate u) - 1, truncate v))
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
thrust dir a force look_up =
  if dir == 3 then transform [force / 40, 0, 0, 1] (rotation_w a look_up)
  else if dir == 4 then transform [force / 40, 0, 0, 1] (rotation_w (mod_angle a 471) look_up)
  else if dir == 5 then transform [force / 40, 0, 0, 1] (rotation_w (mod_angle a 314) look_up)
  else transform [force / 40, 0, 0, 1] (rotation_w (mod_angle a 157) look_up)

floor_surf :: Float -> Float -> Float -> Array (Int, Int, Int) Floor_grid -> Float
floor_surf u v w f_grid =
  let f_tile0 = f_grid ! (truncate w, truncate (u / 2), truncate (v / 2))
      f_tile1 = f_grid ! ((truncate w) - 1, truncate (u / 2), truncate (v / 2))
  in
  if surface f_tile0 == Open then
    if surface f_tile1 == Positive_u then (w_ f_tile1) + (mod' u 2) / 2 + 0.1
    else if surface f_tile1 == Negative_u then 1 - ((w_ f_tile1) + (mod' u 2) / 2) + 0.1
    else if surface f_tile1 == Positive_v then (w_ f_tile1) + (mod' v 2) / 2 + 0.1
    else if surface f_tile1 == Negative_v then 1 - ((w_ f_tile1) + (mod' v 2) / 2) + 0.1
    else if surface f_tile1 == Flat then w_ f_tile1 + 0.1
    else 0
  else
    if surface f_tile0 == Positive_u then (w_ f_tile0) + (mod' u 2) / 2 + 0.1
    else if surface f_tile0 == Negative_u then 1 - ((w_ f_tile0) + (mod' u 2) / 2) + 0.1
    else if surface f_tile0 == Positive_v then (w_ f_tile0) + (mod' v 2) / 2 + 0.1
    else if surface f_tile0 == Negative_v then 1 - ((w_ f_tile0) + (mod' v 2) / 2) + 0.1
    else w_ f_tile0 + 0.1

update_vel :: [Float] -> [Float] -> [Float] -> Float -> Float -> [Float]
update_vel [] _ _ f_rate f = []
update_vel (x:xs) (y:ys) (z:zs) f_rate f =
  if z == 1 then 0 : update_vel xs ys zs f_rate f
  else (x + y / 32 + f * x / f_rate) : update_vel xs ys zs f_rate f

-- Used to generate the sequence of message tile references that represent the pause screen text.
pause_text :: [Char] -> Play_state1 -> ([Char], Int, Int, Int) -> [(Int, [Int])]
pause_text (x0:x1:x2:x3:x4:x5:xs) s1 (diff, a, b, c) =
  [(0, msg9), (0, []), (0, msg1 ++ conv_msg (health s1)), (0, msg2 ++ conv_msg (ammo s1)), (0, msg3 ++ conv_msg (gems s1)), (0, msg4 ++ conv_msg (torches s1)), (0, msg5 ++ keys s1), (0, msg6 ++ region s1), (0, conv_msg_ ("Difficulty: " ++ diff)), (0, conv_msg_ ("Time: " ++ [x0, x1, ':', x2, x3, ':', x4, x5])), (0, []), (1, msg10), (2, msg17), (3, msg11), (4, msg12)]

-- This function ensures that all signals sent to NPC GPLC programs are run before any others.  This is done to fix a corner case problem that occured when an NPC and projectile
-- tried to enter the same voxel in the same tick.
prioritise_npcs :: [Int] -> [Int] -> [Int] -> [Int]
prioritise_npcs [] acc0 acc1 = acc0 ++ acc1
prioritise_npcs (x0:x1:x2:x3:xs) acc0 acc1 =
  if x0 > 127 then prioritise_npcs xs (x0 : x1 : x2 : x3 : acc0) acc1
  else prioritise_npcs xs acc0 (x0 : x1 : x2 : x3 : acc1)

-- This function handles preemptive ceiling collision detection (i.e. stops the player jumping if there is a ceiling directly above).
jump_allowed :: Array (Int, Int, Int) Floor_grid -> Play_state0 -> Bool
jump_allowed f_grid s0 =
  if truncate (pos_w s0) == 2 then True
  else
    if surface (f_grid ! (truncate (pos_w s0) + 1, div (truncate (pos_u s0)) 2, div (truncate (pos_v s0)) 2)) == Open || surface (f_grid ! (truncate (pos_w s0), div (truncate (pos_u s0)) 2, div (truncate (pos_v s0)) 2)) /= Flat then True
    else False

-- The frames per second (FPS) measurements made here are used to drive the optional on screen FPS report and to scale player movement rates in real time, to allow for a variable frame rate
-- with consistent game play speed.  It is intended that the engine will be limited to ~60 FPS (set via the "min_frame_t" field of the conf_reg array) with movement scaling applied
-- between 40 - 60 FPS.  Below 40 FPS game play slow down will be seen.
determine_fps :: SEQ.Seq Integer -> Integer -> (Float, [Int], SEQ.Seq Integer)
determine_fps t_seq t_current =
  let frame_rate0 = 1000000000 / (fromIntegral (t_current - SEQ.index t_seq 0) / 40)
      frame_rate1 = if frame_rate0 >= 40 then frame_rate0
                    else 40
  in
  if SEQ.length t_seq < 40 then (48, [-1, 6, 16, 19, 69, 63] ++ conv_msg 0, t_seq SEQ.|> t_current)
  else (frame_rate1 / 1.25, [-1, 6, 16, 19, 69, 63] ++ conv_msg (truncate frame_rate0), (SEQ.drop 1 (t_seq SEQ.|> t_current)))

-- Game time is now composed of game_t (GPLC interpreter ticks) and frame_num (number of the next frame to be rendered).  These two functions deal with updating
-- game time and preparing a user readable representation of it, respectively.
update_game_clock :: (Int, Float, Int) -> Float -> (Bool, (Int, Float, Int))
update_game_clock (game_t, fl_game_t, frame_num) f_rate =
  let fl_game_t' = fl_game_t + (1 / f_rate) / (1 / 40)
  in
  if truncate fl_game_t == truncate fl_game_t' then (False, (game_t, fl_game_t', frame_num + 1))
  else (True, (truncate fl_game_t', fl_game_t', frame_num + 1))

show_game_time :: Int -> [Char] -> Bool -> [Char]
show_game_time t result True = reverse (take 6 (reverse ("00000" ++ result)))
show_game_time t result False =
  if t < 400 then show_game_time 0 (result ++ "0" ++ show (div t 40)) True
  else if t < 2400 then show_game_time 0 (result ++ show (div t 40)) True
  else if t < 24000 then show_game_time (mod t 2400) (result ++ "0" ++ show (div t 2400)) False
  else if t < 144000 then show_game_time (mod t 2400) (result ++ show (div t 2400)) False
  else if t < 146400 then show_game_time (mod t 144000) (show (div t 144000) ++ "00") False
  else show_game_time (mod t 144000) (show (div t 144000)) False

-- Used to send a set of in game metrics to the message display system, depending on the value of the "on_screen_metrics" field of the conf_reg array.
collect_metrics :: [Int] -> [Char] -> Play_state0 -> [(Int, [Int])]
collect_metrics fps_metric game_t_metric s0 =
  let proc_time = \(x0:x1:x2:x3:x4:x5:xs) -> [read [x0] + 53, read [x1] + 53, 69, read [x2] + 53, read [x3] + 53, 69, read [x4] + 53, read [x5] + 53]
  in
  if on_screen_metrics s0 == 1 then [(60, fps_metric)]
  else [(60, fps_metric), (60, proc_time game_t_metric)]

-- Restart the background music track each time a preset period has elapsed, if music is enabled.
play_music :: Int -> Int -> Array Int Source -> IO ()
play_music t period sound_array = do
  if period == 0 then return ()
  else if mod t period == 0 || t == 40 then play_ (sound_array ! (snd (bounds sound_array)))
  else return ()

-- This function recurses once for each recursion of show_frame (and rendering of that frame) and is the central branching point of the game logic thread.
update_play :: Io_box -> MVar (Play_state0, Array (Int, Int, Int) Wall_grid, Save_state) -> Play_state0 -> Play_state1 -> Bool -> Integer -> (Float, Float, Float, Float) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> UArray (Int, Int) Float -> Save_state -> (Array Int Source, Int) -> Integer -> MVar Integer -> SEQ.Seq Integer -> Float -> IO ()
update_play io_box state_ref s0 s1 in_flight min_frame_t (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up save_state sound_array t_last t_log t_seq f_rate =
  let det = detect_coll (truncate (pos_w s0)) (pos_u s0, pos_v s0) (((vel s0), 522) !! 0 / f_rate, ((vel s0), 523) !! 1 / f_rate) obj_grid w_grid
      floor = floor_surf ((det, 524) !! 0) ((det, 525) !! 1) (pos_w s0) f_grid
      vel_0 = update_vel (vel s0) [0, 0, 0] ((drop 2 det) ++ [0]) f_rate f
      vel_2 = update_vel (vel s0) [0, 0, g] ((drop 2 det) ++ [0]) f_rate 0
      game_clock' = update_game_clock (game_clock s0) (f_rate * 1.25)
      s0_ = \x -> x {message_ = [], mobile_lights = ([], [])}
      angle' = \x -> mod_angle_ (angle_ s0) f_rate x
      det_fps = \t_current -> determine_fps t_seq t_current
  in do
  mainLoopEvent
  control <- readIORef (control_ io_box)
  writeIORef (control_ io_box) 0
  link0 <- link_gplc0 (fst game_clock') (drop 4 det) [truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)] w_grid [] f_grid obj_grid [] s0 (s1 {sig_q = prioritise_npcs (sig_q s1) [] []}) look_up True
  link1 <- link_gplc1 s0 s1 obj_grid 0
  link1_ <- link_gplc1 s0 s1 obj_grid 1
  t <- getTime Monotonic
  if t_last == 0 then update_play io_box state_ref s0 s1 in_flight min_frame_t (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up save_state sound_array (toNanoSecs t) t_log (third_ (det_fps (toNanoSecs t))) 60
  else do
    if toNanoSecs t - t_last < min_frame_t then do
      threadDelay (fromIntegral (div (min_frame_t - (toNanoSecs t - t_last)) 1000))
      t' <- getTime Monotonic
      putMVar t_log (toNanoSecs t')
    else putMVar t_log (toNanoSecs t)
  t'' <- takeMVar t_log
  if mod (fst__ (game_clock s0)) 40 == 0 then do
    if on_screen_metrics s0 > 0 then do
      play_music (fst__ (game_clock s0)) (snd sound_array) (fst sound_array)
      update_play io_box state_ref (s0 {message_ = collect_metrics (snd__ (det_fps (toNanoSecs t))) (show_game_time (fst__ (game_clock s0)) [] False) s0, game_clock = snd game_clock'}) s1 in_flight min_frame_t (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up save_state sound_array t_last t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
    else do
      play_music (fst__ (game_clock s0)) (snd sound_array) (fst sound_array)
      update_play io_box state_ref (s0 {game_clock = snd game_clock'}) s1 in_flight min_frame_t (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up save_state sound_array t_last t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if control == 2 then do
    choice <- run_menu (pause_text (show_game_time (fst__ (game_clock s0)) [] False) s1 (difficulty s1)) [] io_box (-0.75) (-0.75) 1 0 0 s0 1
    if choice == 1 then update_play io_box state_ref (s0_ s0) s1 in_flight min_frame_t (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
    else if choice == 2 then do
      update_play io_box state_ref (s0_ s0) s1 in_flight min_frame_t (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up (Save_state {is_set = True, w_grid_ = w_grid, f_grid_ = f_grid, obj_grid_ = obj_grid, s0_ = s0, s1_ = s1}) sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
    else if choice == 3 then do
      putMVar state_ref (s0 {message_ = [(-1, [])]}, w_grid, save_state)
      update_play io_box state_ref (s0_ s0) s1 in_flight min_frame_t (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
    else do
      putMVar state_ref (s0 {message_ = [(-3, [])]}, w_grid, save_state)
      update_play io_box state_ref (s0_ s0) s1 in_flight min_frame_t (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if control == 10 then update_play io_box state_ref (s0_ (fourth link0)) ((fifth link0) {sig_q = sig_q s1 ++ [2, 0, 0, 0]}) in_flight min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if control == 11 then do
    if view_mode s0 == 0 then update_play io_box state_ref (s0_ ((fourth link0) {view_mode = 1})) (fifth link0) in_flight min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
    else update_play io_box state_ref (s0_ ((fourth link0) {view_mode = 0})) (fifth link0) in_flight min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if control == 12 then update_play io_box state_ref (s0_ ((fourth link0) {view_angle = mod_angle (view_angle s0) 5})) (fifth link0) in_flight min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if control == 13 then update_play io_box state_ref (s0_ (fourth link0)) ((fifth link0) {sig_q = sig_q s1 ++ [2, 0, 0, 1]}) in_flight min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else if message s1 /= [] then do
    event <- proc_msg0 (message s1) s0 s1 io_box (fst sound_array)
    putMVar state_ref (fst event, w_grid, save_state)
    update_play io_box state_ref (s0_ (fst event)) (snd event) in_flight min_frame_t (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
  else
    if in_flight == False then
      if (pos_w s0) - floor > 0.02 then do
        putMVar state_ref (s0 {pos_u = (det, 526) !! 0, pos_v = (det, 527) !! 1, mobile_lights = mobile_lights (fourth link0)}, w_grid, save_state)
        update_play io_box state_ref (s0_ ((fourth link0) {pos_u = (det, 528) !! 0, pos_v = (det, 529) !! 1, vel = vel_0, game_clock = snd game_clock'})) (fifth link0) True min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else if control > 2 && control < 7 then do
        putMVar state_ref (s0 {pos_u = (det, 530) !! 0, pos_v = (det, 531) !! 1, pos_w = floor, mobile_lights = mobile_lights (fourth link0)}, w_grid, save_state)
        update_play io_box state_ref (s0_ ((fourth link0) {pos_u = (det, 532) !! 0, pos_v = (det, 533) !! 1, pos_w = floor, vel = update_vel (vel s0) (take 3 (thrust (fromIntegral control) (angle s0) mag_r look_up)) ((drop 2 det) ++ [0]) f_rate f, game_clock = snd game_clock'})) (fifth link0) False min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else if control == 7 then do
        putMVar state_ref (s0 {pos_u = (det, 534) !! 0, pos_v = (det, 535) !! 1, pos_w = floor, angle = truncate (angle' False), mobile_lights = mobile_lights (fourth link0)}, w_grid, save_state)
        update_play io_box state_ref (s0_ ((fourth link0) {pos_u = (det, 536) !! 0, pos_v = (det, 537) !! 1, pos_w = floor, vel = vel_0, angle = truncate (angle' False), angle_ = (angle' False), game_clock = snd game_clock'})) (fifth link0) False min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else if control == 8 then do
        putMVar state_ref (s0 {pos_u = (det, 538) !! 0, pos_v = (det, 539) !! 1, pos_w = floor, angle = truncate (angle' True), mobile_lights = mobile_lights (fourth link0)}, w_grid, save_state)
        update_play io_box state_ref (s0_ ((fourth link0) {pos_u = (det, 540) !! 0, pos_v = (det, 541) !! 1, pos_w = floor, vel = vel_0, angle = truncate (angle' True), angle_ = (angle' True), game_clock = snd game_clock'})) (fifth link0) False min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else if control == 9 && jump_allowed f_grid s0 == True then do
        putMVar state_ref (s0 {pos_u = (det, 542) !! 0, pos_v = (det, 543) !! 1, pos_w = floor + mag_j / f_rate, mobile_lights = mobile_lights (fourth link0)}, w_grid, save_state)
        update_play io_box state_ref (s0_ ((fourth link0) {pos_u = (det, 544) !! 0, pos_v = (det, 545) !! 1, pos_w = floor + mag_j / f_rate, vel = (take 2 vel_0) ++ [mag_j], game_clock = snd game_clock'})) (fifth link0) False min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else if control == 13 then do
        putMVar state_ref (s0 {pos_u = (det, 546) !! 0, pos_v = (det, 547) !! 1, pos_w = floor, mobile_lights = mobile_lights (fourth link0)}, w_grid, save_state)
        update_play io_box state_ref (s0_ ((fourth link0) {pos_u = (det, 548) !! 0, pos_v = (det, 549) !! 1, pos_w = floor, vel = vel_0, game_clock = snd game_clock'})) ((fifth link0) {sig_q = sig_q s1 ++ [0, 0, 1]}) False min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (fst (send_signal 1 1 (0, 0, 1) (third link0) s1 [])) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else do
        putMVar state_ref (s0 {pos_u = (det, 550) !! 0, pos_v = (det, 551) !! 1, pos_w = floor, mobile_lights = mobile_lights (fourth link0)}, w_grid, save_state)
        update_play io_box state_ref (s0_ ((fourth link0) {pos_u = (det, 552) !! 0, pos_v = (det, 553) !! 1, pos_w = floor, vel = vel_0, game_clock = snd game_clock'})) (fifth link0) False min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
    else if in_flight == True && (pos_w s0) > floor then
      if control == 7 then do
        putMVar state_ref (s0 {pos_u = (det, 554) !! 0, pos_v = (det, 555) !! 1, pos_w = (pos_w s0) + (((vel s0), 556) !! 2) / f_rate, angle = truncate (angle' False), mobile_lights = mobile_lights (fourth link0)}, w_grid, save_state)
        update_play io_box state_ref (s0_ ((fourth link0) {pos_u = (det, 557) !! 0, pos_v = (det, 558) !! 1, pos_w = (pos_w s0) + (((vel s0), 559) !! 2) / f_rate, vel = vel_2, angle = truncate (angle' False), angle_ = (angle' False), game_clock = snd game_clock'})) (fifth link0) True min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else if control == 8 then do
        putMVar state_ref (s0 {pos_u = (det, 560) !! 0, pos_v = (det, 561) !! 1, pos_w = (pos_w s0) + (((vel s0), 562) !! 2) / f_rate, angle = truncate (angle' True), mobile_lights = mobile_lights (fourth link0)}, w_grid, save_state)
        update_play io_box state_ref (s0_ ((fourth link0) {pos_u = (det, 563) !! 0, pos_v = (det, 564) !! 1, pos_w = (pos_w s0) + (((vel s0), 565) !! 2) / f_rate, vel = vel_2, angle = truncate (angle' True), angle_ = (angle' True), game_clock = snd game_clock'})) (fifth link0) True min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else do
        putMVar state_ref (s0 {pos_u = (det, 566) !! 0, pos_v = (det, 567) !! 1, pos_w = (pos_w s0) + (((vel s0), 568) !! 2) / f_rate, mobile_lights = mobile_lights (fourth link0)}, w_grid, save_state)
        update_play io_box state_ref (s0_ ((fourth link0) {pos_u = (det, 569) !! 0, pos_v = (det, 570) !! 1, pos_w = (pos_w s0) + (((vel s0), 571) !! 2) / f_rate, vel = vel_2, game_clock = snd game_clock'})) (fifth link0) True min_frame_t (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
    else do
      putMVar state_ref (s0 {pos_u = (det, 572) !! 0, pos_v = (det, 573) !! 1, pos_w = floor, mobile_lights = mobile_lights (fourth link0)}, w_grid, save_state)
      if ((vel s0), 574) !! 2 < -4 then do
        update_play io_box state_ref (s0_ (s0 {pos_u = (det, 575) !! 0, pos_v = (det, 576) !! 1, pos_w = floor, vel = vel_0, game_clock = snd game_clock'})) link1_ False min_frame_t (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))
      else do
        update_play io_box state_ref (s0_ (s0 {pos_u = (det, 577) !! 0, pos_v = (det, 578) !! 1, pos_w = floor, vel = vel_0, game_clock = snd game_clock'})) link1 False min_frame_t (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up save_state sound_array t'' t_log (third_ (det_fps t'')) (fst__ (det_fps t''))

-- These five functions handle events triggered by a call to pass_msg within a GPLC program.  These include on screen messages, object interaction menus and sound effects.
conv_msg :: Int -> [Int]
conv_msg v =
  if v < 10 then [(mod v 10) + 53]
  else if v < 100 then [(div v 10) + 53, (mod v 10) + 53]
  else [(div v 100) + 53, (div (v - 100) 10) + 53, (mod v 10) + 53]

char_list = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 ,'.?;:+-=!()<>"

find_tile :: [Char] -> Char -> Int -> Int
find_tile [] t i = (i + 1)
find_tile (x:xs) t i =
  if x == t then (i + 1)
  else find_tile xs t (i + 1)

conv_msg_ :: [Char] -> [Int]
conv_msg_ [] = []
conv_msg_ (x:xs) = find_tile char_list x 0 : conv_msg_ xs

proc_msg1 :: [[Int]] -> [(Int, [Int])]
proc_msg1 [] = []
proc_msg1 (x:xs) = (head x, tail x) : proc_msg1 xs

proc_msg0 :: [Int] -> Play_state0 -> Play_state1 -> Io_box -> Array Int Source -> IO (Play_state0, Play_state1)
proc_msg0 [] s0 s1 io_box sound_array = return (s0, s1 {state_chg = 0, message = []})
proc_msg0 (x0:x1:xs) s0 s1 io_box sound_array =
  let signal_ = (head (splitOn [-1] (take x1 xs)))
  in do
  if x0 < 0 then return (s0 {message_ = message_ s0 ++ [(x0, take x1 xs)]}, s1)
  else if x0 == 0 && state_chg s1 == 1 && health s1 <= 0 then do
    play_ (sound_array ! 20)
    return (s0 {message_ = [(-2, take (x1 - 3) xs)]}, s1)
  else if x0 == 0 && state_chg s1 == 1 then proc_msg0 (drop (x1 - 3) xs) (s0 {message_ = message_ s0 ++ [(600, x0 : take (x1 - 3) xs ++ msg1 ++ conv_msg (health s1))]}) s1 io_box sound_array
  else if x0 == 0 && state_chg s1 == 2 then proc_msg0 (drop (x1 - 3) xs) (s0 {message_ = message_ s0 ++ [(600, x0 : take (x1 - 3) xs ++ msg2 ++ conv_msg (ammo s1))]}) s1 io_box sound_array
  else if x0 == 0 && state_chg s1 == 3 then proc_msg0 (drop (x1 - 3) xs) (s0 {message_ = message_ s0 ++ [(600, x0 : take (x1 - 3) xs ++ msg3 ++ conv_msg (gems s1))]}) s1 io_box sound_array
  else if x0 == 0 && state_chg s1 == 4 then proc_msg0 (drop (x1 - 3) xs) (s0 {message_ = message_ s0 ++ [(600, x0 : take (x1 - 3) xs ++ msg4 ++ conv_msg (torches s1))]}) s1 io_box sound_array
  else if x0 == 0 && state_chg s1 == 0 then proc_msg0 (drop (x1 - 3) xs) (s0 {message_ = message_ s0 ++ [(600, x0 : take (x1 - 3) xs)]}) s1 io_box sound_array
  else if x0 == 2 then do
    if ((xs, 582) !! 0) == 0 then return ()
    else play_ (sound_array ! (((xs, 583) !! 0) - 1))
    proc_msg0 (drop (x1 - 3) xs) s0 s1 io_box sound_array
  else if x0 == 3 then proc_msg0 [] (s0 {pos_u = int_to_float ((xs, 584) !! 0), pos_v = int_to_float ((xs, 585) !! 1), pos_w = int_to_float ((xs, 586) !! 2)}) s1 io_box sound_array
  else do
    choice <- run_menu (proc_msg1 (tail (splitOn [-1] (take (x1 - 3) xs)))) [] io_box (-0.96) (-0.2) 1 0 0 s0 (x0 - 3)
    proc_msg0 (drop (x1 - 3) xs) s0 (s1 {sig_q = sig_q s1 ++ [choice + 1, (signal_, 579) !! 0, (signal_, 580) !! 1, (signal_, 581) !! 2]}) io_box sound_array

-- Used by the game logic thread for in game menus and by the main thread for the main menu.
run_menu :: [(Int, [Int])] -> [(Int, [Int])] -> Io_box -> Float -> Float -> Int -> Int -> Int -> Play_state0 -> Int -> IO Int
run_menu [] acc io_box x y c c_max 0 s0 background = run_menu acc [] io_box x y c c_max 2 s0 background
run_menu (n:ns) acc io_box x y c c_max 0 s0 background = do
  if fst n == 0 then run_menu ns (acc ++ [n]) io_box x y c c_max 0 s0 background
  else run_menu ns (acc ++ [n]) io_box x y c (c_max + 1) 0 s0 background
run_menu [] acc io_box x y c c_max d s0 background = do
  swapBuffers
  threadDelay 16667
  mainLoopEvent
  control <- readIORef (control_ io_box)
  writeIORef (control_ io_box) 0
  if control == 3 && c > 1 then do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    run_menu acc [] io_box x 0.1 (c - 1) c_max 2 s0 background
  else if control == 5 && c < c_max then do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    run_menu acc [] io_box x 0.1 (c + 1) c_max 2 s0 background
  else if control == 2 then return c
  else do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    run_menu acc [] io_box x 0.1 c c_max 2 s0 background
run_menu (n:ns) acc io_box x y c c_max d s0 background = do
  if d == 2 then do
    glBindVertexArray (unsafeCoerce ((fst (p_bind_ io_box)) ! 1026))
    glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst (p_bind_ io_box)) ! (1026 + background)))
    glUseProgram (unsafeCoerce ((fst (p_bind_ io_box)) ! ((snd (p_bind_ io_box)) - 3)))
    glUniform1i (fromIntegral ((uniform_ io_box) ! 38)) 0
    p_tt_matrix <- mallocBytes (glfloat * 16)
    load_array [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1] p_tt_matrix 0
    glUniformMatrix4fv (fromIntegral ((uniform_ io_box) ! 36)) 1 1 p_tt_matrix
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT zero_ptr
    free p_tt_matrix
  else return ()
  glBindVertexArray (unsafeCoerce ((fst (p_bind_ io_box)) ! 933))
  p_tt_matrix <- mallocBytes ((length (snd n)) * glfloat * 16)
  if fst n == c then show_text (snd n) 1 933 (uniform_ io_box) (p_bind_ io_box) x y zero_ptr
  else show_text (snd n) 0 933 (uniform_ io_box) (p_bind_ io_box) x y zero_ptr
  free p_tt_matrix
  run_menu ns (acc ++ [n]) io_box x (y - 0.04) c c_max 1 s0 background

-- This function handles the drawing of message tiles (letters and numbers etc) that are used for in game messages and in menus.
show_text :: [Int] -> Int -> Int -> UArray Int Int32 -> (UArray Int Word32, Int) -> Float -> Float -> Ptr GLfloat -> IO ()
show_text [] mode base uniform p_bind x y p_tt_matrix = do
  glEnable GL_DEPTH_TEST
  free p_tt_matrix
show_text (m:ms) mode base uniform p_bind x y p_tt_matrix = do
  if minusPtr p_tt_matrix zero_ptr == 0 then do
    p_tt_matrix_ <- mallocBytes (16 * glfloat)
    glBindVertexArray (unsafeCoerce ((fst p_bind) ! 933))
    glUseProgram (unsafeCoerce ((fst p_bind) ! ((snd p_bind) - 3)))
    glDisable GL_DEPTH_TEST
    show_text (m:ms) mode base uniform p_bind x y p_tt_matrix_
  else do
    load_array (MAT.toList (translation x y 0)) (castPtr p_tt_matrix) 0
    if mode == 0 && m < 83 then do
      glUniformMatrix4fv (unsafeCoerce (uniform ! 36)) 1 1 p_tt_matrix
      glUniform1i (unsafeCoerce (uniform ! 38)) 0
    else if mode == 1 && m < 83 then do
      glUniformMatrix4fv (unsafeCoerce (uniform ! 36)) 1 1 p_tt_matrix
      glUniform1i (unsafeCoerce (uniform ! 38)) 1
    else show_text ms mode base uniform p_bind x y p_tt_matrix
    glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst p_bind) ! (base + m)))
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT zero_ptr
    show_text ms mode base uniform p_bind (x + 0.04) y p_tt_matrix
