module Game_logic_dev where

import System.IO
import System.IO.Unsafe
import System.Exit
import System.Directory
import Graphics.Rendering.OpenGL.Raw.Core41
import Foreign
import System.Win32
import Graphics.Win32
import Data.Fixed
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Maybe
import Data.List.Split
import Control.Concurrent
import Control.Exception
import qualified Data.Matrix as MAT
import Unsafe.Coerce
import Build_model
import Decompress_map

foreign import ccall "wingdi.h SwapBuffers"
  swapBuffers :: HDC -> IO Bool

def_w_grid = Wall_grid {u1 = False, u2 = False, v1 = False, v2 = False, u1_bound = 0, u2_bound = 0, v1_bound = 0, v2_bound = 0, w_level = 0,  wall_flag = [], texture = [], obj = Nothing}
def_obj_grid = (0, [])

load_array :: Storable a => [a] -> Ptr a -> Int -> IO ()
load_array [] p i = return ()
load_array (x:xs) p i = do
  pokeElemOff p i x
  load_array xs p (i + 1)

-- Encodings of set piece on screen messages used in the tile display system
--     <          Health:     >  <        Ammo:         >   <        Gems:          >  <          Torches:               >  <         Keys:          >  <            Region:           >
msg1 = [8,31,27,38,46,34,69,63]; msg2 = [1,39,39,41,69,63]; msg3 = [7,31,39,45,69,63]; msg4 = [20,41,44,29,34,31,45,69,63]; msg5 = [11,31,51,45,69,63]; msg6 = [18,31,33,35,41,40,69,63]
--     <                              Success!  You have discovered a new region.                                >
msg7 = [19,47,29,29,31,45,45,73,63,63,25,41,47,63,34,27,48,31,63,30,35,45,29,41,48,31,44,31,30,63,27,63,40,31,49,63,44,31,33,35,41,40,66]
--     <                GAME OVER!  Health: 0                    >  <            GAME PAUSED          >  <        Resume           >          <                   Return to main menu                  >
msg8 = [7,1,13,5,63,15,22,5,18,73,63,63,8,31,27,38,46,34,69,63,53]; msg9 = [7,1,13,5,63,16,1,21,19,5,4]; msg10 = [18,31,45,47,39,31]; msg11 = [18,31,46,47,44,40,63,46,41,63,39,27,35,40,63,39,31,40,47]
--      <   Exit   >          <                             Ouch!  The player fell.             >  <            Start game               >  <          Settings             >
msg12 = [5,50,35,46]; msg13 = [15,47,29,34,73,63,20,34,31,63,42,38,27,51,31,44,63,32,31,38,38,66]; msg14 = [19,46,27,44,46,63,33,27,39,31]; msg15 = [19,31,46,46,35,40,33,45]
--      <       MAIN MENU       >          <         Save game        >          <         Load game        >          <            New save file             >          <     < Back     >
msg16 = [13,1,9,14,63,13,5,14,21]; msg17 = [19,27,48,31,63,33,27,39,31]; msg18 = [12,41,27,30,63,33,27,39,31]; msg19 = [14,31,49,63,45,27,48,31,63,32,35,38,31]; msg20 = [74,63,2,27,29,37]
--        <                            Press x then enter file name in console.                                                                 >
msg21 = [(0, [16,44,31,45,45,63,50,63,46,34,31,40,63,31,40,46,31,44,63,32,35,38,31,63,40,27,39,31,63,35,40,63,29,41,40,45,41,38,31,66]), (1, [])]
main_menu_text :: [(Int, [Int])]
main_menu_text = [(0, msg16), (0, []), (1, msg14), (2, msg18), (3, msg12)]

-- These functions are where the program interacts with the Windows message system, allowing capture of keyboard and mouse input
wndProc :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc hwnd wmsg wParam lParam
    | wmsg == 258 = if wParam == 120 then do return 2
                    else if wParam == 97 then return 6  -- left
                    else if wParam == 100 then return 4 -- right
                    else if wParam == 115 then return 5 -- back
                    else if wParam == 119 then return 3 -- forward
                    else if wParam == 107 then return 7 -- turn left
                    else if wParam == 108 then return 8 -- turn right
                    else if wParam == 121 then return 9 -- jump
                    else if wParam == 116 then return 10 -- light torch
                    else if wParam == 99 then return 11 -- switch view mode
                    else if wParam == 118 then return 12 -- rotate 3rd person view
                    else do return 1
    | otherwise = do
        defWindowProc (Just hwnd) wmsg wParam lParam
        return 1

messagePump :: HWND -> IO Int32
messagePump hwnd = allocaMessage $ \ msg ->
  let pump = do x <- peekMessage msg (Just hwnd) 0 0 1
                if x /= () then do
                  return 0
                else do
                  translateMessage msg
                  r <- dispatchMessage msg
                  return r
  in pump

-- The following functions implement an interpreter of the Game Programmable Logic Controller (GPLC) language used to determine dynamic object behaviour
upd' x y = x + y
upd'' x y = y
upd''' x y = x - y
upd 0 = upd'
upd 1 = upd''
upd 2 = upd'''
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

mod_rotation :: Int -> Int -> Int -> [Int] -> [Int]
mod_rotation 0 abs v r = [upd_a abs (r !! 0) v, r !! 1, r !! 2]
mod_rotation 1 abs v r = [r !! 0, upd_a abs (r !! 1) v, r !! 2]
mod_rotation 2 abs v r = [r !! 0, r !! 1, upd_a abs (r !! 2) v]

int_to_float :: Int -> Float
int_to_float x = (fromIntegral x) * 0.000001

fl_to_int :: Float -> Int
fl_to_int x = truncate (x * 1000000)

head_ [] = 0
head_ ls = head ls
tail_ [] = []
tail_ ls = tail ls
fst_ (a, b, c, d, e) = a
snd_ (a, b, c, d, e) = b
third (a, b, c, d, e) = c
fourth (a, b, c, d, e) = d
fifth (a, b, c, d, e) = e
fst__ (a, b, c) = a
snd__ (a, b, c) = b
third_ (a, b, c) = c

bool_to_int True = 1
bool_to_int False = 0

int_to_bool 0 = False
int_to_bool 1 = True

-- These functions are part of the implementation of game state saving
save_game2 :: Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> Array (Int, Int, Int) (Bool, [Int]) -> Int -> Int -> Int -> Int -> Int -> Int -> [Char] -> [Char]
save_game2 w_grid f_grid obj_grid save_grid u_limit v_limit w_limit u v w acc =
  let w_cube = w_grid ! (-(w + 1), u, v)
      f_cube = f_grid ! (w, u, v)
      obj_cube = obj_grid ! (w, u, v)
      prog_state = take 2 (snd obj_cube) ++ ((splitOn [536870911] (snd obj_cube)) !! 2)
      w_grid_patch = "w, " ++ show (ident_ (fromJust (obj w_cube))) ++ ", " ++ show (u__ (fromJust (obj w_cube))) ++ ", " ++ show (v__ (fromJust (obj w_cube))) ++ ", " ++ show (w__ (fromJust (obj w_cube))) ++ ", " ++ show (texture__ (fromJust (obj w_cube))) ++ ", " ++ show (num_elem (fromJust (obj w_cube))) ++ ", " ++ show (obj_flag (fromJust (obj w_cube))) ++ ", "
      f_grid_patch = "f, " ++ show (w_ f_cube) ++ ", " ++ show (surface f_cube) ++ ", "
      obj_grid_patch = "o, " ++ show (fst obj_cube) ++ ", " ++ show (length prog_state) ++ ", " ++ show_ints prog_state
  in
  if w > w_limit then acc ++ "e"
  else if v > v_limit then save_game2 w_grid f_grid obj_grid save_grid u_limit v_limit w_limit 0 0 (w + 1) acc
  else if u > u_limit then save_game2 w_grid f_grid obj_grid save_grid u_limit v_limit w_limit 0 (v + 1) w acc
  else if fst (save_grid ! (w, u, v)) == True then save_game2 w_grid f_grid obj_grid save_grid u_limit v_limit w_limit (u + 1) v w (acc ++ show w ++ ", " ++ show u ++ ", " ++ show v ++ ", " ++ show_ints (snd (save_grid ! (w, u, v))) ++ w_grid_patch ++ f_grid_patch ++ obj_grid_patch)
  else save_game2 w_grid f_grid obj_grid save_grid u_limit v_limit w_limit (u + 1) v w acc

save_game1 :: Play_state0 -> Play_state1 -> [Char]
save_game1 s0 s1 =
  let s0_save = show (pos_u s0) ++ ", " ++ show (pos_v s0) ++ ", " ++ show (pos_w s0) ++ ", " ++ show ((vel s0) !! 0) ++ ", " ++ show ((vel s0) !! 1) ++ ", " ++ show ((vel s0) !! 2) ++ ", " ++ show (angle s0) ++ ", " ++ show (rend_mode s0) ++ ", " ++ show (view_mode s0) ++ ", " ++ show (view_angle s0) ++ ", " ++ show (game_t s0) ++ ", " ++ show (torch_t0 s0) ++ ", " ++ show (torch_t_limit s0) ++ ", "
      s1_save = show (health s1) ++ ", " ++ show (ammo s1) ++ ", " ++ show (gems s1) ++ ", " ++ show (torches s1) ++ ", " ++ show_ints (keys s1) ++ ", " ++ show (length (region s1)) ++ ", " ++ show_ints (region s1) ++ ", " ++ show (length (sig_q s1)) ++ ", " ++ show_ints (sig_q s1) ++ ", " ++ show (length (message s1)) ++ ", " ++ show_ints (message s1) ++ ", " ++ show (state_chg s1)
  in
  s0_save ++ s1_save

save_game0 :: Io_box -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> Play_state0 -> Play_state1 -> Array Int [Char] -> IO ()
save_game0 io_box w_grid f_grid obj_grid s0 s1 conf_reg = do
  run_menu msg21 [] io_box (-0.75) (-0.75) 1 0 0
  putStr "\nPlease enter save game file name (no extention needed): "
  file_name <- getLine
  putStr "\nSaving game..."
  h <- openFile ((cfg conf_reg 0 "save_game_dir") ++ file_name ++ ".gds") WriteMode
  hPutStr h (save_game1 s0 s1 ++ "\n~\n" ++ save_game2 w_grid f_grid obj_grid (save_grid s1) (read (cfg conf_reg 0 "u_limit")) (read (cfg conf_reg 0 "v_limit")) (read (cfg conf_reg 0 "w_limit")) 0 0 0 [])
  hClose h

load_game2 :: [[Char]] -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) (Int, [Int]) ->  (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) (Int, [Int]))
load_game2 [] w_grid obj_grid = (w_grid, obj_grid)
load_game2 state_log w_grid obj_grid =
  let position = (read (state_log !! 0), read (state_log !! 1), read (state_log !! 2))
  in
  if read (state_log !! 3) == 1 then load_game2 (drop (21 + read (state_log !! 20)) state_log) (w_grid // [(position, def_w_grid)]) (obj_grid // [(position, def_obj_grid)])
  else load_game2 (drop (21 + read (state_log !! 20)) state_log) w_grid obj_grid

load_game1 :: [[Char]] -> ([Char], [Char], [Char]) -> ([[Char]], [[Char]], [[Char]]) -> ([[Char]], [[Char]], [[Char]])
load_game1 [] (a, b, c) (w, f, o) = (w, f, o)
load_game1 (x:xs) (a, b, c) (w, f, o) =
  if x == "w" then load_game1 (drop 7 xs) (a, b, c) (w ++ [a, b, c] ++ take 7 xs, f, o)
  else if x == "f" then load_game1 (drop 2 xs) (a, b, c) (w, f ++ [a, b, c] ++ take 2 xs, o)
  else if x == "o" then load_game1 (drop ((read (xs !! 1)) + 2) xs) (a, b, c) (w, f, o ++ [a, b, c] ++ take ((read (xs !! 1)) + 2) xs)
  else load_game1 (drop 6 xs) (xs !! 0, xs !! 1, xs !! 2) (w, f, o)

load_game0 :: Int -> [[Char]] -> [[Char]] -> [[Char]] -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> Play_state0 -> Play_state1 -> (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) (Int, [Int]), Play_state0, Play_state1)
load_game0 mode state_log0 state_log1 acc w_grid f_grid obj_grid s0 s1 =
  let load_game1' = load_game1 state_log1 ([], [], []) ([], [], [])
  in
  if mode == 0 then (w_grid, f_grid, obj_grid, s0, s1)
  else if state_log1 == [] then load_game0 2 state_log0 acc [] w_grid f_grid obj_grid s0 s1
  else if mode == 1 then load_game0 1 state_log0 (drop (21 + read (state_log1 !! 20)) state_log1) (acc ++ take (21 + read (state_log1 !! 20)) state_log1) w_grid f_grid obj_grid s0 (s1 {save_grid = (save_grid s1) // [((read (state_log1 !! 0), read (state_log1 !! 1), read (state_log1 !! 2)), (True, [read (state_log1 !! 3), read (state_log1 !! 4), read (state_log1 !! 5), read (state_log1 !! 6)]))]})
  else (patch_w_grid (fst__ load_game1') w_grid, patch_f_grid (snd__ load_game1') f_grid, patch_obj_grid (third_ load_game1') obj_grid, set_play_state0 (take 13 state_log0), set_play_state1 0 (drop 13 state_log0) ps1_init)

-- These functions perform conditional expression folding, evaluating conditional op - codes at the start of a GPLC program run to yield unconditional code
on_signal :: [Int] -> [Int] -> Int -> [Int]
on_signal [] code sig = []
on_signal (x0:x1:x2:xs) code sig =
  if x0 == sig then take x2 (drop x1 code)
  else on_signal xs code sig

if1 :: Int -> Int -> Int -> [Int] -> [Int] -> [Int] -> [Int]
if1 0 arg0 arg1 code0 code1 d_list =
  if d_list !! arg0 == d_list !! arg1 then code0
  else code1
if1 1 arg0 arg1 code0 code1 d_list =
  if d_list !! arg0 < d_list !! arg1 then code0
  else code1
if1 2 arg0 arg1 code0 code1 d_list =
  if d_list !! arg0 > d_list !! arg1 then code0
  else code1

if0 :: [Int] -> [Int] -> [Int]
if0 code d_list =
  if code !! 0 == 1 then if0 (if1 (code !! 1) (code !! 2) (code !! 3) (take (code !! 4) (drop 6 code)) (take (code !! 5) (drop (6 + code !! 4) code)) d_list) d_list
  else code

-- The remaining GPLC op - codes are implemented here
chg_state :: Int -> Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> [Int] -> Array (Int, Int, Int) Wall_grid
chg_state state_val abs v (i0, i1, i2) grid d_list =
  let index = (d_list !! i0, d_list !! i1, d_list !! i2)
      grid_i = fromJust (obj (grid ! index))
  in
  if d_list !! state_val == 0 then grid // [(index, (grid ! index) {obj = Just (grid_i {ident_ = upd (d_list !! abs) (ident_ grid_i) (d_list !! v)})})]
  else if d_list !! state_val == 1 then grid // [(index, (grid ! index) {obj = Just (grid_i {u__ = upd (d_list !! abs) (u__ grid_i) (int_to_float (d_list !! v))})})]
  else if d_list !! state_val == 2 then grid // [(index, (grid ! index) {obj = Just (grid_i {v__ = upd (d_list !! abs) (v__ grid_i) (int_to_float (d_list !! v))})})]
  else if d_list !! state_val == 3 then grid // [(index, (grid ! index) {obj = Just (grid_i {w__ = upd (d_list !! abs) (w__ grid_i) (int_to_float (d_list !! v))})})]
  else if d_list !! state_val == 4 then grid // [(index, (grid ! index) {obj = Just (grid_i {rotation = mod_rotation 0 (d_list !! abs) (d_list !! v) (rotation grid_i)})})]
  else if d_list !! state_val == 5 then grid // [(index, (grid ! index) {obj = Just (grid_i {rotation = mod_rotation 1 (d_list !! abs) (d_list !! v) (rotation grid_i)})})]
  else if d_list !! state_val == 6 then grid // [(index, (grid ! index) {obj = Just (grid_i {rotation = mod_rotation 2 (d_list !! abs) (d_list !! v) (rotation grid_i)})})]
  else if d_list !! state_val == 7 then grid // [(index, (grid ! index) {obj = Just (grid_i {rotate_ = upd_b (d_list !! v)})})]
  else if d_list !! state_val == 8 then grid // [(index, (grid ! index) {obj = Just (grid_i {phase = upd (d_list !! abs) (phase grid_i) (int_to_float (d_list !! v))})})]
  else if d_list !! state_val == 9 then grid // [(index, (grid ! index) {obj = Just (grid_i {texture__ = upd (d_list !! abs) (texture__ grid_i) (d_list !! v)})})]
  else if d_list !! state_val == 10 then grid // [(index, (grid ! index) {obj = Just (grid_i {num_elem = fromIntegral (d_list !! v)})})]
  else grid

chg_grid :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> Array (Int, Int, Int) a -> a -> [Int] -> Array (Int, Int, Int) a
chg_grid mode (i0, i1, i2) (i3, i4, i5) grid def d_list =
  let dest0 = (d_list !! i0, d_list !! i1, d_list !! i2)
      dest1 = (d_list !! i3, d_list !! i4, d_list !! i5)
  in
  if d_list !! mode == 0 then grid // [(dest0, def)]
  else if d_list !! mode == 1 then grid // [(dest1, grid ! dest0), (dest0, def)]
  else grid // [(dest1, grid ! dest0)]

chg_floor :: Int -> Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) Floor_grid -> [Int] -> Array (Int, Int, Int) Floor_grid
chg_floor state_val abs v (i0, i1, i2) grid d_list =
  let index = (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  if d_list !! state_val == 0 then grid // [(index, (grid ! index) {w_ = upd (d_list !! abs) (w_ (grid ! index)) (int_to_float (d_list !! v))})]
  else grid // [(index, (grid ! index) {surface = int_to_surface (d_list !! v)})]

chg_value :: Int -> Int -> Int -> (Int, Int, Int) -> [Int] -> Array (Int, Int, Int) (Int, [Int]) -> Array (Int, Int, Int) (Int, [Int])
chg_value val abs v (i0, i1, i2) d_list obj_grid =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, (take val (snd target)) ++ [upd (d_list !! abs) ((snd target) !! val) (d_list !! v)] ++ drop (val + 1) (snd target)))]

chg_ps0 :: Int -> Int -> Int -> [Int] -> Play_state0 -> Play_state0
chg_ps0 state_val abs v d_list s0 =
  if d_list !! state_val == 0 then s0 {pos_u = upd (d_list !! abs) (pos_u s0) (int_to_float (d_list !! v))}
  else if d_list !! state_val == 1 then s0 {pos_v = upd (d_list !! abs) (pos_v s0) (int_to_float (d_list !! v))}
  else if d_list !! state_val == 2 then s0 {pos_w = upd (d_list !! abs) (pos_w s0) (int_to_float (d_list !! v))}
  else if d_list !! state_val == 3 then s0 {vel = [upd (d_list !! abs) ((vel s0) !! 0) (int_to_float (d_list !! v)), upd (d_list !! abs) ((vel s0) !! 1) (int_to_float (d_list !! (v + 1))), upd (d_list !! abs) ((vel s0) !! 2) (int_to_float (d_list !! (v + 2)))]}
  else if d_list !! state_val == 4 then s0 {rend_mode = d_list !! v}
  else if d_list !! state_val == 5 then s0 {torch_t0 = d_list !! v}
  else s0 {torch_t_limit = d_list !! v}

chg_ps1 :: Int -> Int -> Int -> [Int] -> Play_state1 -> Play_state1
chg_ps1 state_val abs v d_list s =
  if d_list !! state_val == 0 then s {health = upd (d_list !! abs) (health s) (d_list !! v), state_chg = 1}
  else if d_list !! state_val == 1 then s {ammo = upd (d_list !! abs) (ammo s) (d_list !! v), state_chg = 2}
  else if d_list !! state_val == 2 then s {gems = upd (d_list !! abs) (gems s) (d_list !! v), state_chg = 3}
  else if d_list !! state_val == 3 then s {torches = upd (d_list !! abs) (torches s) (d_list !! v), state_chg = 4}
  else s {keys = (take abs (keys s)) ++ [d_list !! v] ++ drop (abs + 1) (keys s), state_chg = 5}

copy_ps0 :: Int -> (Int, Int, Int) -> Play_state0 -> Array (Int, Int, Int) (Int, [Int]) -> [Int] -> Array (Int, Int, Int) (Int, [Int])
copy_ps0 offset (i0, i1, i2) s0 obj_grid d_list =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, (take offset (snd target)) ++ [fl_to_int (pos_u s0), fl_to_int (pos_v s0), fl_to_int (pos_w s0), fl_to_int ((vel s0) !! 0), fl_to_int ((vel s0) !! 1), fl_to_int ((vel s0) !! 2), rend_mode s0, game_t s0, torch_t0 s0, torch_t_limit s0] ++ drop (offset + 10) (snd target)))]

copy_ps1 :: Int -> (Int, Int, Int) -> Play_state1 -> Array (Int, Int, Int) (Int, [Int]) -> [Int] -> Array (Int, Int, Int) (Int, [Int])
copy_ps1 offset (i0, i1, i2) s obj_grid d_list =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, (take offset (snd target)) ++ [health s, ammo s, gems s, torches s] ++ keys s ++ drop (offset + 10) (snd target)))]

obj_type :: Int -> Int -> Int -> Array (Int, Int, Int) (Int, [Int]) -> Int
obj_type w u v obj_grid =
  if u < 0 || v < 0 then 2
  else if bound_check u 0 (bounds obj_grid) == False then 2

  else if bound_check v 1 (bounds obj_grid) == False then 2
  else fst (obj_grid ! (w, u, v))

copy_lstate :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) (Int, [Int]) -> [Int] -> Array (Int, Int, Int) (Int, [Int])
copy_lstate offset (i0, i1, i2) (i3, i4, i5) w_grid obj_grid d_list =
  let w = (d_list !! i3)
      u = (d_list !! i4)
      v = (d_list !! i5)
      u' = (d_list !! i4) - 1
      u'' = (d_list !! i4) + 1
      v' = (d_list !! i5) - 1
      v'' = (d_list !! i5) + 1
      w_conf_u1 = bool_to_int (u1 (w_grid ! (d_list !! i3, d_list !! i4, d_list !! i5)))
      w_conf_u2 = bool_to_int (u2 (w_grid ! (d_list !! i3, d_list !! i4, d_list !! i5)))
      w_conf_v1 = bool_to_int (v1 (w_grid ! (d_list !! i3, d_list !! i4, d_list !! i5)))
      w_conf_v2 = bool_to_int (v2 (w_grid ! (d_list !! i3, d_list !! i4, d_list !! i5)))
      target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, (take offset (snd target)) ++ [obj_type w u v obj_grid, obj_type w u' v obj_grid, obj_type w u'' v obj_grid, obj_type w u v' obj_grid, obj_type w u v'' obj_grid, obj_type w u' v' obj_grid, obj_type w u'' v' obj_grid, obj_type w u' v'' obj_grid, obj_type w u'' v'' obj_grid] ++ [w_conf_v2, w_conf_u2, w_conf_v1, w_conf_u1] ++ drop (offset + 9) (snd target)))]

chg_obj_type :: Int -> (Int, Int, Int) -> [Int] -> Array (Int, Int, Int) (Int, [Int]) -> Array (Int, Int, Int) (Int, [Int])
chg_obj_type v (i0, i1, i2) d_list obj_grid =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (d_list !! v, snd target))]

pass_msg :: Int -> [Int] -> Play_state1 -> [Int] -> ([Int], Play_state1)
pass_msg len msg s d_list = (drop (d_list !! len) msg, s {message = take (d_list !! len) msg})

send_signal :: Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) (Int, [Int]) -> Play_state1 -> [Int] -> (Array (Int, Int, Int) (Int, [Int]), Play_state1)
send_signal 0 sig (i0, i1, i2) obj_grid s d_list =
  let dest = (d_list !! i0, d_list !! i1, d_list !! i2)
      prog = (snd (obj_grid ! dest))
  in
  if fst (obj_grid ! dest) == 1 || fst (obj_grid ! dest) == 3 then (obj_grid // [(dest, (fst (obj_grid ! dest), (head prog) : (d_list !! sig) : drop 2 prog))], s {next_sig_q = next_sig_q s ++ [d_list !! i0, d_list !! i1, d_list !! i2]})
  else (obj_grid, s)
send_signal 1 sig dest obj_grid s d_list =
  let prog = (snd (obj_grid ! dest))
  in
  (obj_grid // [(dest, (fst (obj_grid ! dest), (head prog) : sig : drop 2 prog))], s)

place_hold :: Int -> [Int] -> Play_state1 -> Play_state1
place_hold val d_list s = unsafePerformIO (putStr "\nplace_hold run with value " >> print (d_list !! val) >> return s)

chg_save_grid :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> Play_state1 -> [Int] -> Play_state1
chg_save_grid setting (i0, i1, i2) (i3, i4, i5) s1 d_list = s1 {save_grid = save_grid s1 // [((d_list !! i0, d_list !! i1, d_list !! i2), (int_to_bool setting, [i3, i4, i5]))]}

-- These three functions handle GPLC code interpretation and signal propagation between GPLC programs and the rest of the game logic.  This is the version with optional GPLC debugging.
run_gplc :: [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> Play_state0 -> Play_state1 -> Int -> (Int, Int, Int) -> IO (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) (Int, [Int]), Play_state0, Play_state1)
run_gplc [] d_list w_grid f_grid obj_grid s0 s1 c location = return (w_grid, f_grid, obj_grid, s0, s1)
run_gplc code d_list w_grid f_grid obj_grid s0 s1 0 location = do
  report_state (verbose_mode s1) 2 [] [] "\non_signal run.  Initial state is..."
  report_state (verbose_mode s1) 0 (snd (obj_grid ! location)) ((splitOn [536870911] code) !! 2) []
  run_gplc (on_signal (drop 2 ((splitOn [536870911] code) !! 0)) ((splitOn [536870911] code) !! 1) (code !! 1)) ((splitOn [536870911] code) !! 2) w_grid f_grid obj_grid s0 s1 1 location
run_gplc code d_list w_grid f_grid obj_grid s0 s1 1 location = do
  report_state (verbose_mode s1) 2 [] [] "\nif expression folding run.  State is..."
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  run_gplc (tail_ (if0 code d_list)) d_list w_grid f_grid obj_grid s0 s1 (head_ (if0 code d_list)) location
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid f_grid obj_grid s0 s1 2 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_state run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5))
  run_gplc (tail_ xs) d_list (chg_state x0 x1 x2 (x3, x4, x5) w_grid d_list) f_grid obj_grid s0 s1 (head_ xs) location
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid f_grid obj_grid s0 s1 3 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_grid run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5) ++ " 6: " ++ show (d_list !! x6))
  run_gplc (tail_ xs) d_list (chg_grid x0 (x1, x2, x3) (x4, x5, x6) w_grid def_w_grid d_list) f_grid obj_grid s0 s1 (head_ xs) location
run_gplc (x0:x1:x2:x3:xs) d_list w_grid f_grid obj_grid s0 s1 4 location =
  let sig = send_signal 0 x0 (x1, x2, x3) obj_grid s1 d_list
  in do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nsend_signal run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  run_gplc (tail_ xs) d_list w_grid f_grid (fst sig) s0 (snd sig) (head_ xs) location
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid f_grid obj_grid s0 s1 5 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_value run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5))
  run_gplc (tail_ xs) d_list w_grid f_grid (chg_value x0 x1 x2 (x3, x4, x5) d_list obj_grid) s0 s1 (head_ xs) location
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid f_grid obj_grid s0 s1 6 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_floor run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5))
  run_gplc (tail_ xs) d_list w_grid (chg_floor x0 x1 x2 (x3, x4, x5) f_grid d_list) obj_grid s0 s1 (head_ xs) location
run_gplc (x0:x1:x2:xs) d_list w_grid f_grid obj_grid s0 s1 7 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_ps1 run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2))
  run_gplc (tail_ xs) d_list w_grid f_grid obj_grid s0 (chg_ps1 x0 x1 x2 d_list s1) (head_ xs) location
run_gplc (x0:x1:x2:x3:xs) d_list w_grid f_grid obj_grid s0 s1 8 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_obj_type run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  run_gplc (tail_ xs) d_list w_grid f_grid (chg_obj_type x0 (x1, x2, x3) d_list obj_grid) s0 s1 (head_ xs) location
run_gplc (x0:xs) d_list w_grid f_grid obj_grid s0 s1 9 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  run_gplc (tail_ xs) d_list w_grid f_grid obj_grid s0 (place_hold x0 d_list s1) (head_ xs) location
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid f_grid obj_grid s0 s1 10 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_grid_ run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5) ++ " 6: " ++ show (d_list !! x6))
  run_gplc (tail_ xs) d_list w_grid f_grid (chg_grid x0 (x1, x2, x3) (x4, x5, x6) obj_grid def_obj_grid d_list) s0 s1 (head_ xs) location
run_gplc (x0:x1:x2:x3:xs) d_list w_grid f_grid obj_grid s0 s1 11 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\ncopy_ps1 run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  run_gplc (tail_ xs) d_list w_grid f_grid (copy_ps1 x0 (x1, x2, x3) s1 obj_grid d_list) s0 s1 (head_ xs) location
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid f_grid obj_grid s0 s1 12 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\ncopy_lstate run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5) ++ " 6: " ++ show (d_list !! x6))
  run_gplc (tail_ xs) d_list w_grid f_grid (copy_lstate x0 (x1, x2, x3) (x4, x5, x6) w_grid obj_grid d_list) s0 s1 (head_ xs) location
run_gplc (x:xs) d_list w_grid f_grid obj_grid s0 s1 13 location =
  let pass_msg' = pass_msg x xs s1 d_list
  in do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\npass_msg run with arguments " ++ "msg_length: " ++ show (d_list !! x) ++ " message data: " ++ show (take (d_list !! x) xs))
  run_gplc (tail_ (fst pass_msg')) d_list w_grid f_grid obj_grid s0 (snd pass_msg') (head_ (fst pass_msg')) location
run_gplc (x0:x1:x2:xs) d_list w_grid f_grid obj_grid s0 s1 14 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_ps0 run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2))
  run_gplc (tail_ xs) d_list w_grid f_grid obj_grid (chg_ps0 x0 x1 x2 d_list s0) s1 (head_ xs) location
run_gplc (x0:x1:x2:x3:xs) d_list w_grid f_grid obj_grid s0 s1 15 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\ncopy_ps0 run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  run_gplc (tail_ xs) d_list w_grid f_grid (copy_ps0 x0 (x1, x2, x3) s0 obj_grid d_list) s0 s1 (head_ xs) location
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid f_grid obj_grid s0 s1 16 location = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_save_grid run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  run_gplc (tail_ xs) d_list w_grid f_grid obj_grid s0 (chg_save_grid x0 (x1, x2, x3) (x4, x5, x6) s1 d_list) (head_ xs) location

report_state :: Bool -> Int -> [Int] -> [Int] -> [Char] -> IO ()
report_state False mode prog d_list message__ = return ()
report_state True 0 prog d_list message__ = do
  putStr ("\nProgram list: " ++ show prog)
  putStr ("\nData list: " ++ show d_list)
report_state True 1 prog d_list message__ = putStr ("\nProgram list: " ++ show prog)
report_state True 2 prog d_list message__ = putStr message__

link_gplc0 :: [Float] -> [Int] -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> Play_state0 -> Play_state1 -> Bool -> IO (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) (Int, [Int]), Play_state0, Play_state1)
link_gplc0 (x0:x1:xs) (z0:z1:z2:zs) w_grid f_grid obj_grid s0 s1 swap_flag =
  let obj_grid0' = (send_signal 1 1 (z0, z1, z2 + 1) obj_grid s1 [])
      obj_grid1' = (send_signal 1 1 (z0, z1 + 1, z2) obj_grid s1 [])
      obj_grid2' = (send_signal 1 1 (z0, z1, z2 - 1) obj_grid s1 [])
      obj_grid3' = (send_signal 1 1 (z0, z1 - 1, z2) obj_grid s1 [])
  in do
  if sig_q s1 == [] && swap_flag == False then link_gplc0 (x0:x1:xs) (z0:z1:z2:zs) w_grid f_grid obj_grid s0 (s1 {sig_q = next_sig_q s1, next_sig_q = []}) True
  else if swap_flag == True then 
    if (x1 == 1 || x1 == 3) && x0 == 0 && head (snd (obj_grid ! (z0, z1, z2 + 1))) == 0 then do
      putStr ("\nPlayer starts GPLC program at Obj_grid " ++ show (z0, z1, z2 + 1))
      run_gplc (snd ((fst obj_grid0') ! (z0, z1, z2 + 1))) [] w_grid f_grid (fst obj_grid0') s0 s1 0 (z0, z1, z2 + 1)
    else if (x1 == 1 || x1 == 3) && x0 == 1 && head (snd (obj_grid ! (z0, z1 + 1, z2))) == 0 then do
      putStr ("\nPlayer starts GPLC program at Obj_grid " ++ show (z0, z1 + 1, z2))
      run_gplc (snd ((fst obj_grid1') ! (z0, z1 + 1, z2))) [] w_grid f_grid (fst obj_grid1') s0 s1 0 (z0, z1 + 1, z2)
    else if (x1 == 1 || x1 == 3) && x0 == 2 && head (snd (obj_grid ! (z0, z1, z2 - 1))) == 0 then do
      putStr ("\nPlayer starts GPLC program at Obj_grid " ++ show (z0, z1, z2 - 1))
      run_gplc (snd ((fst obj_grid2') ! (z0, z1, z2 - 1))) [] w_grid f_grid (fst obj_grid2') s0 s1 0 (z0, z1, z2 - 1)
    else if (x1 == 1 || x1 == 3) && x0 == 3 && head (snd (obj_grid ! (z0, z1 - 1, z2))) == 0 then do
      putStr ("\nPlayer starts GPLC program at Obj_grid " ++ show (z0, z1 - 1, z2))
      run_gplc (snd ((fst obj_grid3') ! (z0, z1 - 1, z2))) [] w_grid f_grid (fst obj_grid3') s0 s1 0 (z0, z1 - 1, z2)
    else return (w_grid, f_grid, obj_grid, s0, s1)
  else do
    run_gplc' <- run_gplc (snd (obj_grid ! ((sig_q s1) !! 0, (sig_q s1) !! 1, (sig_q s1) !! 2))) [] w_grid f_grid obj_grid s0 (s1 {sig_q = drop 3 (sig_q s1)}) 0 ((sig_q s1) !! 0, (sig_q s1) !! 1, (sig_q s1) !! 2)
    putStr ("\nGPLC program run at Obj_grid " ++ show ((sig_q s1) !! 0, (sig_q s1) !! 1, (sig_q s1) !! 2))
    link_gplc0 (x0:x1:xs) (z0:z1:z2:zs) (fst_ run_gplc') (snd_ run_gplc') (third run_gplc') (fourth run_gplc') (fifth run_gplc') False

link_gplc1 :: Play_state0 -> Play_state1 -> Array (Int, Int, Int) (Int, [Int]) -> Int -> IO (Array (Int, Int, Int) (Int, [Int]), Play_state1)
link_gplc1 s0 s1 obj_grid mode =
  let dest0 = (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0))
      dest1 = [truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)]
  in do
  if mode == 0 then 
    if fst (obj_grid ! dest0) == 1 || fst (obj_grid ! dest0) == 3 then do
      putStr ("\nlink_gplc1: signal sent to Obj_grid " ++ show dest0)
      return (fst (send_signal 1 1 dest0 obj_grid s1 []), s1 {sig_q = sig_q s1 ++ dest1})
    else return (obj_grid, s1)
  else
    if fst (obj_grid ! dest0) == 1 || fst (obj_grid ! dest0) == 3 then do
      putStr ("\nlink_gplc1: signal sent to Obj_grid " ++ show dest0)
      return (fst (send_signal 1 1 dest0 obj_grid s1 []), s1 {sig_q = sig_q s1 ++ dest1, health = (health s1) - 25, state_chg = 1, message = 0 : msg13})
    else return (obj_grid, s1 {health = (health s1) - 25, state_chg = 1, message = 0 : msg13})

-- These functions handle game physics, including collision detection, thrust, friction and gravity
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

thrust :: Int -> Int -> Float -> Float -> UArray (Int, Int) Float -> [Float]
thrust dir a force f_rate look_up =
  if dir == 3 then transform [force / f_rate, 0, 0, 1] (rotation_w a look_up)
  else if dir == 4 then transform [force / f_rate, 0, 0, 1] (rotation_w (mod_angle a 471) look_up)
  else if dir == 5 then transform [force / f_rate, 0, 0, 1] (rotation_w (mod_angle a 314) look_up)
  else transform [force / f_rate, 0, 0, 1] (rotation_w (mod_angle a 157) look_up)

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
  else (x + y / f_rate + f * x / f_rate) : update_vel xs ys zs f_rate f

pause_text :: Play_state1 -> [(Int, [Int])]
pause_text s1 =
  [(0, msg9), (0, []), (0, msg1 ++ conv_msg (health s1)), (0, msg2 ++ conv_msg (ammo s1)), (0, msg3 ++ conv_msg (gems s1)), (0, msg4 ++ conv_msg (torches s1)), (0, msg5 ++ keys s1), (0, msg6 ++ region s1), (0, []), (1, msg10), (2, msg17), (3, msg11), (4, msg12)]

report0 :: Int -> [Float] -> [Float] -> Int -> IO ()
report0 s pos vel a = do
  putStr ("\nsection: " ++ (show s) ++ " pos: " ++ (show pos) ++ " vel: " ++ (show vel) ++ " a: " ++ show a)

-- This is the central function for updating of the game state
update_play :: Io_box -> MVar (Play_state0, Array (Int, Int, Int) Wall_grid) -> Play_state0 -> Play_state1 -> Bool -> Float -> (Float, Float, Float, Float) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> UArray (Int, Int) Float -> Array Int [Char] -> IO ()
update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg =
  let det = detect_coll (truncate (pos_w s0)) (pos_u s0, pos_v s0) ((vel s0) !! 0 / f_rate, (vel s0) !! 1 / f_rate) obj_grid w_grid
      floor = floor_surf (det !! 0) (det !! 1) (pos_w s0) f_grid
      vel_0 = update_vel (vel s0) [0, 0, 0] ((drop 2 det) ++ [0]) f_rate f
      vel_2 = update_vel (vel s0) [0, 0, g] ((drop 2 det) ++ [0]) f_rate 0
      game_t' = game_t s0 + 1
  in do
  control <- messagePump (hwnd_ io_box)
  link0 <- link_gplc0 (drop 4 det) [truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)] w_grid f_grid obj_grid s0 s1 False
  link1 <- link_gplc1 s0 s1 obj_grid 0
  link1_ <- link_gplc1 s0 s1 obj_grid 1
  if control == 2 then do
    choice <- run_menu (pause_text s1) [] io_box (-0.75) (-0.75) 1 0 0
    if choice == 1 then update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg
    else if choice == 2 then do
      save_game0 io_box w_grid f_grid obj_grid s0 s1 conf_reg
      update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg
    else if choice == 3 then do
      putMVar state_ref (s0 {msg_count = -1}, w_grid)
      update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg
    else do
      putMVar state_ref (s0 {msg_count = -3}, w_grid)
      update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg
  else if control == 10 then update_play io_box state_ref s0 (s1 {sig_q = sig_q s1 ++ [0, 0, 0]}) in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid (fst (send_signal 1 2 (0, 0, 0) obj_grid s1 [])) look_up conf_reg
  else if control == 11 then do
    if view_mode s0 == 0 then update_play io_box state_ref (s0 {view_mode = 1}) s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg
    else update_play io_box state_ref (s0 {view_mode = 0}) s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg
  else if control == 12 then update_play io_box state_ref (s0 {view_angle = mod_angle (view_angle s0) 5}) s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg
  else if message s1 /= [] then do
    event <- proc_msg0 (message s1) s0 s1 obj_grid io_box
    putMVar state_ref (fst__ event, w_grid)
    update_play io_box state_ref ((fst__ event) {msg_count = 0}) (snd__ event) in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid (third_ event) look_up conf_reg
  else
    if in_flight == False then
      if (pos_w s0) - floor > 0.01 then do
        report0 0 [pos_u s0, pos_v s0, pos_w s0] (vel s0) (angle s0)
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1}, w_grid)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, vel = vel_0, game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg
      else if control > 2 && control < 7 then do
        report0 1 [pos_u s0, pos_v s0, pos_w s0] (vel s0) (angle s0)
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor}, w_grid)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = update_vel (vel s0) (take 3 (thrust (fromIntegral control) (angle s0) mag_r f_rate look_up)) ((drop 2 det) ++ [0]) f_rate f, game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg
      else if control == 7 then do
        report0 2 [pos_u s0, pos_v s0, pos_w s0] (vel s0) (angle s0)
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, angle = mod_angle (angle s0) 5}, w_grid)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, angle = mod_angle (angle s0) 5, game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg
      else if control == 8 then do
        report0 3 [pos_u s0, pos_v s0, pos_w s0] (vel s0) (angle s0)
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, angle = mod_angle (angle s0) (-5)}, w_grid)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, angle = mod_angle (angle s0) (-5), game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg
      else if control == 9 then do
        report0 4 [pos_u s0, pos_v s0, pos_w s0] (vel s0) (angle s0)
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor + mag_j / f_rate}, w_grid)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor + mag_j / f_rate, vel = (take 2 vel_0) ++ [mag_j], game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg
      else do
        report0 5 [pos_u s0, pos_v s0, pos_w s0] (vel s0) (angle s0)
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor}, w_grid)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg
    else if in_flight == True && (pos_w s0) > floor then
      if control == 7 then do
        report0 6 [pos_u s0, pos_v s0, pos_w s0] (vel s0) (angle s0)
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, angle = mod_angle (angle s0) 5}, w_grid)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, vel = vel_2, angle = mod_angle (angle s0) 5, game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg
      else if control == 8 then do
        report0 7 [pos_u s0, pos_v s0, pos_w s0] (vel s0) (angle s0)
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, angle = mod_angle (angle s0) (-5)}, w_grid)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, vel = vel_2, angle = mod_angle (angle s0) (-5), game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg
      else do
        report0 8 [pos_u s0, pos_v s0, pos_w s0] (vel s0) (angle s0)
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate}, w_grid)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, vel = vel_2, game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg
    else do
      report0 9 [pos_u s0, pos_v s0, pos_w s0] (vel s0) (angle s0)
      putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor}, w_grid)
      if (vel s0) !! 2 < -4 then do
        update_play io_box state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, game_t = game_t'}) (snd link1_) False f_rate (g, f, mag_r, mag_j) w_grid f_grid (fst link1_) look_up conf_reg
      else do
        update_play io_box state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, game_t = game_t'}) (snd link1) False f_rate (g, f, mag_r, mag_j) w_grid f_grid (fst link1) look_up conf_reg

-- These functions handle game events triggered by a call to pass_msg within a GPLC program.  These include on screen messages, object interaction menus and level completion.
conv_msg :: Int -> [Int]
conv_msg v =
  if v < 10 then [(mod v 10) + 53]
  else if v < 100 then [(div v 10) + 53, (mod v 10) + 53]
  else [(div v 100) + 53, (div (v - 100) 10) + 53, (mod v 10) + 53]

char_list = "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 ,'.?;:+-=!<"

find_tile :: [Char] -> Char -> Int -> Int
find_tile [] t i = i
find_tile (x:xs) t i =
  if x == t then i
  else find_tile xs t (i + 1)

conv_msg_ :: [Char] -> [Int]
conv_msg_ [] = []
conv_msg_ (x:xs) = find_tile char_list x 0 : conv_msg_ xs

proc_msg1 :: [[Int]] -> [(Int, [Int])]
proc_msg1 [] = []
proc_msg1 (x:xs) = (head x, tail x) : proc_msg1 xs

proc_msg0 :: [Int] -> Play_state0 -> Play_state1 -> Array (Int, Int, Int) (Int, [Int]) -> Io_box -> IO (Play_state0, Play_state1, Array (Int, Int, Int) (Int, [Int]))
proc_msg0 msg s0 s1 obj_grid io_box =
  let signal_ = (head (splitOn [-1] (tail msg)))
  in do
  if head msg == 0 && state_chg s1 == 1 && health s1 <= 0 then return (s0 {message_ = [(0, msg8)], msg_count = -2}, s1, obj_grid)
  else if head msg == 0 && state_chg s1 == 1 then return (s0 {message_ = [(0, (tail msg) ++ msg1 ++ conv_msg (health s1))], msg_count = 300}, s1 {state_chg = 0, message = []}, obj_grid)
  else if head msg == 0 && state_chg s1 == 2 then return (s0 {message_ = [(0, (tail msg) ++ msg2 ++ conv_msg (ammo s1))], msg_count = 300}, s1 {state_chg = 0, message = []}, obj_grid)
  else if head msg == 0 && state_chg s1 == 3 then return (s0 {message_ = [(0, (tail msg) ++ msg3 ++ conv_msg (gems s1))], msg_count = 300}, s1 {state_chg = 0, message = []}, obj_grid)
  else if head msg == 0 && state_chg s1 == 4 then return (s0 {message_ = [(0, (tail msg) ++ msg4 ++ conv_msg (torches s1))], msg_count = 300}, s1 {state_chg = 0, message = []}, obj_grid)
  else if head msg < 0 then return (s0 {message_ = [(0, tail msg)], msg_count = head msg}, s1, obj_grid)
  else if head msg == 1 then do
    choice <- run_menu (proc_msg1 (tail (splitOn [-1] (tail msg)))) [] io_box 0.1 0.1 1 0 0
    return (s0, s1 {sig_q = sig_q s1 ++ [signal_ !! 0, signal_ !! 1, signal_ !! 2], message = []}, fst (send_signal 1 (choice + 1) (signal_ !! 0, signal_ !! 1, signal_ !! 2) obj_grid s1 []))
  else return (s0 {message_ = [(0, tail msg)], msg_count = 480}, s1 {message = []}, obj_grid)

run_menu :: [(Int, [Int])] -> [(Int, [Int])] -> Io_box -> Float -> Float -> Int -> Int -> Int -> IO Int
run_menu [] acc io_box x y c c_max 0 = run_menu acc [] io_box x y c c_max 2
run_menu (n:ns) acc io_box x y c c_max 0 = do
  if fst n == 0 then run_menu ns (acc ++ [n]) io_box x y c c_max 0
  else run_menu ns (acc ++ [n]) io_box x y c (c_max + 1) 0
run_menu [] acc io_box x y c c_max d = do
  swapBuffers (hdc_ io_box)
  sleep 33
  control <- messagePump (hwnd_ io_box)
  if control == 3 && c > 1 then do
    glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    run_menu acc [] io_box x 0.1 (c - 1) c_max 2
  else if control == 5 && c < c_max then do
    glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    run_menu acc [] io_box x 0.1 (c + 1) c_max 2
  else if control == 2 then return c
  else do
    glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    run_menu acc [] io_box x 0.1 c c_max 2
run_menu (n:ns) acc io_box x y c c_max d = do
  if d == 2 then do
    glBindVertexArray ((fst (p_bind_ io_box)) ! 1017)
    glBindTexture gl_TEXTURE_2D ((fst (p_bind_ io_box)) ! 1018)
    gl_program <- peekElemOff (fst (p_bind_ io_box)) ((snd (p_bind_ io_box)) - 3)
    glUseProgram gl_program
    glUniform1i (fromIntegral ((uniform_ io_box) ! 38)) 0
    p_tt_matrix <- mallocBytes (glfloat * 16)
    load_array [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1] p_tt_matrix 0
    glUniformMatrix4fv (fromIntegral ((uniform_ io_box) ! 36)) 1 1 p_tt_matrix
    glDrawElements gl_TRIANGLES 6 gl_UNSIGNED_SHORT zero_ptr
    free p_tt_matrix
  else return ()
  vao <- peekElemOff (fst (p_bind_ io_box)) 933
  glBindVertexArray vao
  p_tt_matrix <- mallocBytes ((length (snd n)) * glfloat * 16)
  if fst n == c then show_text (snd n) 1 933 (uniform_ io_box) (p_bind_ io_box) p_tt_matrix x y 0
  else show_text (snd n) 0 933 (uniform_ io_box) (p_bind_ io_box) p_tt_matrix x y 0
  free p_tt_matrix
  run_menu ns (acc ++ [n]) io_box x (y - 0.05) c c_max 1

-- This function handles the drawing of characters (letters and numbers) that are used for in game messages and in menus
show_text :: [Int] -> Int -> Int -> UArray Int Int32 -> (Ptr GLuint, Int) -> Ptr GLfloat -> Float -> Float -> Int -> IO ()
show_text [] mode base uniform p_bind p_tt_matrix x y offset = return ()
show_text (m:ms) mode base uniform p_bind p_tt_matrix x y offset = do
  load_array (MAT.toList (translation x y 0)) (castPtr p_tt_matrix) offset
  if mode == 0 && x < 83 then do
    glUniformMatrix4fv (unsafeCoerce (uniform ! 36) :: GLint) 1 1 (plusPtr p_tt_matrix (offset * glfloat))
    glUniform1i (unsafeCoerce (uniform ! 38) :: GLint) 0
  else if mode == 1 && x < 83 then do
    glUniformMatrix4fv (unsafeCoerce (uniform ! 36) :: GLint) 1 1 (plusPtr p_tt_matrix (offset * glfloat))
    glUniform1i (unsafeCoerce (uniform ! 38) :: GLint) 1
  else do
    putStr "show_text: Invalid mode or character reference in text line..."
    show_text ms mode base uniform p_bind p_tt_matrix (x + 0.05) y (offset + 16)
  tex <- peekElemOff (fst p_bind) (base + m)
  glBindTexture gl_TEXTURE_2D tex
  glDrawElements gl_TRIANGLES 6 gl_UNSIGNED_SHORT zero_ptr
  show_text ms mode base uniform p_bind p_tt_matrix (x + 0.05) y (offset + 16)

