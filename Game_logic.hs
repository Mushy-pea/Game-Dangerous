-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module Game_logic where

import System.IO
import System.IO.Unsafe
import System.Exit
import Graphics.GL.Core33
import Foreign
import System.Win32
import Graphics.Win32
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Fixed
import Data.Foldable
import qualified Data.Sequence as SEQ
import Control.Concurrent
import Control.Exception
import qualified Data.Matrix as MAT
import Unsafe.Coerce
import Data.Coerce
import Build_model
import Game_sound

foreign import ccall "wingdi.h SwapBuffers"
  swapBuffers :: HDC -> IO Bool

-- Used to load C style arrays, which are used with certain OpenGL functions.
load_array :: Storable a => [a] -> Ptr a -> Int -> IO ()
load_array [] p i = return ()
load_array (x:xs) p i = do
  pokeElemOff p i x
  load_array xs p (i + 1)

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
msg29 = [15, 47, 29, 34, 66, 66, 66, 3, 31, 40, 46, 35, 42, 31, 30, 31, 63, 28, 35, 46, 31, 73]

main_menu_text :: [(Int, [Int])]
main_menu_text = [(0, msg16), (0, []), (1, msg14), (2, msg18), (3, msg12)]

-- These two functions are where the program interacts with the Windows message system and captures keyboard input.
wndProc :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc hwnd wmsg wParam lParam
    | wmsg == 258 = if wParam == 120 then return 2 -- pause and select menu option (X)
                    else if wParam == 121 then return 14 -- exit and save log file (Y)
                    else if wParam == 97 then return 6  -- left (A)
                    else if wParam == 100 then return 4 -- right (D)
                    else if wParam == 115 then return 5 -- back (S)
                    else if wParam == 119 then return 3 -- forward (W)
                    else if wParam == 107 then return 7 -- turn left (K)
                    else if wParam == 108 then return 8 -- turn right (L)
                    else if wParam == 117 then return 9 -- jump (U)
                    else if wParam == 116 then return 10 -- light torch (T)
                    else if wParam == 99 then return 11 -- switch view mode (C)
                    else if wParam == 118 then return 12 -- rotate 3rd person view (V)
                    else if wParam == 32 then return 13 -- fire (SPACE)
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

-- These three functions perform GPLC conditional expression folding, evaluating conditional op - codes at the start of a GPLC program run to yield unconditional code.
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
if1 v arg0 arg1 code0 code1 d_list =
  if d_list !! arg0 > (d_list !! arg1) + (v - 2) then code0
  else code1

if0 :: [Int] -> [Int] -> [Int]
if0 [] d_list = []
if0 code d_list =
  if code !! 0 == 1 then if0 (if1 (code !! 1) (code !! 2) (code !! 3) (take (code !! 4) (drop 6 code)) (take (code !! 5) (drop (6 + code !! 4) code)) d_list) d_list
  else code

-- The remaining GPLC op - codes are implemented here.  The GPLC specification document explains their functions in the context of a game logic virtual machine.
chg_state :: [Int] -> (Int, Int, Int) -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> UArray Int Int -> [((Int, Int, Int), Wall_grid)] -> [Int] -> SEQ.Seq Char -> ([((Int, Int, Int), Wall_grid)], [Int], SEQ.Seq Char)
chg_state (2:x1:x2:x3:x4:x5:x6:x7:x8:x9:xs) (i0, i1, i2) (i3, i4, i5) w_grid update w_grid_upd d_list log =
  let m0 = SEQ.fromList "\nchg_state run with arguments "
      m1 = SEQ.fromList ("0: " ++ show (d_list !! x1) ++ " 1: " ++ show (d_list !! x2) ++ " 2: " ++ show (d_list !! x3) ++ " 3: " ++ show (d_list !! x4) ++ " 4: " ++ show (d_list !! x5) ++ " 5: " ++ show (d_list !! x6) ++ " 6: " ++ show (d_list !! x7) ++ " 7: " ++ show (d_list !! x8) ++ " 8: " ++ show (d_list !! x9))
  in
  if d_list !! x1 == 0 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(0, d_list !! x2), (1, d_list !! x3)]) w_grid_upd d_list (log SEQ.>< m0 SEQ.>< m1)
  else if d_list !! x1 == 1 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(2, d_list !! x2), (3, d_list !! x3)]) w_grid_upd d_list (log SEQ.>< m0 SEQ.>< m1)
  else if d_list !! x1 == 2 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(4, d_list !! x2), (5, d_list !! x3)]) w_grid_upd d_list (log SEQ.>< m0 SEQ.>< m1)
  else if d_list !! x1 == 3 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(6, d_list !! x2), (7, d_list !! x3)]) w_grid_upd d_list (log SEQ.>< m0 SEQ.>< m1)
  else if d_list !! x1 == 9 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(8, d_list !! x2), (9, d_list !! x3)]) w_grid_upd d_list (log SEQ.>< m0 SEQ.>< m1)
  else if d_list !! x1 == 10 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(10, d_list !! x2), (11, d_list !! x3)]) w_grid_upd d_list (log SEQ.>< m0 SEQ.>< m1)
  else if d_list !! x1 == 11 then chg_state xs (x4, x5, x6) (x7, x8, x9) w_grid (update // [(12, d_list !! x2), (13, d_list !! x3)]) w_grid_upd d_list (log SEQ.>< m0 SEQ.>< m1)
  else throw Invalid_GPLC_op_argument
chg_state code (i0, i1, i2) (i3, i4, i5) w_grid update w_grid_upd d_list log =
  let source = (d_list !! i0, d_list !! i1, d_list !! i2)
      dest = (d_list !! i3, d_list !! i4, d_list !! i5)
      grid_i = fromJust (obj (w_grid ! source))
      grid_i' = (obj (w_grid ! source))
  in 
  if isNothing grid_i' == True then (w_grid_upd, code, log)
  else ((dest, (w_grid ! source) {obj = Just (grid_i {ident_ = upd (update ! 0) (ident_ grid_i) (update ! 1), u__ = upd (update ! 2) (u__ grid_i) (int_to_float (update ! 3)), v__ = upd (update ! 4) (v__ grid_i) (int_to_float (update ! 5)), w__ = upd (update ! 6) (w__ grid_i) (int_to_float (update ! 7)), texture__ = upd (update ! 8) (texture__ grid_i) (update ! 9), num_elem = upd (update ! 10) (num_elem grid_i) (fromIntegral (update ! 11)), obj_flag = upd (update ! 12) (obj_flag grid_i) (update ! 13)})}) : w_grid_upd, code, log)

chg_grid :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> Wall_grid -> [((Int, Int, Int), Wall_grid)] -> [Int] -> [((Int, Int, Int), Wall_grid)]
chg_grid mode (i0, i1, i2) (i3, i4, i5) w_grid def w_grid_upd d_list =
  let dest0 = (d_list !! i0, d_list !! i1, d_list !! i2)
      dest1 = (d_list !! i3, d_list !! i4, d_list !! i5)
  in
  if d_list !! mode == 0 then (dest0, def) : w_grid_upd
  else if d_list !! mode == 1 then [(dest1, w_grid ! dest0), (dest0, def)] ++ w_grid_upd
  else (dest1, w_grid ! dest0) : w_grid_upd

chg_grid_ :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
chg_grid_ mode (i0, i1, i2) (i3, i4, i5) obj_grid_upd d_list =
  let source = (d_list !! i0, d_list !! i1, d_list !! i2)
      dest = (d_list !! i3, d_list !! i4, d_list !! i5)
  in
  if d_list !! mode == 0 then (source, (-1, [])) : obj_grid_upd
  else if d_list !! mode == 1 then (source, (-2, [])) : (dest, (-2, [])) : obj_grid_upd
  else (source, (-3, [])) : (dest, (-3, [])) : obj_grid_upd

chg_floor :: Int -> Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) Floor_grid -> [Int] -> Array (Int, Int, Int) Floor_grid
chg_floor state_val abs v (i0, i1, i2) grid d_list =
  let index = (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  if d_list !! state_val == 0 then grid // [(index, (grid ! index) {w_ = upd (d_list !! abs) (w_ (grid ! index)) (int_to_float (d_list !! v))})]
  else grid // [(index, (grid ! index) {surface = int_to_surface (d_list !! v)})]

chg_value :: Int -> Int -> Int -> (Int, Int, Int) -> [Int] -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
chg_value val abs v (i0, i1, i2) d_list obj_grid obj_grid_upd =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  if val == 536870910 then ((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, [(0, v)])) : obj_grid_upd
  else ((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, [(val, upd (d_list !! abs) ((snd target) !! val) (d_list !! v))])) : obj_grid_upd

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
  else if d_list !! state_val == 4 then s {keys = (take (d_list !! abs) (keys s)) ++ [d_list !! v] ++ drop ((d_list !! abs) + 1) (keys s), state_chg = 5}
  else if d_list !! state_val == 5 then s {difficulty = ("Hey, not too risky!", 6, 8, 10)}
  else if d_list !! state_val == 6 then s {difficulty = ("Plenty of danger please.", 6, 10, 14)}
  else if d_list !! state_val == 7 then s {difficulty = ("Ultra danger.", 10, 15, 20)}
  else s {difficulty = ("Health and safety nightmare!", 15, 20, 25)}

copy_ps0 :: Int -> (Int, Int, Int) -> Play_state0 -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
copy_ps0 offset (i0, i1, i2) s0 obj_grid obj_grid_upd d_list =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
      v0 = (offset, fl_to_int (pos_u s0))
      v1 = (offset + 1, fl_to_int (pos_v s0))
      v2 = (offset + 2, fl_to_int (pos_w s0))
      v3 = (offset + 3, fl_to_int ((vel s0) !! 0))
      v4 = (offset + 4, fl_to_int ((vel s0) !! 1))
      v5 = (offset + 5, fl_to_int ((vel s0) !! 2))
      v6 = (offset + 6, angle s0)
      v7 = (offset + 7, rend_mode s0)
      v8 = (offset + 8, game_t s0)
      v9 = (offset + 9, torch_t0 s0)
      v10 = (offset + 10, torch_t_limit s0)      
  in ((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, [v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10])) : obj_grid_upd

copy_ps1 :: Int -> (Int, Int, Int) -> Play_state1 -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
copy_ps1 offset (i0, i1, i2) s1 obj_grid obj_grid_upd d_list =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  ((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, [(offset, health s1), (offset + 1, ammo s1), (offset + 2, gems s1), (offset + 3, torches s1), (offset + 4, (keys s1) !! 0), (offset + 5, (keys s1) !! 1), (offset + 6, (keys s1) !! 2), (offset + 7, (keys s1) !! 3), (offset + 8, (keys s1) !! 4), (offset + 9, (keys s1) !! 5)])) : obj_grid_upd

obj_type :: Int -> Int -> Int -> Array (Int, Int, Int) (Int, [Int]) -> Int
obj_type w u v obj_grid =
  if u < 0 || v < 0 then 2
  else if bound_check u 0 (bounds obj_grid) == False then 2
  else if bound_check v 1 (bounds obj_grid) == False then 2
  else fst (obj_grid ! (w, u, v))

copy_lstate :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
copy_lstate offset (i0, i1, i2) (i3, i4, i5) w_grid obj_grid obj_grid_upd d_list =
  let w = (d_list !! i3)
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
      target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
      c0 = (offset, obj_type w u v obj_grid)
      c1 = (offset + 1, obj_type w u' v obj_grid)
      c2 = (offset + 2, obj_type w u'' v obj_grid)
      c3 = (offset + 3, obj_type w u v' obj_grid)
      c4 = (offset + 4, obj_type w u v'' obj_grid)
      c5 = (offset + 5, obj_type w u' v' obj_grid)
      c6 = (offset + 6, obj_type w u'' v' obj_grid)
      c7 = (offset + 7, obj_type w u' v'' obj_grid)
      c8 = (offset + 8, obj_type w u'' v'' obj_grid)
  in ((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, [c0, c1, c2, c3, c4, c5, c6, c7, c8, w_conf_u1, w_conf_u2, w_conf_v1, w_conf_v2])) : obj_grid_upd

chg_obj_type :: Int -> (Int, Int, Int) -> [Int] -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
chg_obj_type v (i0, i1, i2) d_list obj_grid obj_grid_upd =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  ((d_list !! i0, d_list !! i1, d_list !! i2), (d_list !! v, [])) : obj_grid_upd

pass_msg :: Int -> [Int] -> Play_state1 -> [Int] -> ([Int], Play_state1)
pass_msg len msg s d_list = (drop (d_list !! len) msg, s {message = message s ++ [head msg] ++ [(d_list !! len) - 1] ++ take ((d_list !! len) - 1) (tail msg)})

send_signal :: Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) (Int, [Int]) -> Play_state1 -> [Int] -> (Array (Int, Int, Int) (Int, [Int]), Play_state1)
send_signal 0 sig (i0, i1, i2) obj_grid s1 d_list =
  let dest = (d_list !! i0, d_list !! i1, d_list !! i2)
      prog = (snd (obj_grid ! dest))
  in (obj_grid, s1 {next_sig_q = next_sig_q s1 ++ [d_list !! sig, d_list !! i0, d_list !! i1, d_list !! i2]})
send_signal 1 sig dest obj_grid s1 d_list =
  let prog = (snd (obj_grid ! dest))
  in
  (obj_grid // [(dest, (fst (obj_grid ! dest), (head prog) : sig : drop 2 prog))], s1)

place_hold :: Int -> [Int] -> Play_state1 -> Play_state1
place_hold val d_list s = unsafePerformIO (putStr "\nplace_hold run with value " >> print (d_list !! val) >> return s)

project_init :: Int -> Int -> Int -> Int -> Int -> (Int, Int, Int) -> (Int, Int, Int) -> Int -> Int -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> UArray (Int, Int) Float -> [((Int, Int, Int), (Int, [(Int, Int)]))]
project_init u v w a vel (i0, i1, i2) (i3, i4, i5) offset obj_flag obj_grid obj_grid_upd d_list look_up =
  let source = (d_list !! i0, d_list !! i1, d_list !! i2)
      dest = (d_list !! i3, d_list !! i4, d_list !! i5)
      target = obj_grid ! source
      v0 = (offset, d_list !! i3)
      v1 = (offset + 1, d_list !! i4)
      v2 = (offset + 2, d_list !! i5)
      v3 = (offset + 3, d_list !! u)
      v4 = (offset + 4, d_list !! v)
      v5 = (offset + 5, - (div (d_list !! w) 1000000) - 1)
      v6 = (offset + 6, truncate ((look_up ! (2, d_list !! a)) * fromIntegral (d_list !! vel)))
      v7 = (offset + 7, truncate ((look_up ! (1, d_list !! a)) * fromIntegral (d_list !! vel)))
      v8 = (offset + 8, div (d_list !! u) 1000000)
      v9 = (offset + 9, div (d_list !! v) 1000000)
      v10 = (offset + 10, d_list !! obj_flag)
  in (source, (-3, [])) : (dest, (-3, [v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10])) : obj_grid_upd

project_update :: Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> [Int] -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
project_update p_state p_state' (i0, i1, i2) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 d_list =
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
      grid_i = fromJust (obj (w_grid ! index))
      grid_i' = (obj (w_grid ! index))
  in
  if isNothing grid_i' == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
  else if u_block' == u_block && v_block' == v_block then ((index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}) : w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block && v_block' == v_block + 1 then
    if v2 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (obj_grid ! (w_block_, u_block, v_block + 1)) > 0 && fst (obj_grid ! (w_block_, u_block, v_block + 1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block), (p_state' + 11, v_block + 1)])) : obj_grid_upd, s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block, v_block + 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block, v_block + 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block + 1 && v_block' == v_block then
    if u2 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (obj_grid ! (w_block_, u_block + 1, v_block)) > 0 && fst (obj_grid ! (w_block_, u_block + 1, v_block)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block + 1), (p_state' + 11, v_block)])) : obj_grid_upd, s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block + 1, v_block) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block + 1, v_block), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block && v_block' == v_block - 1 then
    if v1 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (obj_grid ! (w_block_, u_block, v_block - 1)) > 0 && fst (obj_grid ! (w_block_, u_block, v_block - 1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block), (p_state' + 11, v_block - 1)])) : obj_grid_upd, s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block, v_block - 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block, v_block - 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block - 1 && v_block' == v_block then
    if u1 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (obj_grid ! (w_block_, u_block - 1, v_block)) > 0 && fst (obj_grid ! (w_block_, u_block - 1, v_block)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block - 1), (p_state' + 11, v_block)])) : obj_grid_upd, s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block - 1, v_block) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block - 1, v_block), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block + 1 && v_block' == v_block + 1 then
    if u2 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (obj_grid ! (w_block_, u_block + 1, v_block + 1)) > 0 && fst (obj_grid ! (w_block_, u_block + 1, v_block + 1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block + 1), (p_state' + 11, v_block + 1)])) : obj_grid_upd, s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block + 1, v_block + 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block + 1, v_block + 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block + 1 && v_block' == v_block - 1 then
    if v1 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (obj_grid ! (w_block_, u_block + 1, v_block - 1)) > 0 && fst (obj_grid ! (w_block_, u_block + 1, v_block - 1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block + 1), (p_state' + 11, v_block - 1)])) : obj_grid_upd, s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block + 1, v_block - 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block + 1, v_block - 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else if u_block' == u_block - 1 && v_block' == v_block - 1 then
    if u1 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (obj_grid ! (w_block_, u_block - 1, v_block - 1)) > 0 && fst (obj_grid ! (w_block_, u_block - 1, v_block - 1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block - 1), (p_state' + 11, v_block - 1)])) : obj_grid_upd, s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block - 1, v_block - 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block - 1, v_block - 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)
  else
    if v2 (w_grid ! (w_block_, u_block, v_block)) == True then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1)
    else if fst (obj_grid ! (w_block_, u_block - 1, v_block + 1)) > 0 && fst (obj_grid ! (w_block_, u_block - 1, v_block + 1)) < 4 then ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 2), (p_state' + 9, w_block_), (p_state' + 10, u_block - 1), (p_state' + 11, v_block + 1)])) : obj_grid_upd, s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block - 1, v_block + 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) s0 <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else ((index, def_w_grid) : w_grid_upd, (location, (fst target, [(p_state' + 8, 1)])) : obj_grid_upd, s1 {health = health s1 - det_damage (difficulty s1) s0, state_chg = 1, message = 0 : msg25})
    else ([((w_block, u_block - 1, v_block + 1), (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})}), (index, def_w_grid)] ++ w_grid_upd, (location, (fst target, [(p_state', u'), (p_state' + 1, v')])) : obj_grid_upd, s1)

-- Called from project_update, npc_move and npc_damage.  Used to determine the damage taken by the player and non - player characters from adverse events.
det_damage :: ([Char], Int, Int, Int) -> Play_state0 -> Int
det_damage (d, low, med, high) s0 =
  if (prob_seq s0) ! (mod (game_t s0) 240) < 20 then low
  else if (prob_seq s0) ! (mod (game_t s0) 240) > 70 then high
  else med

binary_dice :: Int -> Int -> (Int, Int, Int) -> Int -> Play_state0 -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> [Int] -> [((Int, Int, Int), (Int, [(Int, Int)]))]
binary_dice prob diff (i0, i1, i2) offset s0 obj_grid obj_grid_upd d_list =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  if (prob_seq s0) ! (mod ((game_t s0) + (d_list !! diff)) 240) < d_list !! prob then ((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, [(offset, 1)])) : obj_grid_upd
  else ((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, [(offset, 0)])) : obj_grid_upd

binary_dice_ :: Int -> Play_state0 -> Bool
binary_dice_ prob s0 =
  if (prob_seq s0) ! (mod (game_t s0) 240) < prob then True
  else False  

-- These functions implement non - player character (NPC) logic, which is runnable but still in test as of build 8_08.
init_npc :: Int -> Int -> Play_state1 -> [Int] -> Play_state1
init_npc offset i s1 d_list =
  let i_npc_type = d_list !! offset
      i_c_health = d_list !! (offset + 1)
      i_node_locations = take 6 (drop (offset + 2) d_list)
      i_arr_node_locs = take 6 (drop (offset + 8) d_list)
      i_fg_position = int_to_float_v (d_list !! (offset + 14)) (d_list !! (offset + 15)) (d_list !! (offset + 16))
      i_dir_vector = (int_to_float (d_list !! (offset + 17)), int_to_float (d_list !! (offset + 18)))
      i_direction = d_list !! (offset + 19)
      i_last_dir = d_list !! (offset + 20)
      i_speed = int_to_float (d_list !! (offset + 21))
      i_avoid_dist = d_list !! (offset + 22)
      i_attack_mode = int_to_bool (d_list !! (offset + 23))
      i_fire_prob = d_list !! (offset + 24)
  in 
  s1 {npc_states = (npc_states s1) // [(i, NPC_state {npc_type = i_npc_type, c_health = i_c_health, ticks_left0 = 0, ticks_left1 = 0, node_locations = i_node_locations, arr_node_locs = i_arr_node_locs, fg_position = i_fg_position, dir_vector = i_dir_vector, direction = i_direction, last_dir = i_last_dir, target_u' = 0, target_v' = 0, target_w' = 0, speed = i_speed, avoid_dist = i_avoid_dist, attack_mode = i_attack_mode, final_appr = False, fire_prob = i_fire_prob})]}

-- Used to determine a pseudorandom target destination for an NPC, so it can wander around when not set on attacking the player.
det_rand_target :: Play_state0 -> Int -> Int -> (Int, Int, Int)
det_rand_target s0 u_bound v_bound =
  let n = \i -> (prob_seq s0) ! (mod ((game_t s0) + i) 240)
  in (mod (n 0) 2, mod (((n 1) + 1) * ((n 2) + 1)) u_bound, mod (((n 1) + 1) * ((n 2) + 1)) v_bound)

-- The NPC path finding is based around line of sight checks, which use the Obj_grid ( (Int, [Int]) ) instance of the ray tracer.
chk_line_sight :: Int -> Int -> Int -> Int -> Int -> (Float, Float) -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> UArray (Int, Int) Float -> Int
chk_line_sight mode a w_block u_block v_block (fg_u, fg_v) target_u target_v w_grid f_grid obj_grid look_up =
  let proc_angle_ = proc_angle a
  in
  third_ (ray_trace0 fg_u fg_v (fst__ proc_angle_) (snd__ proc_angle_) (third_ proc_angle_) u_block v_block w_grid f_grid obj_grid look_up w_block [] target_u target_v mode 1)

npc_dir_table :: Int -> Int
npc_dir_table dir = truncate (78.625 * fromIntegral (dir - 1))

npc_dir_remap (-1) = 5
npc_dir_remap (-2) = 1
npc_dir_remap (-3) = 7
npc_dir_remap (-4) = 3
npc_dir_remap (-5) = 1
npc_dir_remap (-6) = 5
npc_dir_remap (-7) = 3
npc_dir_remap (-8) = 7

-- Determine an alternative viable direction if an NPC is blocked from following its primary choice of direction.
another_dir :: [Int] -> Int -> Int -> Int -> Int -> (Float, Float) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> UArray (Int, Int) Float -> Play_state0 -> Int
another_dir [] c w_block u_block v_block (fg_u, fg_v) w_grid f_grid obj_grid look_up s0 = 0
another_dir poss_dirs c w_block u_block v_block (fg_u, fg_v) w_grid f_grid obj_grid look_up s0 =
  let choice = poss_dirs !! (mod ((prob_seq s0) ! (mod ((game_t s0) + c) 240)) c)
  in
  if chk_line_sight 1 (npc_dir_table choice) w_block u_block v_block (fg_u, fg_v) 0 0 w_grid f_grid obj_grid look_up == 0 then choice
  else another_dir (delete choice poss_dirs) (c - 1) w_block u_block v_block (fg_u, fg_v) w_grid f_grid obj_grid look_up s0

-- Non - player characters have 8 possible directions of movement (4 for centipedes).  This function is involved in implementing that restriction.
quantise_angle :: Int -> (Int, Int)
quantise_angle a =
  if a < 79 then (0, 1)
  else if a < 157 then (79, 2)
  else if a < 236 then (157, 3)
  else if a < 314 then (236, 4)
  else if a < 393 then (314, 5)
  else if a < 471 then (393, 6)
  else if a < 550 then (471, 7)
  else (550, 8)

no_cpede_reverse 1 = 5
no_cpede_reverse 3 = 7
no_cpede_reverse 5 = 1
no_cpede_reverse 7 = 3

-- There are two NPC behavioural models; standard creatures and centipedes.  This function contains the logic specific to the centipede model and is called from the more general npc_decision function.
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
    if direction char_state == 5 then cpede_decision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
  else if abs (target_u - u) <= abs (target_v - v) && target_u - u < 0 then
    if direction char_state == 1 then cpede_decision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
  else if abs (target_u - u) > abs (target_v - v) && target_v - v > 0 then
    if direction char_state == 7 then cpede_decision 1 5 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 3 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
  else
    if direction char_state == 3 then cpede_decision 1 1 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
    else cpede_decision 1 7 i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up
cpede_decision 1 choice i target_u target_v w u v w_grid f_grid obj_grid s0 s1 look_up =
  let char_state = (npc_states s1) ! i
      line_sight0 = chk_line_sight 2 (npc_dir_table choice) w u v (snd__ (fg_position char_state), third_ (fg_position char_state)) target_u target_v w_grid f_grid obj_grid look_up
      line_sight1 = chk_line_sight 3 (npc_dir_table choice) w u v (snd__ (fg_position char_state), third_ (fg_position char_state)) target_u target_v w_grid f_grid obj_grid look_up
  in
  if final_appr char_state == True then
    if line_sight0 == 0 then (choice, True && attack_mode char_state)
    else if line_sight0 == 1 then (another_dir (delete (no_cpede_reverse choice) (delete choice [1, 3, 5, 7])) 2 w u v (snd__ (fg_position char_state), third_ (fg_position char_state)) w_grid f_grid obj_grid look_up s0, False)
    else (choice, False)
  else
    if line_sight1 == 1 then (another_dir (delete (no_cpede_reverse choice) (delete choice [1, 3, 5, 7])) 2 w u v (snd__ (fg_position char_state), third_ (fg_position char_state)) w_grid f_grid obj_grid look_up s0, False)
    else (choice, False)

vector_to_angle :: Float -> Float -> Int
vector_to_angle u_comp v_comp =
  let a = truncate ((atan (abs v_comp / abs u_comp)) * 100)
  in
  if u_comp >= 0 && v_comp >= 0 then a
  else if u_comp < 0 && v_comp >= 0 then 314 - a
  else if u_comp < 0 && v_comp < 0 then 314 + a
  else 628 - a

npc_decision :: Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> ([((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
npc_decision 0 flag offset target_w target_u target_v d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 look_up =
  let char_state = (npc_states s1) ! (d_list !! 3)
      s1' = \t0 t1 w' u' v' -> s1 {npc_states = (npc_states s1) // [(head d_list, char_state {ticks_left0 = t0, ticks_left1 = t1, target_w' = w', target_u' = u', target_v' = v'})]}
      rand_target = det_rand_target s0 (snd__ (snd (bounds w_grid))) (third_ (snd (bounds w_grid)))
      fg_pos = fg_position char_state
  in
  if npc_type char_state == 2 && ticks_left0 char_state == 0 then
    if attack_mode char_state == True then npc_decision 1 1 offset (truncate (pos_w s0)) (truncate (pos_u s0)) (truncate (pos_v s0)) d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1' 20 0 0 0 0) look_up
    else if ticks_left1 char_state == 0 || (x0 == target_w' char_state && x1 == target_u' char_state && x2 == target_v' char_state) then npc_decision 1 1 offset (fst__ rand_target) (snd__ rand_target) (third_ rand_target) d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1' 20 1000 (fst__ rand_target) (snd__ rand_target) (third_ rand_target)) look_up
    else npc_decision 1 1 offset (target_w' char_state) (target_u' char_state) (target_v' char_state) d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1' 20 ((ticks_left1 char_state) - 1) (target_w' char_state) (target_u' char_state) (target_v' char_state)) look_up
  else if npc_type char_state < 2 && x1 /= truncate (snd__ fg_pos + fst (dir_vector char_state)) || x2 /= truncate (third_ fg_pos + snd (dir_vector char_state)) then
    if attack_mode char_state == True then npc_decision 1 0 offset (truncate (pos_w s0)) (truncate (pos_u s0)) (truncate (pos_v s0)) d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1' 0 0 0 0 0) look_up
    else if ticks_left1 char_state == 0 || (x0 == target_w' char_state && x1 == target_u' char_state && x2 == target_v' char_state) then npc_decision 1 0 offset (fst__ rand_target) (snd__ rand_target) (third_ rand_target) d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1' 0 1000 (fst__ rand_target) (snd__ rand_target) (third_ rand_target)) look_up
    else npc_decision 1 0 offset (target_w' char_state) (target_u' char_state) (target_v' char_state) d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1' 0 ((ticks_left1 char_state) - 1) (target_w' char_state) (target_u' char_state) (target_v' char_state)) look_up
  else if npc_type char_state < 2 then (obj_grid_upd, s1' 1 ((ticks_left1 char_state) - 1) (target_w' char_state) (target_u' char_state) (target_v' char_state))
  else (obj_grid_upd, s1' ((ticks_left0 char_state) - 1) ((ticks_left1 char_state) - 1) (target_w' char_state) (target_u' char_state) (target_v' char_state))
npc_decision 1 flag offset target_w target_u target_v d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 look_up =
  let char_state = (npc_states s1) ! (d_list !! 3)
      down_ramp = local_down_ramp (f_grid ! (x0, div x1 2, div x2 2))
      up_ramp = local_up_ramp (f_grid ! (x0, div x1 2, div x2 2))
  in
  if x0 == target_w then npc_decision (2 + flag) flag offset target_w target_u target_v d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {final_appr = True})]}) look_up
  else if x0 > target_w then npc_decision (2 + flag) flag offset x0 ((fst down_ramp) * 2) ((snd down_ramp) * 2) d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {final_appr = False})]}) look_up
  else npc_decision (2 + flag) flag offset x0 ((fst up_ramp) * 2) ((snd up_ramp) * 2) d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 (s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {final_appr = False})]}) look_up
npc_decision 2 flag offset target_w target_u target_v d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 look_up =
  let char_state = (npc_states s1) ! (d_list !! 3)
      fg_pos = fg_position char_state
      a = vector_to_angle (((fromIntegral target_u) + 0.5) - snd__ fg_pos) (((fromIntegral target_v) + 0.5) - third_ fg_pos)
      qa = quantise_angle a
      prog = obj_grid ! (x0, x1, x2)
      line_sight0 = chk_line_sight 2 (fst qa) x0 x1 x2 (snd__ (fg_position char_state), third_ (fg_position char_state)) target_u target_v w_grid f_grid obj_grid look_up
      line_sight1 = chk_line_sight 3 (fst qa) x0 x1 x2 (snd__ (fg_position char_state), third_ (fg_position char_state)) target_u target_v w_grid f_grid obj_grid look_up
      another_dir_ = another_dir (delete (snd qa) [1..8]) 7 x0 x1 x2 (snd__ fg_pos, third_ fg_pos) w_grid f_grid obj_grid look_up s0
  in
  if final_appr char_state == True then
    if line_sight0 == 0 then
      if (prob_seq s0) ! (mod (game_t s0) 240) < fire_prob char_state && attack_mode char_state == True then
        (((x0, x1, x2), (fst prog, [(offset, 1), (offset + 1, fl_to_int (fst__ fg_pos)), (offset + 2, fl_to_int (snd__ fg_pos)), (offset + 3, fl_to_int (third_ fg_pos)), (offset + 4, a)])) : obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = snd qa, last_dir = snd qa})]})
      else (obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = snd qa, last_dir = snd qa})]})
    else if line_sight0 > avoid_dist char_state then (obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = snd qa, last_dir = snd qa})]})
    else (obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = another_dir_, last_dir = another_dir_})]})
  else
    if line_sight1 < 0 then (obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = line_sight1, last_dir = direction char_state})]})
    else if line_sight1 == 0 then (obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = snd qa, last_dir = snd qa})]})
    else if line_sight1 > avoid_dist char_state then (obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = snd qa, last_dir = snd qa})]})
    else (obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = another_dir_, last_dir = another_dir_})]})
npc_decision 3 flag offset target_w target_u target_v d_list (x0:x1:x2:xs) w_grid f_grid obj_grid obj_grid_upd s0 s1 look_up =
  let char_state = (npc_states s1) ! (d_list !! 3)
      choice = cpede_decision 0 0 (head d_list) target_u target_v x0 x1 x2 w_grid f_grid obj_grid s0 s1 look_up
      prog = obj_grid ! (x0, x1, x2)
      fg_pos = fg_position char_state
  in
  if snd choice == True then
    if (prob_seq s0) ! (mod (game_t s0) 240) < fire_prob char_state then
      (((x0, x1, x2), (fst prog, [(offset, 1), (offset + 1, fl_to_int (fst__ fg_pos)), (offset + 2, fl_to_int (snd__ fg_pos)), (offset + 3, fl_to_int (third_ fg_pos)), (offset + 4, npc_dir_table (fst choice))])) : obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = fst choice})]})
    else (obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = fst choice})]})
  else (obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = fst choice})]})

det_dir_vector :: Int -> Float -> UArray (Int, Int) Float -> (Float, Float)
det_dir_vector dir speed look_up =
  let dir' = npc_dir_table dir
  in
  if dir == 0 then (0, 0)
  else (speed * look_up ! (2, dir'), speed * look_up ! (1, dir'))

char_rotation :: Int -> Int -> Int
char_rotation dir base_id = base_id + ((dir - 1) * 2)

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

ramp_fill :: Int -> Int -> Int -> a -> Terrain -> [((Int, Int, Int), a)]
ramp_fill w u v x t =
  let fill_u = if div u 2 == div (u + 1) 2 then (u + 1, 1)
               else (u - 1, -1)
      fill_v = if div v 2 == div (v + 1) 2 then (v + 1, 1)
               else (v - 1, -1)
      end_point = if t == Positive_u || t == Negative_u then (w, fst fill_u + snd fill_u, v)
                  else (w, u, fst fill_v + snd fill_v)
  in [((w, u, v), x), ((w, fst fill_u, v), x), ((w, u, fst fill_v), x), ((w, fst fill_u, fst fill_v), x), (end_point, x)]

conv_ramp_fill :: Int -> Int -> Int -> Int -> Terrain -> [Int]
conv_ramp_fill w u v dw t =
  let r = last (ramp_fill (w + dw) u v 0 t)
  in [fst__ (fst r), snd__ (fst r), third_ (fst r)]

npc_move :: Int -> [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
npc_move offset d_list (w:u:v:w1:u1:v1:blocks) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up =
  let char_state = (npc_states s1) ! (d_list !! 3)
      u' = truncate ((snd__ (fg_position char_state)) + fst dir_vector')
      v' = truncate ((third_ (fg_position char_state)) + snd dir_vector')
      o_target = fromJust (obj (w_grid ! (-w - 1, u, v)))
      prog = obj_grid ! (w, u, v)
      prog' = \x y -> (fst prog, take offset (snd prog) ++ [x, y] ++ drop (offset + 2) (snd prog))
      ramp_fill' = \dw -> last (ramp_fill (w + dw) u v (-2, [(offset, 0)]) (surface (f_grid ! (w, div u 2, div v 2))))
      dir_vector' = det_dir_vector (last_dir char_state) (speed char_state) look_up
      ramp_climb_ = ramp_climb (direction char_state) (41 - ticks_left0 char_state) (fg_position char_state)
      conv_ramp_fill0 = conv_ramp_fill w u v 1 (surface (f_grid ! (w, div u 2, div v 2)))
      conv_ramp_fill1 = conv_ramp_fill w u v 0 (surface (f_grid ! (w, div u 2, div v 2)))
      w_grid' = ((-w - 1, u, v), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {u__ = snd__ (fg_position char_state), v__ = third_ (fg_position char_state)})})
      w_grid'' = [((-w - 1, u', v'), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {ident_ = char_rotation (direction char_state) (d_list !! 4)})}), ((-w - 1, u, v), def_w_grid)]
      w_grid''' = ((-w - 1, u, v), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {w__ = fst__ ramp_climb_, u__ = snd__ ramp_climb_, v__ = third_ ramp_climb_})})
      w_grid_4 = \dw -> ([((-w - 1, u, v), def_w_grid)] ++ drop 4 (ramp_fill (-w - 1 + dw) u v ((w_grid ! (-w - 1, u, v)) {obj = Just (o_target {w__ = fst__ ramp_climb_, u__ = snd__ ramp_climb_, v__ = third_ ramp_climb_})}) (surface (f_grid ! (w, div u 2, div v 2)))))
      damage = det_damage (difficulty s1) s0 
  in
  if npc_type char_state < 2 && ticks_left0 char_state == 0 then
    if (w, u', v') == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then
      if attack_mode char_state == True && binary_dice_ 1 s0 == True then
        if health s1 - damage <= 0 then (w_grid_upd, obj_grid_upd, s1 {health = 0, state_chg = 1, message = 0 : msg28})
        else (w_grid_upd, obj_grid_upd, s1 {health = health s1 - damage, message = message s1 ++ msg29, next_sig_q = next_sig_q s1 ++ [3, w, u, v]})
      else (w_grid_upd, obj_grid_upd, s1 {next_sig_q = next_sig_q s1 ++ [3, w, u, v]})
    else if direction char_state >= 0 then
      if isNothing (obj (w_grid ! (-w - 1, u', v'))) == True || (u, v) == (u', v') then
        (w_grid'' ++ w_grid_upd, ((w, u, v), (-2, [])) : ((w, u', v'), (-2, [])) : obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {dir_vector = dir_vector', fg_position = add_vel_pos (fg_position char_state) dir_vector', node_locations = [w, u', v', 0, 0, 0], ticks_left0 = 1})], next_sig_q = next_sig_q s1 ++ [3, w, u', v']})
      else (w_grid_upd, obj_grid_upd, s1 {next_sig_q = next_sig_q s1 ++ [3, w, u, v]})
    else if direction char_state < -4 then ([((-w - 1, u, v), def_w_grid), ((-w - 1, u', v'), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {ident_ = char_rotation (npc_dir_remap (direction char_state)) (d_list !! 4)})})] ++ w_grid_upd, (take 4 (ramp_fill w u' v' (2, []) (surface (f_grid ! (w, div u' 2, div v' 2))))) ++ [((w, u, v), (-2, [])), ((w, u', v'), (-2, [(offset, 1)]))] ++ obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {node_locations = [w, u', v', 0, 0, 0], ticks_left0 = 41})], next_sig_q = next_sig_q s1 ++ [3, w, u', v']})
    else
      if (w - 1, u', v') == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then (w_grid_upd, obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {direction = (last_dir char_state)})], next_sig_q = next_sig_q s1 ++ [3, w, u, v]})
      else ([((-w - 1, u, v), def_w_grid), ((-w, u', v'), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {ident_ = char_rotation (npc_dir_remap (direction char_state)) (d_list !! 4)})})] ++ w_grid_upd, (take 4 (ramp_fill (w - 1) u' v' (2, []) (surface (f_grid ! (w - 1, div u' 2, div v' 2))))) ++ [((w, u, v), (-2, [])), ((w - 1, u', v'), (-2, [(offset, 1)]))] ++ obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {node_locations = [w - 1, u', v', 0, 0, 0], ticks_left0 = 41})], next_sig_q = next_sig_q s1 ++ [3, w - 1, u', v']})
  else if npc_type char_state < 2 && ticks_left0 char_state == 1 then
    if direction char_state >= 0 then (w_grid' : w_grid_upd, obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {fg_position = add_vel_pos (fg_position char_state) (dir_vector char_state)})], next_sig_q = next_sig_q s1 ++ [3, w, u, v]})
    else if direction char_state < -4 then
      if (w + 1, conv_ramp_fill0 !! 1, conv_ramp_fill0 !! 2) == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then (w_grid_upd, obj_grid_upd, s1 {next_sig_q = next_sig_q s1 ++ [3, w, u', v']})
      else if fst (obj_grid ! (w + 1, conv_ramp_fill0 !! 1, conv_ramp_fill0 !! 2)) > 0 then (w_grid_upd, obj_grid_upd, s1 {next_sig_q = next_sig_q s1 ++ [3, w, u', v']})
      else (w_grid_4 (-1) ++ w_grid_upd, take 4 (ramp_fill w u v (0, []) (surface (f_grid ! (w, div u 2, div v 2)))) ++ [((w, u, v), (-2, [])), ramp_fill' 1] ++ obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {node_locations = conv_ramp_fill0 ++ [0, 0, 0], fg_position = ramp_climb_, direction = npc_dir_remap (direction char_state), ticks_left0 = 1})], next_sig_q = next_sig_q s1 ++ [3] ++ conv_ramp_fill0})
    else
      if (w, conv_ramp_fill1 !! 1, conv_ramp_fill1 !! 2) == (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) then (w_grid_upd, obj_grid_upd, s1 {next_sig_q = next_sig_q s1 ++ [3, w, u', v']})
      else if fst (obj_grid ! (w, conv_ramp_fill1 !! 1, conv_ramp_fill1 !! 2)) > 0 then (w_grid_upd, obj_grid_upd, s1 {next_sig_q = next_sig_q s1 ++ [3, w, u', v']})
      else (w_grid_4 0 ++ w_grid_upd, take 4 (ramp_fill w u v (0, []) (surface (f_grid ! (w, div u 2, div v 2)))) ++ [((w, u, v), (-2, [])), ramp_fill' 0] ++ obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {node_locations = conv_ramp_fill1 ++ [0, 0, 0], fg_position = ramp_climb_, direction = npc_dir_remap (direction char_state), ticks_left0 = 1})], next_sig_q = next_sig_q s1 ++ [3] ++ conv_ramp_fill1})
  else if npc_type char_state < 2 && ticks_left0 char_state > 1 then (w_grid''' : w_grid_upd, obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {ticks_left0 = ticks_left0 char_state - 1})], next_sig_q = next_sig_q s1 ++ [3, w, u', v']})
  else throw NPC_feature_not_implemented

npc_damage :: [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> [Int] -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state1)
npc_damage (w:u:v:blocks) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 d_list =
  let damage = det_damage ("d", 6, 10, 14) s0
      char_state = (npc_states s1) ! (d_list !! 3)
      o_target = fromJust (obj (w_grid ! (-w - 1, u, v)))
  in
  if c_health char_state - damage <= 0 then (((-w - 1, u, v), (w_grid ! (-w - 1, u, v)) {obj = Just (o_target {ident_ = (d_list !! 1) + 9, texture__ = 1})}) : w_grid_upd, ((w, u, v), (-1, [])) : obj_grid_upd, s1 {message = message s1 ++ [2, 4, 14]})
  else (w_grid_upd, obj_grid_upd, s1 {npc_states = (npc_states s1) // [(d_list !! 3, char_state {c_health = (c_health char_state) - damage})], message = message s1 ++ [2, 4, 16]})

inspect_obj_grid :: Int -> (Int, Int, Int) -> Array (Int, Int, Int) (Int, [Int]) -> [Int] -> IO ()
inspect_obj_grid n (i0, i1, i2) obj_grid d_list =
  let target = (d_list !! i0, d_list !! i1, d_list !! i2)
  in do
  putStr ("\nObj_grid inspection point " ++ show n ++ ", index " ++ show target ++ ": " ++ show (obj_grid ! target))

-- Branch on each GPLC op - code to call the corresponding function, with optional per op - code status reports for debugging.
run_gplc :: [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> Int -> SEQ.Seq Char -> IO ([((Int, Int, Int), Wall_grid)], Array (Int, Int, Int) Floor_grid, [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state0, Play_state1, SEQ.Seq Char)
run_gplc [] d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up c log = return (w_grid_upd, f_grid, obj_grid_upd, s0, s1, log)
run_gplc code d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 0 log =
  let location = ((splitOn [536870911] code) !! 2)
      m0 = SEQ.fromList ("\n\nGPLC program run at Obj_grid " ++ show (location !! 0, location !! 1, location !! 2))
      m1 = SEQ.fromList ("\non_signal run.  Initial state is...")
      m2 = SEQ.fromList ("\nProgram list: " ++ show (snd (obj_grid ! (location !! 0, location !! 1, location !! 2))) ++ "\nData list: " ++ show ((splitOn [536870911] code) !! 2))
  in do
  run_gplc (on_signal (drop 2 ((splitOn [536870911] code) !! 0)) ((splitOn [536870911] code) !! 1) (code !! 1)) ((splitOn [536870911] code) !! 2) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 1 (log SEQ.>< m0 SEQ.>< m1 SEQ.>< m2)
run_gplc code d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 1 log =
  let if0' = if0 code d_list
      m = SEQ.fromList ("\nIf expression folding run.  Branch selected: " ++ show if0')
  in do
  run_gplc (tail_ if0') d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up (head_ if0') (log SEQ.>< m)
run_gplc xs d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 2 log =
  let chg_state_ = chg_state (2 : xs) (0, 0, 0) (0, 0, 0) w_grid (array (0, 13) [(0, 3), (1, 0), (2, 3), (3, 0), (4, 3), (5, 0), (6, 3), (7, 0), (8, 3), (9, 0), (10, 3), (11, 0), (12, 3), (13, 0)]) w_grid_upd d_list log
  in do
  run_gplc (tail_ (snd__ chg_state_)) d_list w_grid (fst__ chg_state_) f_grid obj_grid obj_grid_upd s0 s1 look_up (head_ (snd__ chg_state_)) (third_ chg_state_)
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 3 log =
  let m = SEQ.fromList ("\nchg_grid run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5) ++ " 6: " ++ show (d_list !! x6))
  in do
  run_gplc (tail_ xs) d_list w_grid (chg_grid x0 (x1, x2, x3) (x4, x5, x6) w_grid def_w_grid w_grid_upd d_list) f_grid obj_grid obj_grid_upd s0 s1 look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 4 log =
  let sig = send_signal 0 x0 (x1, x2, x3) obj_grid s1 d_list
      m = SEQ.fromList ("\nsend_signal run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid (fst sig) obj_grid_upd s0 (snd sig) look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 5 log =
  let m = SEQ.fromList ("\nchg_value run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5))
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (chg_value x0 x1 x2 (x3, x4, x5) d_list obj_grid obj_grid_upd) s0 s1 look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 6 log =
  let m = SEQ.fromList ("\nchg_floor run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5))
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd (chg_floor x0 x1 x2 (x3, x4, x5) f_grid d_list) obj_grid obj_grid_upd s0 s1 look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:x2:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 7 log =
  let m = SEQ.fromList ("\nchg_ps1 run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2))
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (chg_ps1 x0 x1 x2 d_list s1) look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 8 log =
  let m = SEQ.fromList ("\nchg_obj_type run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (chg_obj_type x0 (x1, x2, x3) d_list obj_grid obj_grid_upd) s0 s1 look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 9 log = do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (place_hold x0 d_list s1) look_up (head_ xs) log
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 10 log =
  let m = SEQ.fromList ("\nchg_grid_ run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5) ++ " 6: " ++ show (d_list !! x6))
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (chg_grid_ x0 (x1, x2, x3) (x4, x5, x6) obj_grid_upd d_list) s0 s1 look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 11 log =
  let m = SEQ.fromList ("\ncopy_ps1 run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (copy_ps1 x0 (x1, x2, x3) s1 obj_grid obj_grid_upd d_list) s0 s1 look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 12 log =
  let m = SEQ.fromList ("\ncopy_lstate run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5) ++ " 6: " ++ show (d_list !! x6))
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (copy_lstate x0 (x1, x2, x3) (x4, x5, x6) w_grid obj_grid obj_grid_upd d_list) s0 s1 look_up (head_ xs) (log SEQ.>< m)
run_gplc (x:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 13 log =
  let pass_msg' = pass_msg x xs s1 d_list
      m = SEQ.fromList ("\npass_msg run with arguments " ++ "msg_length: " ++ show (d_list !! x) ++ " message data: " ++ show (take (d_list !! x) xs))
  in do
  run_gplc (tail_ (fst pass_msg')) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (snd pass_msg') look_up (head_ (fst pass_msg')) (log SEQ.>< m)
run_gplc (x0:x1:x2:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 14 log =
  let m = SEQ.fromList ("\nchg_ps0 run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2))
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd (chg_ps0 x0 x1 x2 d_list s0) s1 look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 15 log =
  let m = SEQ.fromList ("\ncopy_ps0 run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (copy_ps0 x0 (x1, x2, x3) s0 obj_grid obj_grid_upd d_list) s0 s1 look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 16 log =
  let m = SEQ.fromList ("\nbinary_dice run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show x5)
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (binary_dice x0 x1 (x2, x3, x4) x5 s0 obj_grid obj_grid_upd d_list) s0 s1 look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 17 log =
  let m = SEQ.fromList ("\nproject_init run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ "4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5) ++ " 6: " ++ show (d_list !! x6) ++ " 7: " ++ show (d_list !! x7) ++ " 8: " ++ show x8)
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (project_init x0 x1 x2 x3 x4 (x5, x6, x7) (x8, x9, x10) x11 x12 obj_grid obj_grid_upd d_list look_up) s0 s1 look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:x2:x3:x4:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 18 log =
  let project_update' = project_update x0 x1 (x2, x3, x4) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 d_list
      m = SEQ.fromList ("\nproject_update run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show x1 ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4))
  in do
  run_gplc (tail_ xs) d_list w_grid (fst__ project_update') f_grid obj_grid (snd__ project_update') s0 (third_ project_update') look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:x1:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 19 log =
  let m = SEQ.fromList ("\ninit_npc run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show x1)
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (init_npc x0 x1 s1 d_list) look_up (head_ xs) (log SEQ.>< m)
run_gplc (x0:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 20 log =
  let npc_decision_ = npc_decision 0 0 x0 0 0 0 d_list (node_locations ((npc_states s1) ! (head d_list))) w_grid f_grid obj_grid obj_grid_upd s0 s1 look_up
      m0 = SEQ.fromList ("\nnpc_decision run with arguments " ++ "0: " ++ show x0)
      m1 = SEQ.fromList ("\n" ++ show ((npc_states s1) ! (head d_list)))
  in do
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid (fst npc_decision_) s0 (snd npc_decision_) look_up (head_ xs) (log SEQ.>< m0 SEQ.>< m1)
run_gplc (x0:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 21 log =
  let npc_move_ = npc_move x0 d_list (node_locations ((npc_states s1) ! (head d_list))) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up
      m0 = SEQ.fromList ("\nnpc_move run with arguments " ++ "0: " ++ show x0)
      m1 = SEQ.fromList ("\n" ++ show ((npc_states s1) ! (head d_list)))
  in do
  run_gplc (tail_ xs) d_list w_grid (fst__ npc_move_) f_grid obj_grid (snd__ npc_move_) s0 (third_ npc_move_) look_up (head_ xs) (log SEQ.>< m0 SEQ.>< m1)
run_gplc code d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 22 log =
  let npc_damage_ = npc_damage (node_locations ((npc_states s1) ! (head d_list))) w_grid w_grid_upd obj_grid obj_grid_upd s0 s1 d_list
      m0 = SEQ.fromList ("\nnpc_damage run...")
      m1 = SEQ.fromList ("\n" ++ show ((npc_states s1) ! (head d_list)))
  in do
  run_gplc (tail_ code) d_list w_grid (fst__ npc_damage_) f_grid obj_grid (snd__ npc_damage_) s0 (third_ npc_damage_) look_up (head_ code) (log SEQ.>< m0 SEQ.>< m1)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up 23 log = do
  inspect_obj_grid x0 (x1, x2, x3) obj_grid d_list
  run_gplc (tail_ xs) d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up (head_ xs) log
run_gplc code d_list w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up c log = do
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

gplc_error :: [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Floor_grid -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> SEQ.Seq Char -> SomeException -> IO ([((Int, Int, Int), Wall_grid)], Array (Int, Int, Int) Floor_grid, [((Int, Int, Int), (Int, [(Int, Int)]))], Play_state0, Play_state1, SEQ.Seq Char)
gplc_error w_grid_upd f_grid obj_grid_upd s0 s1 log e = return (w_grid_upd, f_grid, obj_grid_upd, s0 {game_t = -1}, s1, log SEQ.>< (SEQ.fromList ("\nException thrown: " ++ show e)))

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
      new_prog0 = elems ((listArray (0, (length prog) - 1) prog :: UArray Int Int) // (snd (snd (xs !! 0))))
      new_prog1 = elems ((listArray (0, (length prog) - 1) prog :: UArray Int Int) // (acc ++ snd (snd x)))
  in
  if m == 0 then
    if fst (snd x) >= 0 then atomise_obj_grid_upd 1 (x:xs) acc obj_grid
    else if fst (snd x) == -1 then (fst x, def_obj_grid) : atomise_obj_grid_upd 0 xs acc obj_grid
    else if fst (snd x) == -2 then
      if fst x == fst (xs !! 0) then (fst (xs !! 0), (fst source, new_prog0)) : atomise_obj_grid_upd 0 (drop 1 xs) acc obj_grid
      else (fst (xs !! 0), (fst source, new_prog0)) : (fst x, def_obj_grid) : atomise_obj_grid_upd 0 (drop 1 xs) acc obj_grid
    else (fst (xs !! 0), (fst source, new_prog0)) : atomise_obj_grid_upd 0 (drop 1 xs) acc obj_grid
  else
    if xs == [] then [(fst x, (fst source, new_prog1))]
    else if fst x == fst (xs !! 0) then atomise_obj_grid_upd 1 xs (acc ++ snd (snd x)) obj_grid
    else (fst x, (fst source, new_prog1)) : atomise_obj_grid_upd 0 xs [] obj_grid

-- These two functions (together with send_signal) implement the signalling system that drives GPLC program runs.  This involves signalling programs in response to player object collisions and handling
-- the signal queue, which allows programs to signal each other.
link_gplc0 :: Bool -> [Float] -> [Int] -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)] -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> [((Int, Int, Int), (Int, [(Int, Int)]))] -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> Bool -> SEQ.Seq Char -> IO (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) (Int, [Int]), Play_state0, Play_state1, SEQ.Seq Char)
link_gplc0 False (x0:x1:xs) (z0:z1:z2:zs) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up swap_flag log = return (w_grid, f_grid, obj_grid, s0, s1, log)
link_gplc0 True (x0:x1:xs) (z0:z1:z2:zs) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 s1 look_up swap_flag log =
  let dest = ((sig_q s1) !! 1, (sig_q s1) !! 2, (sig_q s1) !! 3)
      prog = (snd (obj_grid ! dest))
      obj_grid0' = (send_signal 1 1 (z0, z1, z2 + 1) obj_grid s1 [])
      obj_grid1' = (send_signal 1 1 (z0, z1 + 1, z2) obj_grid s1 [])
      obj_grid2' = (send_signal 1 1 (z0, z1, z2 - 1) obj_grid s1 [])
      obj_grid3' = (send_signal 1 1 (z0, z1 - 1, z2) obj_grid s1 [])
      obj_grid4' = obj_grid // [(dest, (fst (obj_grid ! dest), (head__ prog) : ((sig_q s1) !! 0) : drop 2 prog))]
      m0 = SEQ.fromList ("\n\nSignal addressed to Obj_grid " ++ show ((sig_q s1) !! 1, (sig_q s1) !! 2, (sig_q s1) !! 3) ++ " but this element is not set to run programs from.")
      m1 = \x -> SEQ.fromList ("\n\nPlayer starts GPLC program at Obj_grid " ++ show x)
      m2 = \x -> SEQ.fromList ("\nw_grid_upd: " ++ show x)
      m3 = \x -> SEQ.fromList ("\nobj_grid_upd: " ++ show x)
  in do
  if sig_q s1 == [] && swap_flag == False then link_gplc0 True (x0:x1:xs) (z0:z1:z2:zs) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (s1 {sig_q = next_sig_q s1, next_sig_q = []}) look_up True log
  else if swap_flag == True then
    if (x1 == 1 || x1 == 3) && x0 == 0 && head (snd (obj_grid ! (z0, z1, z2 + 1))) == 0 then do
      run_gplc' <- catch (run_gplc (snd ((fst obj_grid0') ! (z0, z1, z2 + 1))) [] w_grid [] f_grid (fst obj_grid0') [] s0 s1 look_up 0 (log SEQ.>< m1 (z0, z1, z2 + 1))) (\e -> gplc_error w_grid_upd f_grid obj_grid_upd s0 s1 log e)
      upd <- force_update0 ((fst_ run_gplc') ++ w_grid_upd) [] 0
      return (w_grid // upd, snd_ run_gplc', obj_grid // (atomise_obj_grid_upd 0 ((third run_gplc') ++ obj_grid_upd) [] obj_grid), fourth run_gplc', fifth run_gplc', sixth run_gplc' SEQ.>< m2 (fst_ run_gplc') SEQ.>< m3 (third run_gplc'))
    else if (x1 == 1 || x1 == 3) && x0 == 1 && head (snd (obj_grid ! (z0, z1 + 1, z2))) == 0 then do
      run_gplc' <- catch (run_gplc (snd ((fst obj_grid1') ! (z0, z1 + 1, z2))) [] w_grid [] f_grid (fst obj_grid1') [] s0 s1 look_up 0 (log SEQ.>< m1 (z0, z1 + 1, z2))) (\e -> gplc_error w_grid_upd f_grid obj_grid_upd s0 s1 log e)
      upd <- force_update0 ((fst_ run_gplc') ++ w_grid_upd) [] 0
      return (w_grid // upd, snd_ run_gplc', obj_grid // (atomise_obj_grid_upd 0 ((third run_gplc') ++ obj_grid_upd) [] obj_grid), fourth run_gplc', fifth run_gplc', sixth run_gplc' SEQ.>< m2 (fst_ run_gplc') SEQ.>< m3 (third run_gplc'))
    else if (x1 == 1 || x1 == 3) && x0 == 2 && head (snd (obj_grid ! (z0, z1, z2 - 1))) == 0 then do
      run_gplc' <- catch (run_gplc (snd ((fst obj_grid2') ! (z0, z1, z2 - 1))) [] w_grid [] f_grid (fst obj_grid2') [] s0 s1 look_up 0 (log SEQ.>< m1 (z0, z1, z2 - 1))) (\e -> gplc_error w_grid_upd f_grid obj_grid_upd s0 s1 log e)
      upd <- force_update0 ((fst_ run_gplc') ++ w_grid_upd) [] 0
      return (w_grid // upd, snd_ run_gplc', obj_grid // (atomise_obj_grid_upd 0 ((third run_gplc') ++ obj_grid_upd) [] obj_grid), fourth run_gplc', fifth run_gplc', sixth run_gplc' SEQ.>< m2 (fst_ run_gplc') SEQ.>< m3 (third run_gplc'))
    else if (x1 == 1 || x1 == 3) && x0 == 3 && head (snd (obj_grid ! (z0, z1 - 1, z2))) == 0 then do
      run_gplc' <- catch (run_gplc (snd ((fst obj_grid3') ! (z0, z1 - 1, z2))) [] w_grid [] f_grid (fst obj_grid3') [] s0 s1 look_up 0 (log SEQ.>< m1 (z0, z1 - 1, z2))) (\e -> gplc_error w_grid_upd f_grid obj_grid_upd s0 s1 log e)
      upd <- force_update0 ((fst_ run_gplc') ++ w_grid_upd) [] 0
      return (w_grid // upd, snd_ run_gplc', obj_grid // (atomise_obj_grid_upd 0 ((third run_gplc') ++ obj_grid_upd) [] obj_grid), fourth run_gplc', fifth run_gplc', sixth run_gplc' SEQ.>< m2 (fst_ run_gplc') SEQ.>< m3 (third run_gplc'))
    else do
      upd <- force_update0 w_grid_upd [] 0
      return (w_grid // upd, f_grid, obj_grid // (atomise_obj_grid_upd 0 obj_grid_upd [] obj_grid), s0, s1, log)
  else do
    if fst (obj_grid ! dest) == 1 || fst (obj_grid ! dest) == 3 then do
      run_gplc' <- catch (run_gplc (snd (obj_grid4' ! dest)) [] w_grid [] f_grid obj_grid4' [] s0 (s1 {sig_q = drop 4 (sig_q s1)}) look_up 0 log) (\e -> gplc_error w_grid_upd f_grid obj_grid_upd s0 s1 log e)
      if game_t (fourth run_gplc') == -1 then return (w_grid, f_grid, obj_grid, fourth run_gplc', s1, sixth run_gplc')
      else link_gplc0 True (x0:x1:xs) (z0:z1:z2:zs) w_grid ((fst_ run_gplc') ++ w_grid_upd) (snd_ run_gplc') obj_grid ((third run_gplc') ++ obj_grid_upd) (fourth run_gplc') (fifth run_gplc') look_up False ((sixth run_gplc') SEQ.>< m2 (fst_ run_gplc') SEQ.>< m3 (third run_gplc'))
    else link_gplc0 True (x0:x1:xs) (z0:z1:z2:zs) w_grid w_grid_upd f_grid obj_grid obj_grid_upd s0 (s1 {sig_q = drop 4 (sig_q s1)}) look_up False (log SEQ.>< m0)

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

-- Used to generate the sequence of message tile references that represent the pause screen text.
pause_text :: Play_state1 -> ([Char], Int, Int, Int) -> [(Int, [Int])]
pause_text s1 (diff, a, b, c) =
  [(0, msg9), (0, []), (0, msg1 ++ conv_msg (health s1)), (0, msg2 ++ conv_msg (ammo s1)), (0, msg3 ++ conv_msg (gems s1)), (0, msg4 ++ conv_msg (torches s1)), (0, msg5 ++ keys s1), (0, msg6 ++ region s1), (0, conv_msg_ ("Difficulty: " ++ diff)), (0, []), (1, msg10), (2, msg17), (3, msg11), (4, msg12)]

-- This function causes the GPLC interpreter to skip an appropriate number of ticks (relative to frames rendered) when the frame rate limit is set to 50 or 60.  This means scripted game events run at
-- the correct speed whether the frame rate limit is 40, 50 or 60.
sync_game_t :: Array Int [Char] -> Play_state0 -> Bool
sync_game_t conf_reg s0 =
  let fps_limit = read (cfg conf_reg 0 "fps_limit")
  in
  if fps_limit == 50 && mod (game_t s0) 5 == 4 then False
  else if fps_limit == 60 && mod (game_t s0) 3 == 2 then False
  else True

-- Specific to the Heavy-dubbuging-output-variant branch.  This function limits the length of the sequence used to generate the log file.
limit_log_length :: SEQ.Seq (SEQ.Seq Char) -> Array Int [Char] -> IO (SEQ.Seq (SEQ.Seq Char))
limit_log_length log conf_reg =
  let cfg' = cfg conf_reg 0
  in do
  putStr (toList (SEQ.index log 0))
  if SEQ.length log > read (cfg' "log_file_size") then return (SEQ.drop 1 log)
  else return log

-- Specific to the Heavy-dubbuging-output-variant branch.  This function saves the debugging log to a file.
save_log_file :: SEQ.Seq (SEQ.Seq Char) -> Handle -> IO ()
save_log_file log h =
  if null log == True then return ()
  else do
    hPutStr h (toList (SEQ.index log 0))
    save_log_file (SEQ.drop 1 log) h

-- This function recurses once per game logic clock tick and is the central branching point of the game logic thread.
update_play :: Io_box -> MVar (Play_state0, Array (Int, Int, Int) Wall_grid, Save_state) -> Play_state0 -> Play_state1 -> Bool -> Float -> (Float, Float, Float, Float) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> UArray (Int, Int) Float -> Array Int [Char] -> Save_state -> Array Int Source -> SEQ.Seq (SEQ.Seq Char) -> IO ()
update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array log =
  let det = detect_coll (truncate (pos_w s0)) (pos_u s0, pos_v s0) ((vel s0) !! 0 / f_rate, (vel s0) !! 1 / f_rate) obj_grid w_grid
      floor = floor_surf (det !! 0) (det !! 1) (pos_w s0) f_grid
      vel_0 = update_vel (vel s0) [0, 0, 0] ((drop 2 det) ++ [0]) f_rate f
      vel_2 = update_vel (vel s0) [0, 0, g] ((drop 2 det) ++ [0]) f_rate 0
      game_t' = game_t s0 + 1
      m = SEQ.fromList ("\n\ngame_t = " ++ show (game_t s0) ++ "\n--------------\n")
      cfg' = cfg conf_reg 0
  in do
  control <- messagePump (hwnd_ io_box)
  link0 <- link_gplc0 (sync_game_t conf_reg s0) (drop 4 det) [truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)] w_grid [] f_grid obj_grid [] s0 s1 look_up False m
  link1 <- link_gplc1 s0 s1 obj_grid 0
  link1_ <- link_gplc1 s0 s1 obj_grid 1
  log' <- limit_log_length (log SEQ.>< SEQ.singleton (sixth link0)) conf_reg
  if game_t (fourth link0) == -1 || control == 14 then do
    h <- openFile (cfg' "log_file_name") WriteMode
    save_log_file log h
    hClose h
    putStr "\n\nLog file generated."
    exitSuccess
  else if control == 2 then do
    choice <- run_menu (pause_text s1 (difficulty s1)) [] io_box (-0.75) (-0.75) 1 0 0
    if choice == 1 then update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array log
    else if choice == 2 then do
      update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg (Save_state {is_set = True, w_grid_ = w_grid, f_grid_ = f_grid, obj_grid_ = obj_grid, s0_ = s0, s1_ = s1}) sound_array log
    else if choice == 3 then do
      putMVar state_ref (s0 {msg_count = -1}, w_grid, save_state)
      update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array log
    else do
      putMVar state_ref (s0 {msg_count = -3}, w_grid, save_state)
      update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array log
  else if control == 10 then update_play io_box state_ref (fourth link0) ((fifth link0) {sig_q = sig_q s1 ++ [2, 0, 0, 0]}) in_flight f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
  else if control == 11 then do
    if view_mode s0 == 0 then update_play io_box state_ref ((fourth link0) {view_mode = 1}) (fifth link0) in_flight f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
    else update_play io_box state_ref ((fourth link0) {view_mode = 0}) (fifth link0) in_flight f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
  else if control == 12 then update_play io_box state_ref ((fourth link0) {view_angle = mod_angle (view_angle s0) 5}) (fifth link0) in_flight f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
  else if control == 13 then update_play io_box state_ref (fourth link0) ((fifth link0) {sig_q = sig_q s1 ++ [2, 0, 0, 1]}) in_flight f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
  else if message s1 /= [] then do
    event <- proc_msg0 (message s1) s0 s1 io_box sound_array
    putMVar state_ref (fst event, w_grid, save_state)
    update_play io_box state_ref ((fst event) {msg_count = 0}) (snd event) in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array log'
  else
    if in_flight == False then
      if (pos_w s0) - floor > 0.02 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, vel = vel_0, game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
      else if control > 2 && control < 7 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = update_vel (vel s0) (take 3 (thrust (fromIntegral control) (angle s0) mag_r f_rate look_up)) ((drop 2 det) ++ [0]) f_rate f, game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
      else if control == 7 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, angle = mod_angle (angle s0) (angle_step s1)}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, angle = mod_angle (angle s0) (angle_step s1), game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
      else if control == 8 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, angle = mod_angle (angle s0) (- (angle_step s1))}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, angle = mod_angle (angle s0) (- (angle_step s1)), game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
      else if control == 9 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor + mag_j / f_rate}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor + mag_j / f_rate, vel = (take 2 vel_0) ++ [mag_j], game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
      else if control == 13 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, game_t = game_t'}) ((fifth link0) {sig_q = sig_q s1 ++ [0, 0, 1]}) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (fst (send_signal 1 1 (0, 0, 1) (third link0) s1 [])) look_up conf_reg save_state sound_array log'
      else do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
    else if in_flight == True && (pos_w s0) > floor then
      if control == 7 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, angle = mod_angle (angle s0) (angle_step s1)}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, vel = vel_2, angle = mod_angle (angle s0) (angle_step s1), game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
      else if control == 8 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, angle = mod_angle (angle s0) (- (angle_step s1))}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, vel = vel_2, angle = mod_angle (angle s0) (- (angle_step s1)), game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
      else do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, vel = vel_2, game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array log'
    else do
      putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor}, w_grid, save_state)
      if (vel s0) !! 2 < -4 then do
        update_play io_box state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, game_t = game_t'}) link1_ False f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array log'
      else do
        update_play io_box state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, game_t = game_t'}) link1 False f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array log'

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
  if x0 == 0 && state_chg s1 == 1 && health s1 <= 0 then do
    play_ (sound_array ! 20)
    return (s0 {message_ = [(0, take (x1 - 3) xs)], msg_count = -2}, s1)
  else if x0 == 0 && state_chg s1 == 1 then proc_msg0 (drop (x1 - 3) xs) (s0 {message_ = [(0, (take (x1 - 3) xs) ++ msg1 ++ conv_msg (health s1))], msg_count = 400}) s1 io_box sound_array
  else if x0 == 0 && state_chg s1 == 2 then proc_msg0 (drop (x1 - 3) xs) (s0 {message_ = [(0, (take (x1 - 3) xs) ++ msg2 ++ conv_msg (ammo s1))], msg_count = 400}) s1 io_box sound_array
  else if x0 == 0 && state_chg s1 == 3 then proc_msg0 (drop (x1 - 3) xs) (s0 {message_ = [(0, (take (x1 - 3) xs) ++ msg3 ++ conv_msg (gems s1))], msg_count = 400}) s1 io_box sound_array
  else if x0 == 0 && state_chg s1 == 4 then proc_msg0 (drop (x1 - 3) xs) (s0 {message_ = [(0, (take (x1 - 3) xs) ++ msg4 ++ conv_msg (torches s1))], msg_count = 400}) s1 io_box sound_array
  else if x0 < 0 then return (s0 {message_ = [(0, take x1 xs)], msg_count = x0}, s1)
  else if x0 == 1 then do
    choice <- run_menu (proc_msg1 (tail (splitOn [-1] (take (x1 - 3) xs)))) [] io_box (-0.96) (-0.2) 1 0 0
    proc_msg0 (drop (x1 - 3) xs) s0 (s1 {sig_q = sig_q s1 ++ [choice + 1, signal_ !! 0, signal_ !! 1, signal_ !! 2]}) io_box sound_array
  else if x0 == 2 then do
    if (xs !! 0) == 0 then return ()
    else play_ (sound_array ! ((xs !! 0) - 1))
    proc_msg0 (drop (x1 - 3) xs) s0 s1 io_box sound_array
  else if x0 == 3 then proc_msg0 [] (s0 {pos_u = int_to_float (xs !! 0), pos_v = int_to_float (xs !! 1), pos_w = int_to_float (xs !! 2)}) s1 io_box sound_array
  else proc_msg0 (drop (x1 - 3) xs) (s0 {message_ = [(0, take (x1 - 3) xs)], msg_count = 400}) s1 io_box sound_array

-- Used by the game logic thread for in game menus and by the main thread for the main menu.
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
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    run_menu acc [] io_box x 0.1 (c - 1) c_max 2
  else if control == 5 && c < c_max then do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    run_menu acc [] io_box x 0.1 (c + 1) c_max 2
  else if control == 2 then return c
  else do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    run_menu acc [] io_box x 0.1 c c_max 2
run_menu (n:ns) acc io_box x y c c_max d = do
  if d == 2 then do
    glBindVertexArray (unsafeCoerce ((fst (p_bind_ io_box)) ! 1017))
    glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst (p_bind_ io_box)) ! 1018))
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
  if fst n == c then show_text (snd n) 1 933 (uniform_ io_box) (p_bind_ io_box) p_tt_matrix x y 0
  else show_text (snd n) 0 933 (uniform_ io_box) (p_bind_ io_box) p_tt_matrix x y 0
  free p_tt_matrix
  run_menu ns (acc ++ [n]) io_box x (y - 0.04) c c_max 1

-- This function handles the drawing of message tiles (letters and numbers etc) that are used for in game messages and in menus.
show_text :: [Int] -> Int -> Int -> UArray Int Int32 -> (UArray Int Word32, Int) -> Ptr GLfloat -> Float -> Float -> Int -> IO ()
show_text [] mode base uniform p_bind p_tt_matrix x y offset = return ()
show_text (m:ms) mode base uniform p_bind p_tt_matrix x y offset = do
  load_array (MAT.toList (translation x y 0)) (castPtr p_tt_matrix) offset
  if mode == 0 && x < 83 then do
    glUniformMatrix4fv (unsafeCoerce (uniform ! 36)) 1 1 (plusPtr p_tt_matrix (offset * glfloat))
    glUniform1i (unsafeCoerce (uniform ! 38)) 0
  else if mode == 1 && x < 83 then do
    glUniformMatrix4fv (unsafeCoerce (uniform ! 36)) 1 1 (plusPtr p_tt_matrix (offset * glfloat))
    glUniform1i (unsafeCoerce (uniform ! 38)) 1
  else do
    putStr "show_text: Invalid mode or character reference in text line..."
    show_text ms mode base uniform p_bind p_tt_matrix (x + 0.05) y (offset + 16)
  glBindTexture GL_TEXTURE_2D (unsafeCoerce ((fst p_bind) ! (base + m)))
  glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT zero_ptr
  show_text ms mode base uniform p_bind p_tt_matrix (x + 0.04) y (offset + 16)
