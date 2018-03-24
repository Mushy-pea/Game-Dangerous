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
import Data.List.Split
import Data.Fixed
import Control.Concurrent
import Control.Exception
import qualified Data.Matrix as MAT
import Unsafe.Coerce
import Data.Coerce
import Build_model
import Game_sound

foreign import ccall "wingdi.h SwapBuffers"
  swapBuffers :: HDC -> IO Bool

-- A psudorandom number sequence used by the game logic to add an element of chance to certain game events (e.g. player damage when hit by projectiles).
prob_seq :: UArray Int Int
prob_seq = listArray (0, 239) [5, 1, 8, 5, 6, 4, 0, 9, 5, 2, 0, 9, 1, 7, 7, 6, 7, 0, 4, 5, 9, 6, 4, 3, 5, 9, 8, 8, 3, 1, 3, 7, 1, 7, 1, 5, 2, 4, 3, 2, 9, 8, 9, 9, 9, 4, 3, 0, 2, 2, 0, 8, 2, 5, 4, 4, 4, 7, 2, 1, 3, 7, 3, 4, 1, 0, 0, 0, 7, 3, 6, 9, 1, 9, 1, 9, 4, 3, 6, 0, 8, 0, 2, 8, 9, 6, 5, 8, 4, 7, 3, 7, 1, 7, 9, 2, 4, 7, 5, 9, 5, 0, 0, 1, 4, 1, 5, 3, 4, 9, 4, 6, 0, 1, 8, 4, 2, 5, 1, 5, 8, 3, 6, 4, 5, 9, 8, 6, 0, 9, 9, 7, 7, 4, 5, 2, 1, 8, 1, 0, 2, 8, 5, 1, 5, 7, 1, 7, 5, 2, 1, 5, 3, 4, 7, 3, 3, 2, 5, 2, 6, 8, 2, 8, 1, 1, 7, 1, 9, 7, 0, 6, 5, 7, 9, 3, 4, 1, 3, 9, 3, 8, 9, 5, 7, 7, 8, 2, 3, 2, 8, 5, 3, 7, 4, 8, 5, 9, 2, 1, 0, 5, 8, 5, 2, 6, 7, 3, 4, 5, 1, 9, 3, 3, 7, 4, 0, 8, 9, 1, 4, 6, 8, 5, 3, 7, 1, 5, 2, 3, 6, 9, 5, 7, 8, 5, 1, 5, 8, 4]

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

main_menu_text :: [(Int, [Int])]
main_menu_text = [(0, msg16), (0, []), (1, msg14), (2, msg18), (3, msg12)]

-- These two functions are where the program interacts with the Windows message system and capture keyboard input.
wndProc :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc hwnd wmsg wParam lParam
    | wmsg == 258 = if wParam == 120 then do return 2 -- pause and select menu option (X)
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
                    else if wParam == 32 then return 13 -- Fire (SPACE)
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

int_to_float :: Int -> Float
int_to_float x = (fromIntegral x) * 0.000001

fl_to_int :: Float -> Int
fl_to_int x = truncate (x * 1000000)

bool_to_int True = 1
bool_to_int False = 0

int_to_bool 0 = False
int_to_bool 1 = True

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
chg_state :: Int -> Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> [Int] -> Array (Int, Int, Int) Wall_grid
chg_state state_val abs v (i0, i1, i2) grid d_list =
  let index = (d_list !! i0, d_list !! i1, d_list !! i2)
      grid_i = fromJust (obj (grid ! index))
      grid_i' = (obj (grid ! index))
  in
  if isNothing grid_i' == True then grid
  else if d_list !! state_val == 0 then grid // [(index, (grid ! index) {obj = Just (grid_i {ident_ = upd (d_list !! abs) (ident_ grid_i) (d_list !! v)})})]
  else if d_list !! state_val == 1 then grid // [(index, (grid ! index) {obj = Just (grid_i {u__ = upd (d_list !! abs) (u__ grid_i) (int_to_float (d_list !! v))})})]
  else if d_list !! state_val == 2 then grid // [(index, (grid ! index) {obj = Just (grid_i {v__ = upd (d_list !! abs) (v__ grid_i) (int_to_float (d_list !! v))})})]
  else if d_list !! state_val == 3 then grid // [(index, (grid ! index) {obj = Just (grid_i {w__ = upd (d_list !! abs) (w__ grid_i) (int_to_float (d_list !! v))})})]
  else if d_list !! state_val == 9 then grid // [(index, (grid ! index) {obj = Just (grid_i {texture__ = upd (d_list !! abs) (texture__ grid_i) (d_list !! v)})})]
  else if d_list !! state_val == 10 then grid // [(index, (grid ! index) {obj = Just (grid_i {num_elem = fromIntegral (d_list !! v)})})]
  else if d_list !! state_val == 11 then grid // [(index, (grid ! index) {obj = Just (grid_i {obj_flag = d_list !! v})})]
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
  else if d_list !! state_val == 4 then s {keys = (take (d_list !! abs) (keys s)) ++ [d_list !! v] ++ drop ((d_list !! abs) + 1) (keys s), state_chg = 5}
  else if d_list !! state_val == 5 then s {difficulty = ("Hey, not too risky!", 6, 8, 10)}
  else if d_list !! state_val == 6 then s {difficulty = ("Plenty of danger please", 6, 10, 14)}
  else s {difficulty = ("Ultra danger", 10, 15, 20)}

copy_ps0 :: Int -> (Int, Int, Int) -> Play_state0 -> Array (Int, Int, Int) (Int, [Int]) -> [Int] -> Array (Int, Int, Int) (Int, [Int])
copy_ps0 offset (i0, i1, i2) s0 obj_grid d_list =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, (take offset (snd target)) ++ [fl_to_int (pos_u s0), fl_to_int (pos_v s0), fl_to_int (pos_w s0), fl_to_int ((vel s0) !! 0), fl_to_int ((vel s0) !! 1), fl_to_int ((vel s0) !! 2), angle s0, rend_mode s0, game_t s0, torch_t0 s0, torch_t_limit s0] ++ drop (offset + 11) (snd target)))]

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
  obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, (take offset (snd target)) ++ [obj_type w u v obj_grid, obj_type w u' v obj_grid, obj_type w u'' v obj_grid, obj_type w u v' obj_grid, obj_type w u v'' obj_grid, obj_type w u' v' obj_grid, obj_type w u'' v' obj_grid, obj_type w u' v'' obj_grid, obj_type w u'' v'' obj_grid, w_conf_v2, w_conf_u2, w_conf_v1, w_conf_u1] ++ drop (offset + 9) (snd target)))]

chg_obj_type :: Int -> (Int, Int, Int) -> [Int] -> Array (Int, Int, Int) (Int, [Int]) -> Array (Int, Int, Int) (Int, [Int])
chg_obj_type v (i0, i1, i2) d_list obj_grid =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (d_list !! v, snd target))]

pass_msg :: Int -> [Int] -> Play_state1 -> [Int] -> ([Int], Play_state1)
pass_msg len msg s d_list = (drop (d_list !! len) msg, s {message = message s ++ [head msg] ++ [(d_list !! len) - 1] ++ take ((d_list !! len) - 1) (tail msg)})

send_signal :: Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) (Int, [Int]) -> Play_state1 -> [Int] -> (Array (Int, Int, Int) (Int, [Int]), Play_state1)
send_signal 0 sig (i0, i1, i2) obj_grid s d_list =
  let dest = (d_list !! i0, d_list !! i1, d_list !! i2)
      prog = (snd (obj_grid ! dest))
  in
  if fst (obj_grid ! dest) == 1 || fst (obj_grid ! dest) == 3 then (obj_grid, s {next_sig_q = next_sig_q s ++ [d_list !! sig, d_list !! i0, d_list !! i1, d_list !! i2]})
  else (obj_grid, s)
send_signal 1 sig dest obj_grid s d_list =
  let prog = (snd (obj_grid ! dest))
  in
  (obj_grid // [(dest, (fst (obj_grid ! dest), (head prog) : sig : drop 2 prog))], s)

place_hold :: Int -> [Int] -> Play_state1 -> Play_state1
place_hold val d_list s = unsafePerformIO (putStr "\nplace_hold run with value " >> print (d_list !! val) >> return s)

project_init :: Int -> Int -> Int -> Int -> Int -> (Int, Int, Int) -> Int -> Int -> Array (Int, Int, Int) (Int, [Int]) -> [Int] -> UArray (Int, Int) Float -> Array (Int, Int, Int) (Int, [Int])
project_init u v w a vel (i0, i1, i2) offset obj_flag obj_grid d_list look_up =
  let location = (d_list !! i0, d_list !! i1, d_list !! i2)
      target = obj_grid ! location
  in
  obj_grid // [(location, (fst target, (take offset (snd target)) ++ [d_list !! u, d_list !! v, - (div (d_list !! w) 1000000) - 1, truncate ((look_up ! (2, d_list !! a)) * fromIntegral (d_list !! vel)), truncate ((look_up ! (1, d_list !! a)) * fromIntegral (d_list !! vel)), d_list !! i0, d_list !! i1, d_list !! i2, div (d_list !! u) 1000000, div (d_list !! v) 1000000, d_list !! obj_flag] ++ drop (offset + 11) (snd target)))]

project_update :: Int -> Int -> (Int, Int, Int) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) (Int, [Int]) -> Play_state0 -> Play_state1 -> [Int] -> (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) (Int, [Int]), Play_state1)
project_update p_state p_state' (i0, i1, i2) w_grid obj_grid s0 s1 d_list =
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
  if isNothing grid_i' == True then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1)
  else if u_block' == u_block && v_block' == v_block then (w_grid // [(index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})})], obj_grid // [(location, (fst target, (take p_state' (snd target)) ++ [u', v'] ++ drop (p_state' + 2) (snd target)))], s1)
  else if u_block' == u_block && v_block' == v_block + 1 then
    if v2 (w_grid ! (w_block_, u_block, v_block)) == True then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1)
    else if fst (obj_grid ! (w_block_, u_block, v_block + 1)) > 0 && fst (obj_grid ! (w_block_, u_block, v_block + 1)) < 4 then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [2, w_block_, u_block, v_block + 1] ++ drop (p_state' + 15) (snd target)))], s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block, v_block + 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) (game_t s0) <= 0 then (w_grid, obj_grid, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1 {health = health s1 - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg25})
    else (chg_grid 0 (1, 2, 3) (4, 5, 6) (w_grid // [(index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})})]) def_w_grid [1, w_block, u_block, v_block, w_block, u_block, v_block + 1], obj_grid // [(location, (fst target, (take p_state' (snd target)) ++ [u', v'] ++ drop (p_state' + 2) (snd target)))], s1)
  else if u_block' == u_block + 1 && v_block' == v_block then
    if u2 (w_grid ! (w_block_, u_block, v_block)) == True then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1)
    else if fst (obj_grid ! (w_block_, u_block + 1, v_block)) > 0 && fst (obj_grid ! (w_block_, u_block + 1, v_block)) < 4 then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [2, w_block_, u_block + 1, v_block] ++ drop (p_state' + 15) (snd target)))], s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block + 1, v_block) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) (game_t s0) <= 0 then (w_grid, obj_grid, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1 {health = health s1 - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg25})
    else (chg_grid 0 (1, 2, 3) (4, 5, 6) (w_grid // [(index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})})]) def_w_grid [1, w_block, u_block, v_block, w_block, u_block + 1, v_block], obj_grid // [(location, (fst target, (take p_state' (snd target)) ++ [u', v'] ++ drop (p_state' + 2) (snd target)))], s1)
  else if u_block' == u_block && v_block' == v_block - 1 then
    if v1 (w_grid ! (w_block_, u_block, v_block)) == True then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1)
    else if fst (obj_grid ! (w_block_, u_block, v_block - 1)) > 0 && fst (obj_grid ! (w_block_, u_block, v_block - 1)) < 4 then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [2, w_block_, u_block, v_block - 1] ++ drop (p_state' + 15) (snd target)))], s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block, v_block - 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) (game_t s0) <= 0 then (w_grid, obj_grid, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1 {health = health s1 - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg25})
    else (chg_grid 0 (1, 2, 3) (4, 5, 6) (w_grid // [(index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})})]) def_w_grid [1, w_block, u_block, v_block, w_block, u_block, v_block - 1], obj_grid // [(location, (fst target, (take p_state' (snd target)) ++ [u', v'] ++ drop (p_state' + 2) (snd target)))], s1)
  else if u_block' == u_block - 1 && v_block' == v_block then
    if u1 (w_grid ! (w_block_, u_block, v_block)) == True then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1)
    else if fst (obj_grid ! (w_block_, u_block - 1, v_block)) > 0 && fst (obj_grid ! (w_block_, u_block - 1, v_block)) < 4 then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [2, w_block_, u_block - 1, v_block] ++ drop (p_state' + 15) (snd target)))], s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block - 1, v_block) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) (game_t s0) <= 0 then (w_grid, obj_grid, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1 {health = health s1 - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg25})
    else (chg_grid 0 (1, 2, 3) (4, 5, 6) (w_grid // [(index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})})]) def_w_grid [1, w_block, u_block, v_block, w_block, u_block - 1 , v_block], obj_grid // [(location, (fst target, (take p_state' (snd target)) ++ [u', v'] ++ drop (p_state' + 2) (snd target)))], s1)
  else if u_block' == u_block + 1 && v_block' == v_block + 1 then
    if u2 (w_grid ! (w_block_, u_block, v_block)) == True then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1)
    else if fst (obj_grid ! (w_block_, u_block + 1, v_block + 1)) > 0 && fst (obj_grid ! (w_block_, u_block + 1, v_block + 1)) < 4 then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [2, w_block_, u_block + 1, v_block + 1] ++ drop (p_state' + 15) (snd target)))], s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block + 1, v_block + 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) (game_t s0) <= 0 then (w_grid, obj_grid, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1 {health = health s1 - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg25})
    else (chg_grid 0 (1, 2, 3) (4, 5, 6) (w_grid // [(index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})})]) def_w_grid [1, w_block, u_block, v_block, w_block, u_block + 1 , v_block + 1], obj_grid // [(location, (fst target, (take p_state' (snd target)) ++ [u', v'] ++ drop (p_state' + 2) (snd target)))], s1)
  else if u_block' == u_block + 1 && v_block' == v_block - 1 then
    if v1 (w_grid ! (w_block_, u_block, v_block)) == True then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1)
    else if fst (obj_grid ! (w_block_, u_block + 1, v_block - 1)) > 0 && fst (obj_grid ! (w_block_, u_block + 1, v_block - 1)) < 4 then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [2, w_block_, u_block + 1, v_block - 1] ++ drop (p_state' + 15) (snd target)))], s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block + 1, v_block - 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) (game_t s0) <= 0 then (w_grid, obj_grid, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1 {health = health s1 - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg25})
    else (chg_grid 0 (1, 2, 3) (4, 5, 6) (w_grid // [(index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})})]) def_w_grid [1, w_block, u_block, v_block, w_block, u_block + 1 , v_block - 1], obj_grid // [(location, (fst target, (take p_state' (snd target)) ++ [u', v'] ++ drop (p_state' + 2) (snd target)))], s1)
  else if u_block' == u_block - 1 && v_block' == v_block - 1 then
    if u1 (w_grid ! (w_block_, u_block, v_block)) == True then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1)
    else if fst (obj_grid ! (w_block_, u_block - 1, v_block - 1)) > 0 && fst (obj_grid ! (w_block_, u_block - 1, v_block - 1)) < 4 then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [2, w_block_, u_block - 1, v_block - 1] ++ drop (p_state' + 15) (snd target)))], s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block - 1, v_block - 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) (game_t s0) <= 0 then (w_grid, obj_grid, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1 {health = health s1 - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg25})
    else (chg_grid 0 (1, 2, 3) (4, 5, 6) (w_grid // [(index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})})]) def_w_grid [1, w_block, u_block, v_block, w_block, u_block - 1 , v_block - 1], obj_grid // [(location, (fst target, (take p_state' (snd target)) ++ [u', v'] ++ drop (p_state' + 2) (snd target)))], s1)
  else
    if v2 (w_grid ! (w_block_, u_block, v_block)) == True then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1)
    else if fst (obj_grid ! (w_block_, u_block - 1, v_block + 1)) > 0 && fst (obj_grid ! (w_block_, u_block - 1, v_block + 1)) < 4 then (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [2, w_block_, u_block - 1, v_block + 1] ++ drop (p_state' + 15) (snd target)))], s1)
    else if (truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)) == (w_block_, u_block - 1, v_block + 1) && d_list !! i0 /= 0 then
      if health s1 - det_damage (difficulty s1) (game_t s0) <= 0 then (w_grid, obj_grid, s1 {health = 0, state_chg = 1, message = 0 : msg27})
      else (w_grid // [((w_block, u_block, v_block), def_w_grid)], obj_grid // [(location, (fst target, (take (p_state' + 11) (snd target)) ++ [1] ++ drop (p_state' + 12) (snd target)))], s1 {health = health s1 - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg25})
    else (chg_grid 0 (1, 2, 3) (4, 5, 6) (w_grid // [(index, (w_grid ! index) {obj = Just (grid_i {u__ = u__ grid_i + int_to_float vel_u, v__ = v__ grid_i + int_to_float vel_v})})]) def_w_grid [1, w_block, u_block, v_block, w_block, u_block - 1 , v_block + 1], obj_grid // [(location, (fst target, (take p_state' (snd target)) ++ [u', v'] ++ drop (p_state' + 2) (snd target)))], s1)

det_damage :: ([Char], Int, Int, Int) -> Int -> Int
det_damage (d, low, med, high) game_t =
  if prob_seq ! (mod game_t 240) < 2 then low
  else if prob_seq ! (mod game_t 240) > 7 then high
  else med

sort_path :: [Int] -> Int -> Int -> Int -> Int
sort_path [] best path i = path + 2
sort_path (x:xs) best path i =
  if x < 1 then sort_path xs best path (i + 1)
  else if x < best then sort_path xs x i (i + 1)
  else sort_path xs best path (i + 1)

test_path :: Int -> Int -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) (Int, [Int]) -> [Int] -> Int
test_path path dir pl_u_block pl_v_block w_grid obj_grid d_list =
  let w_block = d_list !! 2
      u_block = d_list !! 3
      v_block = d_list !! 4
  in
  if dir == 0 && path == 2 then -1
  else if dir == 1 && path == 3 then -1
  else if dir == 2 && path == 0 then -1
  else if dir == 3 && path == 1 then -1
  else if path == 0 && (u2 (w_grid ! (w_block, u_block, v_block)) == True || fst (obj_grid ! (w_block, u_block + 1, v_block)) > 0) then -1
  else if path == 1 && (v1 (w_grid ! (w_block, u_block, v_block)) == True || fst (obj_grid ! (w_block, u_block, v_block - 1)) > 0) then -1
  else if path == 2 && (u1 (w_grid ! (w_block, u_block, v_block)) == True || fst (obj_grid ! (w_block, u_block - 1, v_block)) > 0) then -1
  else if path == 3 && (v2 (w_grid ! (w_block, u_block, v_block)) == True || fst (obj_grid ! (w_block, u_block, v_block + 1)) > 0) then -1
  else if path == 0 then pl_u_block - u_block
  else if path == 1 then v_block - pl_v_block
  else if path == 2 then u_block - pl_u_block
  else pl_v_block - v_block

cpede_logic :: Int -> Int -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) (Int, [Int]) -> Play_state0 -> Play_state1 -> [Int] -> (Array (Int, Int, Int) (Int, [Int]), Play_state1)
cpede_logic m n offset num_seg w_grid obj_grid s0 s1 d_list =
  let target = obj_grid ! (w_block, u_block, v_block)
      dir = d_list !! 1
      w_block = d_list !! 2
      u_block = d_list !! 3
      v_block = d_list !! 4
      pl_w_block = truncate (pos_w s0)
      pl_u_block = truncate (pos_u s0)
      pl_v_block = truncate (pos_v s0)
      dir_seq = take num_seg (drop 8 d_list)
  in
  if m == 0 then
    if mod (d_list !! n) 20 == 0 then cpede_logic 1 0 offset num_seg w_grid obj_grid s0 s1 d_list
    else (obj_grid // [((w_block, u_block, v_block), (fst target, take offset (snd target) ++ [n + 1] ++ drop (offset + 1) (snd target)))], s1)
  else if m == 1 then
    if (pl_u_block - u_block == 1 || pl_u_block - u_block == -1) && pl_v_block - v_block == 0 && pl_w_block == w_block then
      if health s1 - det_damage (difficulty s1) (game_t s0) <= 0 then (obj_grid, s1 {health = 0, state_chg = 1, message = 0 : msg28})
      else (obj_grid // [((w_block, u_block, v_block), (fst target, take offset (snd target) ++ [n + 1] ++ drop (offset + 1) (snd target)))], s1 {health = health s1 - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg25})
    else if (pl_v_block - v_block == 1 || pl_v_block - v_block == -1) && pl_u_block - u_block == 0 && pl_w_block == w_block then
      if health s1 - det_damage (difficulty s1) (game_t s0) <= 0 then (obj_grid, s1 {health = 0, state_chg = 1, message = 0 : msg28})
      else (obj_grid // [((w_block, u_block, v_block), (fst target, take offset (snd target) ++ [n + 1] ++ drop (offset + 1) (snd target)))], s1 {health = health s1 - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg25})
    else cpede_logic (sort_path [test_path 0 dir pl_u_block pl_v_block w_grid obj_grid d_list, test_path 1 dir pl_u_block pl_v_block w_grid obj_grid d_list, test_path 2 dir pl_u_block pl_v_block w_grid obj_grid d_list, test_path 3 dir pl_u_block pl_v_block w_grid obj_grid d_list] 101 0 0) 0 offset num_seg w_grid obj_grid s0 s1 d_list
  else (obj_grid // [((w_block, u_block, v_block), (fst target, 1 : m : take (offset - 2) (drop 2 (snd target)) ++ [n + 1] ++ [m] ++ take 6 (drop (offset + 2) (snd target)) ++ tail dir_seq ++ [m] ++ drop (offset + 16) (snd target)))], s1 {next_sig_q = next_sig_q s1 ++ [w_block, u_block, v_block]})

cpede_upd_hpos :: Int -> [Int] -> [Int]
cpede_upd_hpos 2 [w, u, v] = [w, u + 1, v]
cpede_upd_hpos 3 [w, u, v] = [w, u, v - 1]
cpede_upd_hpos 4 [w, u, v] = [w, u - 1, v]
cpede_upd_hpos 5 [w, u, v] = [w, u, v + 1]

cpede_move :: Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) (Int, [Int]) -> [Int] -> (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) (Int, [Int]))
cpede_move mode dir offset0 offset1 seg_num w_grid obj_grid d_list =
  let target = obj_grid ! (w_block, u_block, v_block)
      w_block = d_list !! 2
      u_block = d_list !! 3
      v_block = d_list !! 4
  in
  if mode == 0 then
    if dir == 2 then (chg_grid 1 (0, 1, 2) (3, 4, 5) w_grid def_w_grid [w_block, u_block, v_block, w_block, u_block + 1, v_block], chg_grid 1 (0, 1, 2) (3, 4, 5) (obj_grid // [((w_block, u_block, v_block), (fst target, take (offset0 + 2) (snd target) ++ [w_block, u_block + 1, v_block, w_block, u_block, v_block] ++ drop (offset0 + 8) (snd target)))]) def_obj_grid [w_block, u_block, v_block, w_block, u_block + 1, v_block])
    else if dir == 3 then (chg_grid 1 (0, 1, 2) (3, 4, 5) w_grid def_w_grid [w_block, u_block, v_block, w_block, u_block, v_block - 1], chg_grid 1 (0, 1, 2) (3, 4, 5) (obj_grid // [((w_block, u_block, v_block), (fst target, take (offset0 + 2) (snd target) ++ [w_block, u_block, v_block - 1, w_block, u_block, v_block] ++ drop (offset0 + 8) (snd target)))]) def_obj_grid [w_block, u_block, v_block, w_block, u_block, v_block - 1])
    else if dir == 4 then (chg_grid 1 (0, 1, 2) (3, 4, 5) w_grid def_w_grid [w_block, u_block, v_block, w_block, u_block - 1, v_block], chg_grid 1 (0, 1, 2) (3, 4, 5) (obj_grid // [((w_block, u_block, v_block), (fst target, take (offset0 + 2) (snd target) ++ [w_block, u_block - 1, v_block, w_block, u_block, v_block] ++ drop (offset0 + 8) (snd target)))]) def_obj_grid [w_block, u_block, v_block, w_block, u_block - 1, v_block])
    else (chg_grid 1 (0, 1, 2) (3, 4, 5) w_grid def_w_grid [w_block, u_block, v_block, w_block, u_block, v_block + 1], chg_grid 1 (0, 1, 2) (3, 4, 5) (obj_grid // [((w_block, u_block, v_block), (fst target, take (offset0 + 2) (snd target) ++ [w_block, u_block, v_block + 1, w_block, u_block, v_block] ++ drop (offset0 + 8) (snd target)))]) def_obj_grid [w_block, u_block, v_block, w_block, u_block, v_block + 1])
  else
    if (snd (obj_grid ! (d_list !! 8, d_list !! 9, d_list !! 10))) !! (offset0 + 8 + seg_num) == 2 then (chg_grid 1 (0, 1, 2) (3, 4, 5) w_grid def_w_grid [w_block, u_block, v_block, w_block, u_block + 1, v_block], chg_grid 1 (0, 1, 2) (3, 4, 5) (obj_grid //[((w_block, u_block, v_block), (fst target, take (offset1 + 2) (snd target) ++ [w_block, u_block + 1, v_block, w_block, u_block, v_block] ++ cpede_upd_hpos 2 [w_block, u_block, v_block] ++ drop (offset1 + 11) (snd target)))]) def_obj_grid [w_block, u_block, v_block, w_block, u_block + 1, v_block])
    else if (snd (obj_grid ! (d_list !! 8, d_list !! 9, d_list !! 10))) !! (offset0 + 8 + seg_num) == 3 then (chg_grid 1 (0, 1, 2) (3, 4, 5) w_grid def_w_grid [w_block, u_block, v_block, w_block, u_block, v_block - 1], chg_grid 1 (0, 1, 2) (3, 4, 5) (obj_grid //[((w_block, u_block, v_block), (fst target, take (offset1 + 2) (snd target) ++ [w_block, u_block, v_block - 1, w_block, u_block, v_block] ++ cpede_upd_hpos 3 [w_block, u_block, v_block] ++ drop (offset1 + 11) (snd target)))]) def_obj_grid [w_block, u_block, v_block, w_block, u_block, v_block - 1])
    else if (snd (obj_grid ! (d_list !! 8, d_list !! 9, d_list !! 10))) !! (offset0 + 8 + seg_num) == 4 then (chg_grid 1 (0, 1, 2) (3, 4, 5) w_grid def_w_grid [w_block, u_block, v_block, w_block, u_block - 1, v_block], chg_grid 1 (0, 1, 2) (3, 4, 5) (obj_grid //[((w_block, u_block, v_block), (fst target, take (offset1 + 2) (snd target) ++ [w_block, u_block - 1, v_block, w_block, u_block, v_block] ++ cpede_upd_hpos 4 [w_block, u_block, v_block] ++ drop (offset1 + 11) (snd target)))]) def_obj_grid [w_block, u_block, v_block, w_block, u_block - 1, v_block])
    else (chg_grid 1 (0, 1, 2) (3, 4, 5) w_grid def_w_grid [w_block, u_block, v_block, w_block, u_block, v_block + 1], chg_grid 1 (0, 1, 2) (3, 4, 5) (obj_grid //[((w_block, u_block, v_block), (fst target, take (offset1 + 2) (snd target) ++ [w_block, u_block, v_block + 1, w_block, u_block, v_block] ++ cpede_upd_hpos 5 [w_block, u_block, v_block] ++ drop (offset1 + 11) (snd target)))]) def_obj_grid [w_block, u_block, v_block, w_block, u_block, v_block + 1])

cpede_damage :: (Int, Int, Int) -> Int -> Array (Int, Int, Int) (Int, [Int]) -> Play_state0 -> Play_state1 -> [Int] -> (Array (Int, Int, Int) (Int, [Int]), Play_state1)
cpede_damage (i0, i1, i2) offset obj_grid s0 s1 d_list =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
      damage = det_damage ([], 6, 10, 14) (game_t s0)
  in
  if (d_list !! 16) - damage < 1 then (obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, 1 : 7 : drop 2 (snd target)))], s1 {next_sig_q = next_sig_q s1 ++ [d_list !! i0, d_list !! i1, d_list !! i2]})
  else (obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, 1 : 8 : take 14 (drop (offset + 2) (snd target)) ++ [(d_list !! 16) - damage] ++ drop (offset + 17) (snd target)))], s1 {next_sig_q = next_sig_q s1 ++ [d_list !! i0, d_list !! i1, d_list !! i2]})

binary_dice :: Int -> Int -> (Int, Int, Int) -> Int -> Int -> Array (Int, Int, Int) (Int, [Int]) -> [Int] -> Array (Int, Int, Int) (Int, [Int])
binary_dice prob diff (i0, i1, i2) offset game_t obj_grid d_list =
  let target = obj_grid ! (d_list !! i0, d_list !! i1, d_list !! i2)
  in
  if prob_seq ! (mod (game_t + (d_list !! diff)) 240) < d_list !! prob then obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, (take offset (snd target)) ++ [1] ++ drop (offset + 1) (snd target)))]
  else obj_grid // [((d_list !! i0, d_list !! i1, d_list !! i2), (fst target, (take offset (snd target)) ++ [0] ++ drop (offset + 1) (snd target)))]

-- Branch on each GPLC op - code to call the corresponding function, with optional per op - code status reports for debugging.
run_gplc :: [Int] -> [Int] -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> Play_state0 -> Play_state1 -> (Int, Int, Int) -> UArray (Int, Int) Float -> Int -> IO (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) (Int, [Int]), Play_state0, Play_state1)
run_gplc [] d_list w_grid f_grid obj_grid s0 s1 location look_up c = return (w_grid, f_grid, obj_grid, s0, s1)
run_gplc code d_list w_grid f_grid obj_grid s0 s1 location look_up 0 = do
  report_state (verbose_mode s1) 2 [] [] "\non_signal run.  Initial state is..."
  report_state (verbose_mode s1) 0 (snd (obj_grid ! location)) ((splitOn [536870911] code) !! 2) []
  run_gplc (on_signal (drop 2 ((splitOn [536870911] code) !! 0)) ((splitOn [536870911] code) !! 1) (code !! 1)) ((splitOn [536870911] code) !! 2) w_grid f_grid obj_grid s0 s1 location look_up 1
run_gplc code d_list w_grid f_grid obj_grid s0 s1 location look_up 1 =
  let if0' = if0 code d_list
  in do
  report_state (verbose_mode s1) 2 [] [] "\nif expression folding run.  State is..."
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  run_gplc (tail_ if0') d_list w_grid f_grid obj_grid s0 s1 location look_up (head_ if0')
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 2 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_state run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5))
  run_gplc (tail_ xs) d_list (chg_state x0 x1 x2 (x3, x4, x5) w_grid d_list) f_grid obj_grid s0 s1 location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 3 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_grid run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5) ++ " 6: " ++ show (d_list !! x6))
  run_gplc (tail_ xs) d_list (chg_grid x0 (x1, x2, x3) (x4, x5, x6) w_grid def_w_grid d_list) f_grid obj_grid s0 s1 location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 4 =
  let sig = send_signal 0 x0 (x1, x2, x3) obj_grid s1 d_list
  in do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nsend_signal run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  run_gplc (tail_ xs) d_list w_grid f_grid (fst sig) s0 (snd sig) location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 5 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_value run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5))
  run_gplc (tail_ xs) d_list w_grid f_grid (chg_value x0 x1 x2 (x3, x4, x5) d_list obj_grid) s0 s1 location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 6 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_floor run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5))
  run_gplc (tail_ xs) d_list w_grid (chg_floor x0 x1 x2 (x3, x4, x5) f_grid d_list) obj_grid s0 s1 location look_up (head_ xs)
run_gplc (x0:x1:x2:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 7 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_ps1 run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2))
  run_gplc (tail_ xs) d_list w_grid f_grid obj_grid s0 (chg_ps1 x0 x1 x2 d_list s1) location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 8 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_obj_type run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  run_gplc (tail_ xs) d_list w_grid f_grid (chg_obj_type x0 (x1, x2, x3) d_list obj_grid) s0 s1 location look_up (head_ xs)
run_gplc (x0:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 9 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  run_gplc (tail_ xs) d_list w_grid f_grid obj_grid s0 (place_hold x0 d_list s1) location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 10 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_grid_ run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5) ++ " 6: " ++ show (d_list !! x6))
  run_gplc (tail_ xs) d_list w_grid f_grid (chg_grid x0 (x1, x2, x3) (x4, x5, x6) obj_grid def_obj_grid d_list) s0 s1 location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 11 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\ncopy_ps1 run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  run_gplc (tail_ xs) d_list w_grid f_grid (copy_ps1 x0 (x1, x2, x3) s1 obj_grid d_list) s0 s1 location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:x6:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 12 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\ncopy_lstate run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5) ++ " 6: " ++ show (d_list !! x6))
  run_gplc (tail_ xs) d_list w_grid f_grid (copy_lstate x0 (x1, x2, x3) (x4, x5, x6) w_grid obj_grid d_list) s0 s1 location look_up (head_ xs)
run_gplc (x:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 13 =
  let pass_msg' = pass_msg x xs s1 d_list
  in do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\npass_msg run with arguments " ++ "msg_length: " ++ show (d_list !! x) ++ " message data: " ++ show (take (d_list !! x) xs))
  run_gplc (tail_ (fst pass_msg')) d_list w_grid f_grid obj_grid s0 (snd pass_msg') location look_up (head_ (fst pass_msg'))
run_gplc (x0:x1:x2:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 14 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nchg_ps0 run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2))
  run_gplc (tail_ xs) d_list w_grid f_grid obj_grid (chg_ps0 x0 x1 x2 d_list s0) s1 location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 15 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\ncopy_ps0 run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3))
  run_gplc (tail_ xs) d_list w_grid f_grid (copy_ps0 x0 (x1, x2, x3) s0 obj_grid d_list) s0 s1 location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 16 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nbinary_dice run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4) ++ " 5: " ++ show x5)
  run_gplc (tail_ xs) d_list w_grid f_grid (binary_dice x0 x1 (x2, x3, x4) x5 (game_t s0) obj_grid d_list) s0 s1 location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 17 = do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nproject_init run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ "4: " ++ show (d_list !! x4) ++ " 5: " ++ show (d_list !! x5) ++ " 6: " ++ show (d_list !! x6) ++ " 7: " ++ show (d_list !! x7) ++ " 8: " ++ show x8)
  run_gplc (tail_ xs) d_list w_grid f_grid (project_init x0 x1 x2 x3 x4 (x5, x6, x7) x8 x9 obj_grid d_list look_up) s0 s1 location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 18 =
  let project_update' = project_update x0 x1 (x2, x3, x4) w_grid obj_grid s0 s1 d_list
  in do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\nproject_update run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show x1 ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show (d_list !! x3) ++ " 4: " ++ show (d_list !! x4))
  run_gplc (tail_ xs) d_list (fst__ project_update') f_grid (snd__ project_update') s0 (third_ project_update') location look_up (head_ xs)
run_gplc (x0:x1:x2:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 19 =
  let cpede_logic' = cpede_logic 0 x0 x1 x2 w_grid obj_grid s0 s1 d_list
  in do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\ncpede_logic run with arguments " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show x1 ++ " 2: " ++ show x2)
  run_gplc (tail_ xs) d_list w_grid f_grid (fst cpede_logic') s0 (snd cpede_logic') location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:x4:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 20 =
  let cpede_move' = cpede_move x0 x1 x2 x3 x4 w_grid obj_grid d_list
  in do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\ncpede_move run with arguments " ++ "0: " ++ show x0 ++ " 1: " ++ show x1 ++ " 2: " ++ show x2 ++ " 3: " ++ show x3 ++ " 4: " ++ show x4)
  run_gplc (tail_ xs) d_list (fst cpede_move') f_grid (snd cpede_move') s0 s1 location look_up (head_ xs)
run_gplc (x0:x1:x2:x3:xs) d_list w_grid f_grid obj_grid s0 s1 location look_up 21 =
  let cpede_damage' = cpede_damage (x0, x1, x2) x3 obj_grid s0 s1 d_list
  in do
  report_state (verbose_mode s1) 1 (snd (obj_grid ! location)) [] []
  report_state (verbose_mode s1) 2 [] [] ("\ncpede_damage run with arguments: " ++ "0: " ++ show (d_list !! x0) ++ " 1: " ++ show (d_list !! x1) ++ " 2: " ++ show (d_list !! x2) ++ " 3: " ++ show x3)
  run_gplc (tail_ xs) d_list w_grid f_grid (fst cpede_damage') s0 (snd cpede_damage') location look_up (head_ xs)
run_gplc code d_list w_grid f_grid obj_grid s0 s1 location look_up c = do
  putStr ("\nInvalid opcode: " ++ show c)
  putStr ("\nremaining code block: " ++ show code)
  throw Invalid_GPLC_opcode

-- These functions deal with GPLC debugging output and error reporting.
report_state :: Bool -> Int -> [Int] -> [Int] -> [Char] -> IO ()
report_state False mode prog d_list message = return ()
report_state True 0 prog d_list message = do
  putStr ("\nProgram list: " ++ show prog)
  putStr ("\nData list: " ++ show d_list)
report_state True 1 prog d_list message = putStr ("\nProgram list: " ++ show prog)
report_state True 2 prog d_list message = putStr message

gplc_error :: Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> Play_state0 -> Play_state1 -> SomeException -> IO (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) (Int, [Int]), Play_state0, Play_state1)
gplc_error w_grid f_grid obj_grid s0 s1 e = do
  putStr ("\nA GPLC program in the map has had a runtime exception and Game :: Dangerous engine is designed to shut down in this case.  Exception thrown: " ++ show e)
  putStr "\nPlease see the readme.txt file for details of how to report this bug."
  exitSuccess
  return (w_grid, f_grid, obj_grid, s0, s1)

-- These two functions (together with send_signal) implement the signalling system that drives GPLC program runs.  This involves signalling programs in response to player object collisions and handling
-- the signal queue, which allows programs to signal each other.
link_gplc0 :: [Float] -> [Int] -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> Play_state0 -> Play_state1 -> UArray (Int, Int) Float -> Bool -> IO (Array (Int, Int, Int) Wall_grid, Array (Int, Int, Int) Floor_grid, Array (Int, Int, Int) (Int, [Int]), Play_state0, Play_state1)
link_gplc0 (x0:x1:xs) (z0:z1:z2:zs) w_grid f_grid obj_grid s0 s1 look_up swap_flag =
  let dest = ((sig_q s1) !! 1, (sig_q s1) !! 2, (sig_q s1) !! 3)
      prog = (snd (obj_grid ! dest))
      obj_grid0' = (send_signal 1 1 (z0, z1, z2 + 1) obj_grid s1 [])
      obj_grid1' = (send_signal 1 1 (z0, z1 + 1, z2) obj_grid s1 [])
      obj_grid2' = (send_signal 1 1 (z0, z1, z2 - 1) obj_grid s1 [])
      obj_grid3' = (send_signal 1 1 (z0, z1 - 1, z2) obj_grid s1 [])
      obj_grid4' = obj_grid // [(dest, (fst (obj_grid ! dest), (head prog) : ((sig_q s1) !! 0) : drop 2 prog))]
  in do
  if sig_q s1 == [] && swap_flag == False then link_gplc0 (x0:x1:xs) (z0:z1:z2:zs) w_grid f_grid obj_grid s0 (s1 {sig_q = next_sig_q s1, next_sig_q = []}) look_up True
  else if swap_flag == True then
    if (x1 == 1 || x1 == 3) && x0 == 0 && head (snd (obj_grid ! (z0, z1, z2 + 1))) == 0 then do
      report_state (verbose_mode s1) 2 [] [] ("\nPlayer starts GPLC program at Obj_grid " ++ show (z0, z1, z2 + 1))
      catch (run_gplc (snd ((fst obj_grid0') ! (z0, z1, z2 + 1))) [] w_grid f_grid (fst obj_grid0') s0 s1 (z0, z1, z2 + 1) look_up 0) (\e -> gplc_error w_grid f_grid obj_grid s0 s1 e)
    else if (x1 == 1 || x1 == 3) && x0 == 1 && head (snd (obj_grid ! (z0, z1 + 1, z2))) == 0 then do
      report_state (verbose_mode s1) 2 [] [] ("\nPlayer starts GPLC program at Obj_grid " ++ show (z0, z1 + 1, z2))
      catch (run_gplc (snd ((fst obj_grid1') ! (z0, z1 + 1, z2))) [] w_grid f_grid (fst obj_grid1') s0 s1 (z0, z1 + 1, z2) look_up 0) (\e -> gplc_error w_grid f_grid obj_grid s0 s1 e)
    else if (x1 == 1 || x1 == 3) && x0 == 2 && head (snd (obj_grid ! (z0, z1, z2 - 1))) == 0 then do
      report_state (verbose_mode s1) 2 [] [] ("\nPlayer starts GPLC program at Obj_grid " ++ show (z0, z1, z2 - 1))
      catch (run_gplc (snd ((fst obj_grid2') ! (z0, z1, z2 - 1))) [] w_grid f_grid (fst obj_grid2') s0 s1 (z0, z1, z2 - 1) look_up 0) (\e -> gplc_error w_grid f_grid obj_grid s0 s1 e)
    else if (x1 == 1 || x1 == 3) && x0 == 3 && head (snd (obj_grid ! (z0, z1 - 1, z2))) == 0 then do
      report_state (verbose_mode s1) 2 [] [] ("\nPlayer starts GPLC program at Obj_grid " ++ show (z0, z1 - 1, z2))
      catch (run_gplc (snd ((fst obj_grid3') ! (z0, z1 - 1, z2))) [] w_grid f_grid (fst obj_grid3') s0 s1 (z0, z1 - 1, z2) look_up 0) (\e -> gplc_error w_grid f_grid obj_grid s0 s1 e)
    else return (w_grid, f_grid, obj_grid, s0, s1)
  else do
    if fst (obj_grid ! dest) == 1 || fst (obj_grid ! dest) == 3 then do
      report_state (verbose_mode s1) 2 [] [] ("\nGPLC program run at Obj_grid " ++ show ((sig_q s1) !! 1, (sig_q s1) !! 2, (sig_q s1) !! 3))
      run_gplc' <- catch (run_gplc (snd (obj_grid4' ! dest)) [] w_grid f_grid obj_grid4' s0 (s1 {sig_q = drop 4 (sig_q s1)}) dest look_up 0) (\e -> gplc_error w_grid f_grid obj_grid s0 s1 e)
      link_gplc0 (x0:x1:xs) (z0:z1:z2:zs) (fst_ run_gplc') (snd_ run_gplc') (third run_gplc') (fourth run_gplc') (fifth run_gplc') look_up False
    else link_gplc0 (x0:x1:xs) (z0:z1:z2:zs) w_grid f_grid obj_grid s0 (s1 {sig_q = drop 4 (sig_q s1)}) look_up False

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
      if health s1 <= det_damage (difficulty s1) (game_t s0) then return s1 {health = 0, state_chg = 1, message = 0 : msg26}
      else return s1 {sig_q = sig_q s1 ++ [1] ++ dest1, health = (health s1) - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg13}
    else do
      if health s1 <= det_damage (difficulty s1) (game_t s0) then return s1 {health = 0, state_chg = 1, message = 0 : msg26}
      else return s1 {health = health s1 - det_damage (difficulty s1) (game_t s0), state_chg = 1, message = 0 : msg13}

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

-- This function recurses once per game logic clock tick and is the central branching point of the game logic thread.
update_play :: Io_box -> MVar (Play_state0, Array (Int, Int, Int) Wall_grid, Save_state) -> Play_state0 -> Play_state1 -> Bool -> Float -> (Float, Float, Float, Float) -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) (Int, [Int]) -> UArray (Int, Int) Float -> Array Int [Char] -> Save_state -> Array Int Source -> IO ()
update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array =
  let det = detect_coll (truncate (pos_w s0)) (pos_u s0, pos_v s0) ((vel s0) !! 0 / f_rate, (vel s0) !! 1 / f_rate) obj_grid w_grid
      floor = floor_surf (det !! 0) (det !! 1) (pos_w s0) f_grid
      vel_0 = update_vel (vel s0) [0, 0, 0] ((drop 2 det) ++ [0]) f_rate f
      vel_2 = update_vel (vel s0) [0, 0, g] ((drop 2 det) ++ [0]) f_rate 0
      game_t' = game_t s0 + 1
  in do
  control <- messagePump (hwnd_ io_box)
  link0 <- link_gplc0 (drop 4 det) [truncate (pos_w s0), truncate (pos_u s0), truncate (pos_v s0)] w_grid f_grid obj_grid (s0 ) s1 look_up False
  link1 <- link_gplc1 s0 s1 obj_grid 0
  link1_ <- link_gplc1 s0 s1 obj_grid 1
  if control == 2 then do
    choice <- run_menu (pause_text s1 (difficulty s1)) [] io_box (-0.75) (-0.75) 1 0 0
    if choice == 1 then update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array
    else if choice == 2 then do
      update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg (Save_state {is_set = True, w_grid_ = w_grid, f_grid_ = f_grid, obj_grid_ = obj_grid, s0_ = s0, s1_ = s1}) sound_array
    else if choice == 3 then do
      putMVar state_ref (s0 {msg_count = -1}, w_grid, save_state)
      update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array
    else do
      putMVar state_ref (s0 {msg_count = -3}, w_grid, save_state)
      update_play io_box state_ref s0 s1 in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array
  else if control == 10 then update_play io_box state_ref (fourth link0) ((fifth link0) {sig_q = sig_q s1 ++ [2, 0, 0, 0]}) in_flight f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
  else if control == 11 then do
    if view_mode s0 == 0 then update_play io_box state_ref ((fourth link0) {view_mode = 1}) (fifth link0) in_flight f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
    else update_play io_box state_ref ((fourth link0) {view_mode = 0}) (fifth link0) in_flight f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
  else if control == 12 then update_play io_box state_ref ((fourth link0) {view_angle = mod_angle (view_angle s0) 5}) (fifth link0) in_flight f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
  else if control == 13 then update_play io_box state_ref (fourth link0) ((fifth link0) {sig_q = sig_q s1 ++ [2, 0, 0, 1]}) in_flight f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
  else if message s1 /= [] then do
    event <- proc_msg0 (message s1) s0 s1 io_box sound_array
    putMVar state_ref (fst event, w_grid, save_state)
    update_play io_box state_ref ((fst event) {msg_count = 0}) (snd event) in_flight f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array
  else
    if in_flight == False then
      if (pos_w s0) - floor > 0.02 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, vel = vel_0, game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
      else if control > 2 && control < 7 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = update_vel (vel s0) (take 3 (thrust (fromIntegral control) (angle s0) mag_r f_rate look_up)) ((drop 2 det) ++ [0]) f_rate f, game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
      else if control == 7 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, angle = mod_angle (angle s0) (angle_step s1)}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, angle = mod_angle (angle s0) (angle_step s1), game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
      else if control == 8 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, angle = mod_angle (angle s0) (- (angle_step s1))}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, angle = mod_angle (angle s0) (- (angle_step s1)), game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
      else if control == 9 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor + mag_j / f_rate}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor + mag_j / f_rate, vel = (take 2 vel_0) ++ [mag_j], game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
      else if control == 13 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, game_t = game_t'}) ((fifth link0) {sig_q = sig_q s1 ++ [0, 0, 1]}) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (fst (send_signal 1 1 (0, 0, 1) (third link0) s1 [])) look_up conf_reg save_state sound_array
      else do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, game_t = game_t'}) (fifth link0) False f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
    else if in_flight == True && (pos_w s0) > floor then
      if control == 7 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, angle = mod_angle (angle s0) (angle_step s1)}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, vel = vel_2, angle = mod_angle (angle s0) (angle_step s1), game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
      else if control == 8 then do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, angle = mod_angle (angle s0) (- (angle_step s1))}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, vel = vel_2, angle = mod_angle (angle s0) (- (angle_step s1)), game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
      else do
        putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate}, w_grid, save_state)
        update_play io_box state_ref ((fourth link0) {pos_u = det !! 0, pos_v = det !! 1, pos_w = (pos_w s0) + ((vel s0) !! 2) / f_rate, vel = vel_2, game_t = game_t'}) (fifth link0) True f_rate (g, f, mag_r, mag_j) (fst_ link0) (snd_ link0) (third link0) look_up conf_reg save_state sound_array
    else do
      putMVar state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor}, w_grid, save_state)
      if (vel s0) !! 2 < -4 then do
        update_play io_box state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, game_t = game_t'}) link1_ False f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array
      else do
        update_play io_box state_ref (s0 {pos_u = det !! 0, pos_v = det !! 1, pos_w = floor, vel = vel_0, game_t = game_t'}) link1 False f_rate (g, f, mag_r, mag_j) w_grid f_grid obj_grid look_up conf_reg save_state sound_array

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

