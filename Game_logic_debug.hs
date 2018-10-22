module Game_logic_debug

-- chg_state modified for the Heavy Debugging Output Variant
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

-- run_gplc modified for the Heavy Debugging Output Variant
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

-- link_gplc0 modified for the Heavy Debugging Output Variant
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

-- update_play modified for the Heavy Debugging Output Variant
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
