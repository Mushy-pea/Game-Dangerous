module ExtraGPLC_Debug where

gplcSymbol :: [Char] -> Int -> [Char]
gplcSymbol "PlayerGun:37556ae25af6361bffa584624950e37e5b2520f2dd4887c584f55b6535601ad2" d_list_index
  | d_list_index == 0 = "w: "
  | d_list_index == 1 = "u: "
  | d_list_index == 2 = "v: "
  | d_list_index == 3 = "v_: "
  | d_list_index == 4 = "sig2: "
  | d_list_index == 5 = "sig3: "
  | d_list_index == 6 = "sig4: "
  | d_list_index == 7 = "pos_u: "
  | d_list_index == 8 = "pos_v: "
  | d_list_index == 9 = "pos_w: "
  | d_list_index == 10 = "vel_u: "
  | d_list_index == 11 = "vel_v: "
  | d_list_index == 12 = "vel_w: "
  | d_list_index == 13 = "angle: "
  | d_list_index == 14 = "rend_mode: "
  | d_list_index == 15 = "game_t: "
  | d_list_index == 16 = "torch_t0: "
  | d_list_index == 17 = "torch_t_limit: "
  | d_list_index == 18 = "health: "
  | d_list_index == 19 = "ammo: "
  | d_list_index == 20 = "gems: "
  | d_list_index == 21 = "torches: "
  | d_list_index == 22 = "key0: "
  | d_list_index == 23 = "key1: "
  | d_list_index == 24 = "key2: "
  | d_list_index == 25 = "key3: "
  | d_list_index == 26 = "key4: "
  | d_list_index == 27 = "key5: "
  | d_list_index == 28 = "zero: "
  | d_list_index == 29 = "speed: "
  | d_list_index == 30 = "copy: "
  | d_list_index == 31 = "v_limit: "
  | d_list_index == 32 = "msg_length0: "
  | d_list_index == 33 = "msg_length1: "
  | d_list_index == 34 = "game_t': "
  | d_list_index == 35 = "obj_flag: "
  | d_list_index == 36 = "obj_flag_base: "
  | d_list_index == 37 = "w_: "
  | d_list_index == 38 = "v__: "
gplcSymbol program_name d_list_index = show d_list_index ++ ": "

recoverGplcSymbols :: [Int] -> [Char] -> Int -> [Char]
recoverGplcSymbols [] prog_name i = []
recoverGplcSymbols (x:xs) prog_name i = gplcSymbol prog_name i ++ show x ++ " " ++ recoverGplcSymbols xs prog_name (i + 1)

