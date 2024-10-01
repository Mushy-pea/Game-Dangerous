module SaveGame where

import Prelude hiding ((!!))
import IndexWrapper0
import System.IO
import Control.Exception
import Data.Array.IArray
import Data.Binary hiding (get)
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence as SEQ
import qualified Data.Foldable as FOLD
import Data.List.Split
import BuildModel
import GameLogic

-- Constants used to fix the types decoded from save game files.
def_obj_place_ = ((0, 0, 0), Nothing) :: ((Int, Int, Int), Maybe Obj_place)
def_f_grid_ = ((0, 0, 0), def_f_grid) :: ((Int, Int, Int), Floor_grid)
def_obj_grid_ = ((0, 0, 0), def_obj_grid) :: ((Int, Int, Int), Obj_grid)

-- These three functions deal with loading a saved game state and recreating the game state by updating a base map state.
decodeSequence :: Binary a => Int -> a -> LBS.ByteString -> SEQ.Seq a -> SEQ.Seq a
decodeSequence c def bs diff_seq =
  let result = decodeOrFail bs
      tested_result = if (isLeft result) == True then error ("\ndecode_sequence: " ++ third_ (fromLeft (LBS.empty, 0, "def") result) ++ "\noffset: " ++ show c)
                      else fromRight (LBS.empty, 0, def) result
  in
  if LBS.length bs == 0 then diff_seq
  else decodeSequence (c + 1) def (fst__ tested_result) (diff_seq SEQ.>< SEQ.singleton (third_ tested_result))

procWGridUpd :: [((Int, Int, Int), Maybe Obj_place)] -> Array (Int, Int, Int) Wall_grid -> Int -> Int -> [((Int, Int, Int), Wall_grid)]
procWGridUpd [] w_grid u_offset v_offset = []
procWGridUpd (((w, u, v), obj_):xs) w_grid u_offset v_offset =
  ((w, u, v), (w_grid ! (w, u + u_offset, v + v_offset)) {obj = obj_}) : procWGridUpd xs w_grid u_offset v_offset

loadGameStateFile1 :: Int -> LBS.ByteString
                     -> SEQ.Seq ((Int, Int, Int), Maybe Obj_place) -> SEQ.Seq ((Int, Int, Int), Floor_grid) -> SEQ.Seq ((Int, Int, Int), Obj_grid)
                     -> Play_state0 -> Play_state1
                     -> ([((Int, Int, Int), Maybe Obj_place)], [((Int, Int, Int), Floor_grid)], [((Int, Int, Int), Obj_grid)], Play_state0, Play_state1)
loadGameStateFile1 c bs w_grid_upd f_grid_upd obj_grid_upd s0 s1 =
  if c == 0 then loadGameStateFile1 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs)
                                    (decodeSequence 0 def_obj_place_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
                                    f_grid_upd obj_grid_upd s0 s1
  else if c == 1 then loadGameStateFile1 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid_upd
                                         (decodeSequence 0 def_f_grid_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
                                         obj_grid_upd s0 s1
  else if c == 2 then loadGameStateFile1 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid_upd f_grid_upd
                                         (decodeSequence 0 def_obj_grid_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
                                         s0 s1
  else if c == 3 then loadGameStateFile1 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid_upd f_grid_upd
                                        obj_grid_upd ((decode (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs))) :: Play_state0) s1
  else if c == 4 then loadGameStateFile1 (c + 1) LBS.empty w_grid_upd f_grid_upd obj_grid_upd s0
                                         ((decode (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs))) :: Play_state1)
  else (FOLD.toList w_grid_upd, FOLD.toList f_grid_upd, FOLD.toList obj_grid_upd, s0, s1)

loadGameStateFile2 :: [Char] -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) Obj_grid
                      -> WGridDiffContainer -> FGridDiffContainer -> ObjGridDiffContainer -> Play_state0 -> Play_state1 -> (Int, Int) -> Game_state
loadGameStateFile2 map_file w_grid f_grid obj_grid w_grid_upd f_grid_upd obj_grid_upd s0 s1 (u_offset, v_offset)
  | map_file == "map1.dan" = Game_state {event_context = None,
                                         w_grid_ = w_grid // applyLoadTransform (procWGridUpd (map1WDiff w_grid_upd) w_grid u_offset v_offset) (u_offset, v_offset),
                                         f_grid_ = f_grid // applyLoadTransform (map1FDiff f_grid_upd) (u_offset, v_offset),
                                         obj_grid_ = obj_grid // applyLoadTransform (map1ODiff obj_grid_upd) (u_offset, v_offset),
                                         s0_ = s0, s1_ = s1, save_file = LBS.empty}
  | map_file == "map2.dan" = Game_state {event_context = None,
                                         w_grid_ = w_grid // applyLoadTransform (procWGridUpd (map2WDiff w_grid_upd) w_grid u_offset v_offset) (u_offset, v_offset),
                                         f_grid_ = f_grid // applyLoadTransform (map2FDiff f_grid_upd) (u_offset, v_offset),
                                         obj_grid_ = obj_grid // applyLoadTransform (map2ODiff obj_grid_upd) (u_offset, v_offset),
                                         s0_ = s0, s1_ = s1, save_file = LBS.empty}
  | map_file == "map3.dan" = Game_state {event_context = None,
                                         w_grid_ = w_grid // applyLoadTransform (procWGridUpd (map3WDiff w_grid_upd) w_grid u_offset v_offset) (u_offset, v_offset),
                                         f_grid_ = f_grid // applyLoadTransform (map3FDiff f_grid_upd) (u_offset, v_offset),
                                         obj_grid_ = obj_grid // applyLoadTransform (map3ODiff obj_grid_upd) (u_offset, v_offset),
                                         s0_ = s0, s1_ = s1, save_file = LBS.empty}
  | map_file == "map4.dan" = Game_state {event_context = None,
                                         w_grid_ = w_grid // applyLoadTransform (procWGridUpd (map4WDiff w_grid_upd) w_grid u_offset v_offset) (u_offset, v_offset),
                                         f_grid_ = f_grid // applyLoadTransform (map4FDiff f_grid_upd) (u_offset, v_offset),
                                         obj_grid_ = obj_grid // applyLoadTransform (map4ODiff obj_grid_upd) (u_offset, v_offset),
                                         s0_ = s0, s1_ = s1, save_file = LBS.empty}

loadGameStateFile3 :: LBS.ByteString -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) Obj_grid
                      -> [Char] -> (Int, Int) -> Game_state
loadGameStateFile3 bs w_grid f_grid obj_grid map_file (u_offset, v_offset) =
  let loaded_diffs = loadGameStateFile1 0 bs SEQ.Empty SEQ.Empty SEQ.Empty ps0_init ps1_init
      w_grid_diffs = filterWGridDiffs (fst_ loaded_diffs) emptyWGridDiffContainer
      f_grid_diffs = filterFGridDiffs (snd_ loaded_diffs) emptyFGridDiffContainer
      obj_grid_diffs = filterObjGridDiffs (third loaded_diffs) emptyObjGridDiffContainer
      s0 = fourth loaded_diffs
      s1 = fifth loaded_diffs
  in
  loadGameStateFile2 map_file w_grid f_grid obj_grid w_grid_diffs f_grid_diffs obj_grid_diffs s0 s1 (u_offset, v_offset)

-- If an error occurs while attempting to open a save game file the user is informed through the menu system.
loaderError :: SomeException -> Io_box -> IO LBS.ByteString
loaderError x box = do
  runMenu 0 error_opening_file_text [] box (-0.75) (-0.75) 1 0 0 ps0_init 1
  putStr ("\nload_saved_game: " ++ show x)
  return LBS.empty

-- This function is the entry point to the game state saving logic and handles user input from the load game menu.
loadSavedGame :: Int -> [[Char]] -> [Char] -> Int -> Int -> Io_box -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
                 -> Array (Int, Int, Int) Obj_grid -> Array Int [Char] -> (Int, Int) -> [Char] -> IO (Maybe Game_state)
loadSavedGame 0 [] chosen_file c choice box w_grid f_grid obj_grid conf_reg load_offset map_file = error "\nload_saved_game: encountered an unexpected log file structure."
loadSavedGame 0 ((y0:y1:y2:y3:y4:y5:y6:y7:y8:y9:y10:y11:y12:y13:y14:y15:y16:ys):xs) chosen_file c choice box w_grid f_grid obj_grid conf_reg load_offset
              map_file = do
  if choice == 7 then return Nothing
  else if c == choice then loadSavedGame 1 [] [y1, y2, y3, y4, y5, y6, y7, y8, y9] c choice box w_grid f_grid obj_grid conf_reg load_offset map_file
  else loadSavedGame 0 xs chosen_file (c + 1) choice box w_grid f_grid obj_grid conf_reg load_offset map_file
loadSavedGame 1 [] chosen_file c choice box w_grid f_grid obj_grid conf_reg load_offset map_file = do
  contents <- catch (do contents <- LBS.readFile ((cfg conf_reg 0 "game_save_path") ++ chosen_file); return contents) (\e -> loaderError e box)
  if LBS.length contents == 0 then return Nothing
  else return (Just (loadGameStateFile3 contents w_grid f_grid obj_grid map_file load_offset))

-- Sequential saves of the same game produce a sequence of save game files up to a preset maximum.
-- The automation of this feature is done in the two functions below.
addTimeStamp :: [[Char]] -> Play_state0 -> Int -> Int -> [Char]
addTimeStamp [] s0 save_slot i = []
addTimeStamp ((y0:y1:y2:y3:y4:y5:y6:y7:y8:y9:y10:y11:y12:y13:y14:y15:y16:ys):xs) s0 save_slot i =
  let file_name = ['_', y1, y2, y3, y4, y5, y6, y7, y8, y9, y10]
  in
  if i == save_slot then file_name ++ showGameTime (mod (fst__ (gameClock s0)) 1440000) [] False ++ "\n" ++ addTimeStamp xs s0 save_slot (i + 1)
  else [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16] ++ "\n" ++ addTimeStamp xs s0 save_slot (i + 1)

selectSaveFile :: [[Char]] -> Play_state0 -> Int -> ([Char], [Char])
selectSaveFile file_list s0 limit =
  let i = read ((file_list, 631) !! 0)
  in
  if i == limit then (take 9 (tail ((file_list, 632) !! limit)), "1\n" ++ init (addTimeStamp (tail file_list) s0 i 1))
  else (take 9 (tail ((file_list, 633) !! i)), show (i + 1) ++ "\n" ++ init (addTimeStamp (tail file_list) s0 i 1))

-- This class and the four other functions below deal with generating a save game file.
-- The Serialise_diff class is used so that the Obj_place type gets extracted from Wall_grid.  This is done because the rest of the Wall_grid structure is
-- not exposed to the GPLC interpreter and so cannot change during game play.
class Serialise_diff a where
  save_diff :: ((Int, Int, Int), a) -> LBS.ByteString

instance Serialise_diff Wall_grid where
  save_diff ((w, u, v), x) = LBS.append (encode (w, u, v)) (encode (obj x))

instance Serialise_diff Floor_grid where
  save_diff ((w, u, v), x) = LBS.append (encode (w, u, v)) (encode x)

instance Serialise_diff Obj_grid where
  save_diff ((w, u, v), x) = LBS.append (encode (w, u, v)) (encode x)

saveArrayDiff0 :: Int -> ([Char], [Char]) -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> Array Int [Char]
                  -> Play_state0 -> IO Int
saveArrayDiff0 mode (save_file, save_log) w_grid_bstring f_grid_bstring obj_grid_bstring s0_bstring s1_bstring conf_reg s0 =
  let block0 = LBS.append (LBS.drop 8 w_grid_bstring)
      block1 = LBS.append (LBS.drop 8 f_grid_bstring)
      block2 = LBS.append (LBS.drop 8 obj_grid_bstring)
      block3 = LBS.append s0_bstring
  in
  if mode == 0 then do
    contents <- bracket (openFile "save_log.log" ReadMode) (hClose) (\h -> do c <- hGetContents h; putStr ("\nsave_log size: " ++ show (length c)); return c)
    saveArrayDiff0 1 (selectSaveFile (splitOn "\n" (tailFile contents)) s0 ((length (splitOn "\n" (tailFile contents))) - 1)) w_grid_bstring f_grid_bstring
                   obj_grid_bstring s0_bstring s1_bstring conf_reg s0
  else do
    h0 <- openFile "save_log.log" WriteMode
    hPutStr h0 save_log
    hClose h0
    LBS.writeFile (cfg conf_reg 0 "game_save_path" ++ save_file) (block0 $ block1 $ block2 $ block3 $ s1_bstring)
    putStr ("\n\nGame saved as: " ++ (cfg conf_reg 0 "game_save_path") ++ save_file)
    return ((read (take 1 (drop 4 save_file))) + 1)

wrappedSaveArrayDiff1 :: Serialise_diff a => SEQ.Seq ((Int, Int, Int), a) -> LBS.ByteString
wrappedSaveArrayDiff1 x =
  if SEQ.length x == 0 then LBS.append (encode (0 :: Int)) (encode (0 :: Int))
  else saveArrayDiff1 x LBS.empty 0 ((SEQ.length x) - 1)

saveArrayDiff1 :: Serialise_diff a => SEQ.Seq ((Int, Int, Int), a) -> LBS.ByteString -> Int -> Int -> LBS.ByteString
saveArrayDiff1 diff_seq diff_bytestring i limit =
  if i > limit then LBS.append (encode i) (LBS.append (encode (fromIntegral (LBS.length diff_bytestring) :: Int)) diff_bytestring)
  else saveArrayDiff1 diff_seq (LBS.append diff_bytestring (save_diff (SEQ.index diff_seq i))) (i + 1) limit

labelPlayStateEncoding :: LBS.ByteString -> LBS.ByteString
labelPlayStateEncoding x = LBS.append (encode (fromIntegral (LBS.length x) :: Int)) x

detMapTransform :: [Char] -> [Char] -> Int -> Int -> (Int, Int)
detMapTransform map operation u_max v_max
  | map == "map1.dan" && operation == "save" = (0, 0)
  | map == "map1.dan" && operation == "load" = (0, 0)
  | map == "map2.dan" && operation == "save" = (0, - (v_max + 1))
  | map == "map2.dan" && operation == "load" = (0, v_max + 1)
  | map == "map3.dan" && operation == "save" = ( - (u_max + 1), - (v_max + 1))
  | map == "map3.dan" && operation == "load" = (u_max + 1, v_max + 1)
  | map == "map4.dan" && operation == "save" = (- (u_max + 1), 0)
  | map == "map4.dan" && operation == "load" = (u_max + 1, 0)

applyLoadTransform :: [((Int, Int, Int), a)] -> (Int, Int) -> [((Int, Int, Int), a)]
applyLoadTransform [] (u_offset, v_offset) = []
applyLoadTransform (((w, u, v), y):xs) (u_offset, v_offset) = ((w, u + u_offset, v + v_offset), y) : applyLoadTransform xs (u_offset, v_offset)

filterWGridDiffs :: [((Int, Int, Int), Maybe Obj_place)] -> WGridDiffContainer -> WGridDiffContainer
filterWGridDiffs [] diff_container = diff_container
filterWGridDiffs (((w, u, v), y):xs) diff_container
  | u >= 0 && v >= 0 = filterWGridDiffs xs (diff_container {map1WDiff = ((w, u, v), y) : map1WDiff diff_container})
  | u >= 0 && v < 0 = filterWGridDiffs xs (diff_container {map2WDiff = ((w, u, v), y) : map2WDiff diff_container})
  | u < 0 && v < 0 = filterWGridDiffs xs (diff_container {map3WDiff = ((w, u, v), y) : map3WDiff diff_container})
  | otherwise = filterWGridDiffs xs (diff_container {map4WDiff = ((w, u, v), y) : map4WDiff diff_container})

filterFGridDiffs :: [((Int, Int, Int), Floor_grid)] -> FGridDiffContainer -> FGridDiffContainer
filterFGridDiffs [] diff_container = diff_container
filterFGridDiffs (((w, u, v), y):xs) diff_container
  | u >= 0 && v >= 0 = filterFGridDiffs xs (diff_container {map1FDiff = ((w, u, v), y) : map1FDiff diff_container})
  | u >= 0 && v < 0 = filterFGridDiffs xs (diff_container {map2FDiff = ((w, u, v), y) : map2FDiff diff_container})
  | u < 0 && v < 0 = filterFGridDiffs xs (diff_container {map3FDiff = ((w, u, v), y) : map3FDiff diff_container})
  | otherwise = filterFGridDiffs xs (diff_container {map4FDiff = ((w, u, v), y) : map4FDiff diff_container})

filterObjGridDiffs :: [((Int, Int, Int), Obj_grid)] -> ObjGridDiffContainer -> ObjGridDiffContainer
filterObjGridDiffs [] diff_container = diff_container
filterObjGridDiffs (((w, u, v), y):xs) diff_container
  | u >= 0 && v >= 0 = filterObjGridDiffs xs (diff_container {map1ODiff = ((w, u, v), y) : map1ODiff diff_container})
  | u >= 0 && v < 0 = filterObjGridDiffs xs (diff_container {map2ODiff = ((w, u, v), y) : map1ODiff diff_container})
  | u < 0 && v < 0 = filterObjGridDiffs xs (diff_container {map3ODiff = ((w, u, v), y) : map3ODiff diff_container})
  | otherwise = filterObjGridDiffs xs (diff_container {map4ODiff = ((w, u, v), y) : map4ODiff diff_container})

