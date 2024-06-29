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

procWGridUpd :: SEQ.Seq ((Int, Int, Int), Maybe Obj_place) -> Array (Int, Int, Int) Wall_grid -> [((Int, Int, Int), Wall_grid)]
procWGridUpd SEQ.Empty w_grid = []
procWGridUpd (x SEQ.:<| xs) w_grid = (fst x, (w_grid ! (fst x)) {obj = snd x}) : procWGridUpd xs w_grid

applyLoadTransform :: [((Int, Int, Int), a)] -> (Int, Int) -> [((Int, Int, Int), a)]
applyLoadTransform [] (u_offset, v_offset) = []
applyLoadTransform (((w, u, v), y):xs) (u_offset, v_offset) = ((w, u + u_offset, v + v_offset), y) : applyLoadTransform xs (u_offset, v_offset)

loadGameStateFile1 :: Int -> LBS.ByteString -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid -> Array (Int, Int, Int) Obj_grid
                     -> SEQ.Seq ((Int, Int, Int), Maybe Obj_place) -> SEQ.Seq ((Int, Int, Int), Floor_grid) -> SEQ.Seq ((Int, Int, Int), Obj_grid)
                     -> Play_state0 -> Play_state1 -> (Int, Int) -> Game_state
loadGameStateFile1 c bs w_grid f_grid obj_grid w_grid_upd f_grid_upd obj_grid_upd s0 s1 load_offset =
  if c == 0 then loadGameStateFile1 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid
                                   (decodeSequence 0 def_obj_place_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
                                   f_grid_upd obj_grid_upd s0 s1 load_offset
  else if c == 1 then loadGameStateFile1 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid w_grid_upd
                                        (decodeSequence 0 def_f_grid_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
                                        obj_grid_upd  s0 s1 load_offset
  else if c == 2 then loadGameStateFile1 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid w_grid_upd f_grid_upd
                                        (decodeSequence 0 def_obj_grid_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
                                        s0 s1 load_offset
  else if c == 3 then loadGameStateFile1 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid f_grid obj_grid w_grid_upd f_grid_upd
                                        obj_grid_upd ((decode (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs))) :: Play_state0) s1
                                        load_offset
  else if c == 4 then loadGameStateFile1 (c + 1) LBS.empty w_grid f_grid obj_grid w_grid_upd f_grid_upd obj_grid_upd s0
                                        ((decode (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs))) :: Play_state1) load_offset
  else Game_state {is_set = False,
                   w_grid_ = w_grid // applyLoadTransform (procWGridUpd w_grid_upd w_grid) load_offset,
                   f_grid_ = f_grid // applyLoadTransform (FOLD.toList f_grid_upd) load_offset,
                   obj_grid_ = obj_grid // applyLoadTransform (FOLD.toList obj_grid_upd) load_offset, s0_ = s0, s1_ = s1}

loadGameStateFile2 :: Int -> LBS.ByteString -> Array (Int, Int, Int) Wall_grid -> SEQ.Seq ((Int, Int, Int), Maybe Obj_place)
                      -> SEQ.Seq ((Int, Int, Int), Floor_grid)
                      -> SEQ.Seq ((Int, Int, Int), Obj_grid)
                      -> ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), Floor_grid)], [((Int, Int, Int), Obj_grid)])
loadGameStateFile2 c bs w_grid w_grid_upd f_grid_upd obj_grid_upd =
  if c == 0 then loadGameStateFile2 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid
                                   (decodeSequence 0 def_obj_place_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
                                   f_grid_upd obj_grid_upd
  else if c == 1 then loadGameStateFile2 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid w_grid_upd
                                        (decodeSequence 0 def_f_grid_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
                                        obj_grid_upd
  else if c == 2 then loadGameStateFile2 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid w_grid_upd f_grid_upd
                                        (decodeSequence 0 def_obj_grid_ (LBS.take (fromIntegral ((decode (LBS.take 8 bs)) :: Int)) (LBS.drop 8 bs)) SEQ.empty)
  else if c == 3 then loadGameStateFile2 (c + 1) (LBS.drop (8 + fromIntegral ((decode (LBS.take 8 bs)) :: Int)) bs) w_grid w_grid_upd f_grid_upd
                                        obj_grid_upd
  else if c == 4 then loadGameStateFile2 (c + 1) LBS.empty w_grid w_grid_upd f_grid_upd obj_grid_upd
  else (procWGridUpd w_grid_upd w_grid, FOLD.toList f_grid_upd, FOLD.toList obj_grid_upd)

-- If an error occurs while attempting to open a save game file the user is informed through the menu system.
loaderError :: SomeException -> Io_box -> IO LBS.ByteString
loaderError x box = do
  runMenu error_opening_file_text [] box (-0.75) (-0.75) 1 0 0 ps0_init 1
  putStr ("\nload_saved_game: " ++ show x)
  return LBS.empty

viewSavedGame :: [Char] -> Array (Int, Int, Int) Wall_grid -> IO ([((Int, Int, Int), Wall_grid)], [((Int, Int, Int), Floor_grid)], [((Int, Int, Int), Obj_grid)])
viewSavedGame chosen_file w_grid = do
  contents <- LBS.readFile chosen_file
  return (loadGameStateFile2 0 contents w_grid SEQ.Empty SEQ.Empty SEQ.Empty)

showSavedGame :: Show a => [a] -> IO ()
showSavedGame [] = return ()
showSavedGame (x:xs) = do
  putStr ("\n" ++ show x)
  showSavedGame xs

-- showSavedGame :: Show a => [((Int, Int, Int), a)] -> [[Char]]
-- showSavedGame [] = []
-- showSavedGame (x:xs) = (show x) : showSavedGame xs

-- displaySavedGame :: [[Char]] -> IO ()
-- displaySavedGame [] = return ()
-- displaySavedGame (x:xs) = do
--   putStr ("\n" ++ show x)
--   displaySavedGame xs

-- This function is the entry point to the game state saving logic and handles user input from the load game menu.
loadSavedGame :: Int -> [[Char]] -> [Char] -> Int -> Int -> Io_box -> Array (Int, Int, Int) Wall_grid -> Array (Int, Int, Int) Floor_grid
                 -> Array (Int, Int, Int) Obj_grid -> Array Int [Char] -> (Int, Int) -> IO (Maybe Game_state)
loadSavedGame 0 [] chosen_file c choice box w_grid f_grid obj_grid conf_reg load_offset = error "\nload_saved_game: encountered an unexpected log file structure."
loadSavedGame 0 ((y0:y1:y2:y3:y4:y5:y6:y7:y8:y9:y10:y11:y12:y13:y14:y15:y16:ys):xs) chosen_file c choice box w_grid f_grid obj_grid conf_reg load_offset = do
  if choice == 7 then return Nothing
  else if c == choice then loadSavedGame 1 [] [y1, y2, y3, y4, y5, y6, y7, y8, y9] c choice box w_grid f_grid obj_grid conf_reg load_offset
  else loadSavedGame 0 xs chosen_file (c + 1) choice box w_grid f_grid obj_grid conf_reg load_offset
loadSavedGame 1 [] chosen_file c choice box w_grid f_grid obj_grid conf_reg load_offset = do
  contents <- catch (do contents <- LBS.readFile ((cfg conf_reg 0 "game_save_path") ++ chosen_file); return contents) (\e -> loaderError e box)
  if LBS.length contents == 0 then return Nothing
  else return (Just (loadGameStateFile1 0 contents w_grid f_grid obj_grid SEQ.Empty SEQ.Empty SEQ.Empty ps0_init ps1_init load_offset))

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
                  -> Play_state0 -> IO ()
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

