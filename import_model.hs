module Main where

import System.IO
import System.IO.Unsafe
import System.Environment
import Data.List
import Data.List.Split
import Control.Exception

data Model = Model {vertex :: [[Char]], tex_coord :: [[Char]], normal :: [[Char]], face :: [Int]} deriving Show

data Material = Material {mat_name :: [Char], colour :: [[Char]]} deriving Show

data Loader = Loader {tex_image :: [Char], mod_descrip :: [Char], data_layout :: [Char]} deriving Show

blank_model = Model {vertex = [], tex_coord = [], normal = [], face = []}
blank_loader = Loader {tex_image = [], mod_descrip = [], data_layout = []}

i_list :: [a] -> Int -> a -> a
i_list x i def =
  if i >= length x then def
  else x !! i

init_ :: [a] -> [a]
init_ x = take ((length x) - 2) x

show_ints :: [Int] -> [Char]
show_ints [] = []
show_ints (x:xs) = (show x) ++ ", " ++ show_ints xs

drop_double :: Eq a => [a] -> a -> a -> [a]
drop_double [] t0 t1 = []
drop_double (x:xs) t0 t1 =
  if x == t0 && x == t1 then drop_double xs t0 x
  else x : drop_double xs t0 x

concat_ :: Int -> [[Char]] -> [Char]
concat_ 0 x = (intercalate ", " x)
concat_ 1 x = (intercalate ", " x) ++ ", 1"

-- These two functions are used to generate the indices needed for element drawing
face_index :: Int -> Int -> [Int]
face_index order 0 =
  if order < 4 then [1, 2, 3]
  else if order < 5 then [1, 2, 4, 2, 3, 4]
  else [1, 2, 4, 2, 3, 4] ++ face_index order 3
face_index order n =
  if n > (order - 2) then []
  else [n + 1, n + 2, 1] ++ face_index order (n + 1)

gen_indices :: [[Char]] -> Int -> Int -> Int -> [Char] -> ([Char], Int)
gen_indices [] mode n num_i acc = (acc, num_i)
gen_indices (x:xs) mode n num_i acc =
  let order = ((length (splitOneOf " /" x)) - 1)
      test = i_list (splitOn " " x) 0 "x"
      f_ind0 = face_index (div order 2) 0
      f_ind1 = face_index (div order 3) 0
  in
  if test == "usemtl" then gen_indices xs mode n num_i acc
  else if test == "v" then gen_indices xs mode n num_i acc
  else if test == "vn" then gen_indices xs mode n num_i acc
  else if test == "vt" then gen_indices xs mode n num_i acc
  else if mode == 0 then gen_indices xs mode (n + (div order 2)) (num_i + length f_ind0) (acc ++ show_ints (map (+ (n - 2)) f_ind0))
  else gen_indices xs mode (n + (div order 3)) (num_i + length f_ind1) (acc ++ show_ints (map (+ (n - 2)) f_ind1))

-- These two functions deal with reading vertex attribute data from the OBJ file and transforming this into the form used by the engine for rendering
get_vert :: [[Char]] -> Int -> Int -> Model -> Model
get_vert [] phase mode model = model
get_vert (x:xs) 0 mode model =
  if x == "v" then get_vert (drop 3 xs) 0 mode (model {vertex = (vertex model) ++ take 3 xs})
  else if x == "vt" then get_vert (drop 2 xs) 0 mode (model {tex_coord = (tex_coord model) ++ take 2 xs})
  else if x == "vn" then get_vert (drop 3 xs) 0 mode (model {normal = (normal model) ++ take 3 xs})
  else if x == "usemtl" then get_vert (drop 1 xs) 0 mode model
  else get_vert xs 1 mode model
get_vert (x:xs) 1 mode model =
  if x == "usemtl" then get_vert (drop 1 xs) 1 mode model
  else if x == "f" then get_vert xs 1 mode model
  else get_vert xs 1 mode (model {face = (face model) ++ [read x]})

get_colour :: [[Char]] -> [Material]
get_colour [] = []
get_colour (x0:x1:x2:x3:xs) = Material {mat_name = x0, colour = [x1, x2, x3]} : get_colour xs

check_attrib :: [[Char]] -> Int -> [[Char]]
check_attrib [] i = ["e"] ++ [show i]
check_attrib attrib i = attrib

place_vert :: Model -> Int -> [Char] -> [Char] -> [Char] -> [Char] -> Int -> ([Char], Int)
place_vert model mode vert_acc tex_acc norm_acc col_attrib c =
  let face_vert = ((i_list (face model) 0 0) * 3) - 3
      face_tex = ((i_list (face model) 1 0) * 2) - 2
      face_norm = ((i_list (face model) 2 0) * 3) - 3
      face_norm' = ((i_list (face model) 1 0) * 3) - 3
  in
  if face model == [] then (init_ (vert_acc ++ col_attrib ++ tex_acc ++ norm_acc), c)
  else if mode == 0 then place_vert (model {face = drop 2 (face model)}) mode (vert_acc ++ concat_ 1 (check_attrib (take 3 (drop face_vert (vertex model))) face_vert) ++ ", ") tex_acc (norm_acc ++ concat_ 0 (check_attrib (take 3 (drop face_norm' (normal model))) face_norm') ++ ", ") col_attrib (c + 1)
  else place_vert (model {face = drop 3 (face model)}) mode (vert_acc ++ concat_ 1 (check_attrib (take 3 (drop face_vert (vertex model))) face_vert) ++ ", ") (tex_acc ++ concat_ 0 (check_attrib (take 2 (drop face_tex (tex_coord model))) face_tex) ++ ", ") (norm_acc ++ concat_ 0 (check_attrib (take 3 (drop face_norm (normal model))) face_norm) ++ ", ") col_attrib (c + 1)

add_colour1 :: [Material] -> [Char] -> [Char]
add_colour1 [] query = []
add_colour1 (x:xs) query =
  if mat_name x == query then concat_ 0 (colour x)
  else add_colour1 xs query

add_colour0 :: [[Char]] -> [Material] -> [Char] -> [Char]
add_colour0 [] mat col = []
add_colour0 (x:xs) mat col =
  if x == "usemtl" then add_colour0 (drop 1 xs) mat (add_colour1 mat (i_list xs 0 "x"))
  else if x == "v" then add_colour0 (drop 3 xs) mat col
  else if x == "vt" then add_colour0 (drop 2 xs) mat col
  else if x == "vn" then add_colour0 (drop 3 xs) mat col
  else if x == "f" then add_colour0 xs mat col
  else col ++ ", " ++ add_colour0 xs mat col

num_elem :: Int -> Int -> Int
num_elem 0 c = 11 * c
num_elem 1 c = 9 * c

num_ind :: Int -> Int
num_ind f =
  if f < 4 then 3
  else 3 + (f - 3) * 3

det_mode :: Int -> Int
det_mode num_tex =
  if num_tex > 0 then 1
  else 0

main = do
  args <- getArgs
  h0 <- openFile (args !! 0) ReadMode
  h1 <- openFile (args !! 1) ReadMode
  h2 <- openFile (args !! 2) WriteMode
  contents0 <- hGetContents h0
  contents1 <- hGetContents h1
  unroll_data (splitOn "\n~\n" contents0) (splitOn "\n~\n" contents1) h2 blank_loader [] [] 0 0 0
  hClose h0
  hClose h1
  hClose h2

unroll_data :: [[Char]] -> [[Char]] -> Handle -> Loader -> [Char] -> [Char] -> Int -> Int -> Int -> IO ()
unroll_data [] _ h2 load_acc vert_acc ind_acc e_count i_count mat_count = hPutStr h2 (init_ (tex_image load_acc) ++ "~" ++ init_ (mod_descrip load_acc) ++ "~" ++ init_ (data_layout load_acc) ++ "~" ++ init_ vert_acc ++ "~" ++ init_ ind_acc)
unroll_data (x0:x1:xs) mat h2 load_acc vert_acc ind_acc e_count i_count mat_count =
  let mode = det_mode (read (i_list (splitOn ", " x0) 1 "x"))
      col_attrib = add_colour0 (splitOneOf "\n /" (drop_double x1 '/' 'x')) (get_colour (splitOneOf "\n " (i_list mat mat_count "x"))) []
      pl_vert0 = place_vert (get_vert (splitOneOf "\n /" (drop_double x1 '/' 'x')) 0 mode blank_model) mode [] [] [] col_attrib 0
      pl_vert1 = place_vert (get_vert (splitOneOf "\n /" (drop_double x1 '/' 'x')) 0 mode blank_model) mode [] [] [] [] 0
      vertices0 = fst pl_vert0
      vertices1 = fst pl_vert1
      indices = gen_indices (splitOn "\n" (drop_double x1 '/' 'x')) mode 1 0 []
      m_data = (splitOn ", " x0)
      l_data0 = load_acc {mod_descrip = mod_descrip load_acc ++ (i_list m_data 0 "x") ++ ", " ++ show (snd pl_vert0) ++ ", " ++ concat_ 0 (drop 1 m_data) ++ "&", data_layout = data_layout load_acc ++ show_ints [e_count, num_elem 0 (snd pl_vert0), i_count, snd indices]}
      l_data1 = load_acc {tex_image = tex_image load_acc ++ (concat_ 0 (drop 5 (splitOn ", " x0))) ++ ", ", mod_descrip = mod_descrip load_acc ++ (i_list m_data 0 "x") ++ ", " ++ show (snd pl_vert0) ++ ", " ++ (concat_ 0 (take 4 (drop 1 m_data))) ++ "&", data_layout = data_layout load_acc ++ show_ints [e_count, num_elem 1 (snd pl_vert1), i_count, snd indices]}
  in do
  if mode == 0 then unroll_data xs mat h2 l_data0 (vert_acc ++ fst pl_vert0 ++ ", ") (ind_acc ++ fst indices) (e_count + num_elem 0 (snd pl_vert0)) (i_count + snd indices) (mat_count + 1)
  else unroll_data xs mat h2 l_data1 (vert_acc ++ fst pl_vert1 ++ ", ") (ind_acc ++ fst indices) (e_count + num_elem 1 (snd pl_vert1)) (i_count + snd indices) mat_count

