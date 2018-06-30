-- This is a development tool that transforms a modified form of the OBJ file format into the model import file format used by the engine.

module Main where

import System.IO
import System.IO.Unsafe
import System.Environment
import Data.List
import Data.List.Split
import Data.Matrix hiding ((!))
import Data.Array.IArray
import Data.Array.Unboxed
import Control.Exception

sub_i :: Int -> [a] -> Int -> a
sub_i location x y =
  if y >= length x then error ("\nList index error.  max index: " ++ show ((length x) - 1) ++ " index: " ++ show y ++ " location: " ++ show location)
  else x !! y

data Model = Model {vertex :: [[Char]], tex_coord :: [[Char]], normal :: [[Char]], face :: [Int]} deriving Show

data Material = Material {mat_name :: [Char], colour :: [[Char]]} deriving Show

data Loader = Loader {tex_image :: [Char], mod_descrip :: [Char], data_layout :: [Char]} deriving Show

blank_model = Model {vertex = [], tex_coord = [], normal = [], face = []}
blank_loader = Loader {tex_image = [], mod_descrip = [], data_layout = []}

i_list :: [a] -> Int -> a -> a
i_list x i def =
  if i >= length x then def
  else (sub_i 0 x i)

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

-- These functions are used to optionally apply rotations to a model before the data is unrolled.
make_table :: Int -> Float -> [Float]
make_table 0 a =
  if a > 6.29 then []
  else a : make_table 0 (a + 0.01)
make_table 1 a =
  if a > 6.29 then []
  else sin a : make_table 1 (a + 0.01)
make_table 2 a =
  if a > 6.29 then []
  else cos a : make_table 2 (a + 0.01)
make_table 3 a =
  if a > 6.29 then []
  else tan a : make_table 3 (a + 0.01)

look_up :: [[Float]] -> UArray (Int, Int) Float
look_up a = array ((0, 0), (3, 628)) [((x, y), realToFrac ((a !! x) !! y)) | x <- [0..3], y <- [0..628]]

rotation_w :: Int -> UArray (Int, Int) Float -> Matrix Float
rotation_w a look_up = fromList 4 4 [look_up ! (2, a), - look_up ! (1, a), 0, 0, look_up ! (1, a), look_up ! (2, a), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]

transform :: Floating a => [Matrix a] -> Matrix a -> [Matrix a]
transform [] mat = []
transform (v:vs) mat = (multStd mat v) : transform vs mat

conv_to_vector :: Int -> Model -> [Matrix Float] -> [Matrix Float] -> ([Matrix Float], [Matrix Float])
conv_to_vector 0 model vert_acc norm_acc =
  if vertex model == [] then conv_to_vector 1 model vert_acc norm_acc
  else conv_to_vector 0 (model {vertex = drop 3 (vertex model)}) (vert_acc ++ [fromList 4 1 [read ((sub_i 3 (vertex model) 0)), read ((sub_i 4 (vertex model) 1)), read ((sub_i 5 (vertex model) 2)), 1]]) norm_acc
conv_to_vector 1 model vert_acc norm_acc =
  if normal model == [] then (vert_acc, norm_acc)
  else conv_to_vector 1 (model {normal = drop 3 (normal model)}) vert_acc (norm_acc ++ [fromList 4 1 [read ((sub_i 6 (normal model) 0)), read ((sub_i 7 (normal model) 1)), read ((sub_i 8 (normal model) 2)), 1]])

conv_to_model :: [Matrix Float] -> [[Char]]
conv_to_model [] = []
conv_to_model (x:xs) =
  let ls = toList x
  in [show ((sub_i 9 ls 0)), show ((sub_i 10 ls 1)), show ((sub_i 11 ls 2))] ++ conv_to_model xs

rotate_model :: Model -> Int -> UArray (Int, Int) Float -> [Model]
rotate_model model a look_up =
  let vector_form = conv_to_vector 0 model [] []
  in
  if a > 553 then []
  else model {vertex = conv_to_model (transform (fst vector_form) (rotation_w a look_up)), normal = conv_to_model (transform (snd vector_form) (rotation_w a look_up))} : rotate_model model (a + 79) look_up

model_to_obj2 :: [Int] -> [Char]
model_to_obj2 [] = "\n"
model_to_obj2 (x0:x1:x2:xs) = " " ++ show x0 ++ "/" ++ show x1 ++ "/" ++ show x2 ++ model_to_obj2 xs

model_to_obj1 :: Int -> Int -> Model -> [Char]
model_to_obj1 0 face_order model =
  if vertex model == [] then model_to_obj1 1 face_order model
  else "v " ++ ((sub_i 13 (vertex model) 0)) ++ " " ++ ((sub_i 14 (vertex model) 1)) ++ " " ++ ((sub_i 15 (vertex model) 2)) ++ "\n" ++ model_to_obj1 0 face_order (model {vertex = drop 3 (vertex model)})
model_to_obj1 1 face_order model =
  if tex_coord model == [] then model_to_obj1 2 face_order model
  else "vt " ++ ((sub_i 16 (tex_coord model) 0)) ++ " " ++ ((sub_i 17 (tex_coord model) 1)) ++ "\n" ++ model_to_obj1 1 face_order (model {tex_coord = drop 2 (tex_coord model)})
model_to_obj1 2 face_order model =
  if normal model == [] then model_to_obj1 3 face_order model
  else "vn " ++ ((sub_i 18 (normal model) 0)) ++ " " ++ ((sub_i 19 (normal model) 1)) ++ " " ++ ((sub_i 20 (normal model) 2)) ++ "\n" ++ model_to_obj1 2 face_order (model {normal = drop 3 (normal model)})
model_to_obj1 3 face_order model =
  if face model == [] then []
  else "f" ++ model_to_obj2 (take face_order (face model)) ++ model_to_obj1 3 face_order (model {face = drop face_order (face model)})

model_to_obj0 :: [Model] -> [[Char]] -> Int -> [[Char]]
model_to_obj0 [] _ face_order = []
model_to_obj0 (x:xs) (y:ys) face_order = y : init (model_to_obj1 0 face_order x) : model_to_obj0 xs ys face_order

-- These two functions are used to generate the indices needed for element drawing.
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

-- These two functions deal with reading vertex attribute data from the OBJ file and transforming this into the form used by the engine for rendering.
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

prep_meta_data :: [[Char]] -> Int -> [[Char]]
prep_meta_data (x0:x1:x2:x3:x4:x5:xs) 16 = []
prep_meta_data (x0:x1:x2:x3:x4:x5:xs) c = (show ((read x0) + c) ++ x1 ++ x2 ++ x3 ++ x4 ++ x5) : prep_meta_data (x0:x1:x2:x3:x4:x5:xs) (c + 2)

proc_data :: [Char] -> [Char] -> Int -> UArray (Int, Int) Float -> [Char]
proc_data meta_data vert_data face_order look_up =
  let meta_data' = prep_meta_data (splitOn ", " meta_data) 0
      model = get_vert (splitOneOf "\n /" (drop_double vert_data '/' 'x')) 0 0 blank_model
      model_set = model : rotate_model model 79 look_up
  in intercalate "\n~\n" (model_to_obj0 model_set meta_data' face_order)

--"\n\n" ++ show (model_set !! 0) ++ "\n\n" ++ show (model_set !! 1) ++ "\n\n" ++ show (model_set !! 2) ++ "\n\n" ++ show (model_set !! 3) ++ "\n\n" ++ show (model_set !! 4) ++ "\n\n" ++ show (model_set !! 5) ++ "\n\n" ++ show (model_set !! 6) ++ "\n\n" ++ show (model_set !! 7)

main = do
  args <- getArgs
  h0 <- openFile ((sub_i 21 args 1)) ReadMode
  h1 <- openFile ((sub_i 22 args 2)) WriteMode
  contents <- hGetContents h0
  hPutStr h1 (proc_data ((sub_i 23 (splitOn "\n~\n" contents) 0)) ((sub_i 24 (splitOn "\n~\n" contents) 1)) (read ((sub_i 25 args 0))) (look_up [make_table 0 0, make_table 1 0, make_table 2 0, make_table 3 0]))
  hClose h0
  hClose h1


