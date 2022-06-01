-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This is a development tool used to apply translations and rotations to 3D models, which uses a modified form of the OBJ file format for input and output.

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

subI :: Int -> [a] -> Int -> a
subI location ls i =
  if i >= length ls then error ("List index too large.  location: " ++ show location ++ " index: " ++ show i ++ " max: " ++ show ((length ls) - 1))
  else ls !! i

data Model = Model {vertex :: [[Char]], tex_coord :: [[Char]], normal :: [[Char]], face :: [Int]} deriving Show

data Material = Material {mat_name :: [Char], colour :: [[Char]]} deriving Show

data Loader = Loader {tex_image :: [Char], mod_descrip :: [Char], data_layout :: [Char]} deriving Show

blank_model = Model {vertex = [], tex_coord = [], normal = [], face = []}
blank_loader = Loader {tex_image = [], mod_descrip = [], data_layout = []}

init_ :: [a] -> [a]
init_ x = take ((length x) - 2) x

showInts :: [Int] -> [Char]
showInts [] = []
showInts (x:xs) = (show x) ++ ", " ++ showInts xs

dropDouble :: Eq a => [a] -> a -> a -> [a]
dropDouble [] t0 t1 = []
dropDouble (x:xs) t0 t1 =
  if x == t0 && x == t1 then dropDouble xs t0 x
  else x : dropDouble xs t0 x

concat_ :: Int -> [[Char]] -> [Char]
concat_ 0 x = (intercalate ", " x)
concat_ 1 x = (intercalate ", " x) ++ ", 1"

-- These functions are used to optionally apply rotations to a model before the data is unrolled.
makeTable :: Int -> Float -> [Float]
makeTable 0 a =
  if a > 6.29 then []
  else a : makeTable 0 (a + 0.01)
makeTable 1 a =
  if a > 6.29 then []
  else sin a : makeTable 1 (a + 0.01)
makeTable 2 a =
  if a > 6.29 then []
  else cos a : makeTable 2 (a + 0.01)
makeTable 3 a =
  if a > 6.29 then []
  else tan a : makeTable 3 (a + 0.01)

lookUp :: [[Float]] -> UArray (Int, Int) Float
lookUp a = array ((0, 0), (3, 628)) [((x, y), realToFrac ((a !! x) !! y)) | x <- [0..3], y <- [0..628]]

rotationW :: Int -> UArray (Int, Int) Float -> Matrix Float
rotationW a look_up = fromList 4 4 [look_up ! (2, a), - look_up ! (1, a), 0, 0, look_up ! (1, a), look_up ! (2, a), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]

translation :: Floating a => a -> a -> a -> Matrix a
translation u v w = fromList 4 4 [1, 0, 0, u, 0, 1, 0, v, 0, 0, 1, w, 0, 0, 0, 1]

transform :: Floating a => [Matrix a] -> Matrix a -> [Matrix a]
transform [] mat = []
transform (v:vs) mat = (multStd mat v) : transform vs mat

convToVector :: Int -> Model -> [Matrix Float] -> [Matrix Float] -> ([Matrix Float], [Matrix Float])
convToVector 0 model vert_acc norm_acc =
  if vertex model == [] then convToVector 1 model vert_acc norm_acc
  else convToVector 0 (model {vertex = drop 3 (vertex model)}) (vert_acc ++ [fromList 4 1 [read ((subI 3 (vertex model) 0)), read ((subI 4 (vertex model) 1)), read ((subI 5 (vertex model) 2)), 1]]) norm_acc
convToVector 1 model vert_acc norm_acc =
  if normal model == [] then (vert_acc, norm_acc)
  else convToVector 1 (model {normal = drop 3 (normal model)}) vert_acc (norm_acc ++ [fromList 4 1 [read ((subI 6 (normal model) 0)), read ((subI 7 (normal model) 1)), read ((subI 8 (normal model) 2)), 1]])

convToModel :: [Matrix Float] -> [[Char]]
convToModel [] = []
convToModel (x:xs) =
  let ls = toList x
  in [show ((subI 9 ls 0)), show ((subI 10 ls 1)), show ((subI 11 ls 2))] ++ convToModel xs

rotateModel :: Model -> Int -> Int -> UArray (Int, Int) Float -> [Model]
rotateModel model a da look_up =
  let vector_form = convToVector 0 model [] []
  in
  if a > 553 then []
  else model {vertex = convToModel (transform (fst vector_form) (rotationW a look_up)), normal = convToModel (transform (snd vector_form) (rotationW a look_up))} : rotateModel model (a + da) da look_up

translateModel :: Model -> Float -> Float -> Float -> Model
translateModel model u v w =
  let vector_form = convToVector 0 model [] []
  in model {vertex = convToModel (transform (fst vector_form) (translation u v w))}

modelToObj2 :: [Int] -> [Char]
modelToObj2 [] = "\n"
modelToObj2 (x0:x1:x2:xs) = " " ++ show x0 ++ "/" ++ show x1 ++ "/" ++ show x2 ++ modelToObj2 xs

modelToObj1 :: Int -> Int -> Model -> [Char]
modelToObj1 0 face_order model =
  if vertex model == [] then modelToObj1 1 face_order model
  else "v " ++ ((subI 13 (vertex model) 0)) ++ " " ++ ((subI 14 (vertex model) 1)) ++ " " ++ ((subI 15 (vertex model) 2)) ++ "\n" ++ modelToObj1 0 face_order (model {vertex = drop 3 (vertex model)})
modelToObj1 1 face_order model =
  if tex_coord model == [] then modelToObj1 2 face_order model
  else "vt " ++ ((subI 16 (tex_coord model) 0)) ++ " " ++ ((subI 17 (tex_coord model) 1)) ++ "\n" ++ modelToObj1 1 face_order (model {tex_coord = drop 2 (tex_coord model)})
modelToObj1 2 face_order model =
  if normal model == [] then []
  else "vn " ++ ((subI 18 (normal model) 0)) ++ " " ++ ((subI 19 (normal model) 1)) ++ " " ++ ((subI 20 (normal model) 2)) ++ "\n" ++ modelToObj1 2 face_order (model {normal = drop 3 (normal model)})

modelToObj0 :: [Model] -> [[Char]] -> Int -> [[Char]]
modelToObj0 [] _ face_order = []
modelToObj0 (x:xs) (y:ys) face_order = y : init (modelToObj1 0 face_order x) : modelToObj0 xs ys face_order

-- These two functions are used to generate the indices needed for element drawing.
faceIndex :: Int -> Int -> [Int]
faceIndex order 0 =
  if order < 4 then [1, 2, 3]
  else if order < 5 then [1, 2, 4, 2, 3, 4]
  else [1, 2, 4, 2, 3, 4] ++ faceIndex order 3
faceIndex order n =
  if n > (order - 2) then []
  else [n + 1, n + 2, 1] ++ faceIndex order (n + 1)

genIndices :: [[Char]] -> Int -> Int -> Int -> [Char] -> ([Char], Int)
genIndices [] mode n num_i acc = (acc, num_i)
genIndices (x:xs) mode n num_i acc =
  let order = ((length (splitOneOf " /" x)) - 1)
      test = (subI 28 (splitOn " " x) 0)
      f_ind0 = faceIndex (div order 2) 0
      f_ind1 = faceIndex (div order 3) 0
  in
  if test == "usemtl" then genIndices xs mode n num_i acc
  else if test == "v" then genIndices xs mode n num_i acc
  else if test == "vn" then genIndices xs mode n num_i acc
  else if test == "vt" then genIndices xs mode n num_i acc
  else if mode == 0 then genIndices xs mode (n + (div order 2)) (num_i + length f_ind0) (acc ++ showInts (map (+ (n - 2)) f_ind0))
  else genIndices xs mode (n + (div order 3)) (num_i + length f_ind1) (acc ++ showInts (map (+ (n - 2)) f_ind1))

-- These two functions deal with reading vertex attribute data from the OBJ file and transforming this into the form used by the engine for rendering.
getVert :: [[Char]] -> Int -> Int -> Model -> Model
getVert [] phase mode model = model
getVert (x:xs) 0 mode model =
  if x == "v" then getVert (drop 3 xs) 0 mode (model {vertex = (vertex model) ++ take 3 xs})
  else if x == "vt" then getVert (drop 2 xs) 0 mode (model {tex_coord = (tex_coord model) ++ take 2 xs})
  else if x == "vn" then getVert (drop 3 xs) 0 mode (model {normal = (normal model) ++ take 3 xs})
  else if x == "usemtl" then getVert (drop 1 xs) 0 mode model
  else getVert xs 1 mode model
getVert (x:xs) 1 mode model =
  if x == "usemtl" then getVert (drop 1 xs) 1 mode model
  else if x == "f" then getVert xs 1 mode model
  else getVert xs 1 mode (model {face = (face model) ++ [read x]})

getColour :: [[Char]] -> [Material]
getColour [] = []
getColour (x0:x1:x2:x3:xs) = Material {mat_name = x0, colour = [x1, x2, x3]} : getColour xs

checkAttrib :: [[Char]] -> Int -> [[Char]]
checkAttrib [] i = ["e"] ++ [show i]
checkAttrib attrib i = attrib

placeVert :: Model -> Int -> [Char] -> [Char] -> [Char] -> [Char] -> Int -> ([Char], Int)
placeVert model mode vert_acc tex_acc norm_acc col_attrib c =
  let face_vert = ((subI 29 (face model) 0)) * 3 - 3
      face_tex = ((subI 30 (face model) 1)) * 2 - 2
      face_norm = ((subI 31 (face model) 2)) * 3 - 3
      face_norm' = ((subI 32 (face model) 1)) * 3 - 3
  in
  if face model == [] then (init_ (vert_acc ++ col_attrib ++ tex_acc ++ norm_acc), c)
  else if mode == 0 then placeVert (model {face = drop 2 (face model)}) mode (vert_acc ++ concat_ 1 (checkAttrib (take 3 (drop face_vert (vertex model))) face_vert) ++ ", ") tex_acc (norm_acc ++ concat_ 0 (checkAttrib (take 3 (drop face_norm' (normal model))) face_norm') ++ ", ") col_attrib (c + 1)
  else placeVert (model {face = drop 3 (face model)}) mode (vert_acc ++ concat_ 1 (checkAttrib (take 3 (drop face_vert (vertex model))) face_vert) ++ ", ") (tex_acc ++ concat_ 0 (checkAttrib (take 2 (drop face_tex (tex_coord model))) face_tex) ++ ", ") (norm_acc ++ concat_ 0 (checkAttrib (take 3 (drop face_norm (normal model))) face_norm) ++ ", ") col_attrib (c + 1)

addColour1 :: [Material] -> [Char] -> [Char]
addColour1 [] query = []
addColour1 (x:xs) query =
  if mat_name x == query then concat_ 0 (colour x)
  else addColour1 xs query

addColour0 :: [[Char]] -> [Material] -> [Char] -> [Char]
addColour0 [] mat col = []
addColour0 (x:xs) mat col =
  if x == "usemtl" then addColour0 (drop 1 xs) mat (addColour1 mat ((subI 33 xs 0)))
  else if x == "v" then addColour0 (drop 3 xs) mat col
  else if x == "vt" then addColour0 (drop 2 xs) mat col
  else if x == "vn" then addColour0 (drop 3 xs) mat col
  else if x == "f" then addColour0 xs mat col
  else col ++ ", " ++ addColour0 xs mat col

numElem :: Int -> Int -> Int
numElem 0 c = 11 * c
numElem 1 c = 9 * c

numInd :: Int -> Int
numInd f =
  if f < 4 then 3
  else 3 + (f - 3) * 3

detMode :: Int -> Int
detMode num_tex =
  if num_tex > 0 then 1
  else 0

prepMetaData :: [[Char]] -> Int -> [[Char]]
prepMetaData (x0:x1:x2:x3:x4:x5:xs) 16 = []
prepMetaData (x0:x1:x2:x3:x4:x5:xs) c = (show ((read x0) + c) ++ x1 ++ x2 ++ x3 ++ x4 ++ x5) : prepMetaData (x0:x1:x2:x3:x4:x5:xs) (c + 2)

procData :: Int -> Int -> Float -> Float -> Float -> [Char] -> [Char] -> UArray (Int, Int) Float -> [Char]
procData mode da u v w meta_data vert_data lookUp =
  let meta_data' = prepMetaData (splitOn ", " meta_data) 0
      model = getVert (splitOneOf "\n /" (dropDouble vert_data '/' 'x')) 0 0 blank_model
      rotated_set = model : rotateModel model da da lookUp
      translated_model = translateModel model u v w
  in
  if mode == 0 then intercalate "\n~\n" (modelToObj0 [translated_model] meta_data' 0)
  else intercalate "\n~\n" (modelToObj0 rotated_set meta_data' 0)

main = do
  args <- getArgs
  h0 <- openFile ((subI 21 args 0)) ReadMode
  h1 <- openFile ((subI 22 args 1)) WriteMode
  contents <- hGetContents h0
  hPutStr h1 (procData (read ((subI 34 args 2))) (read ((subI 38 args 3))) (read ((subI 35 args 4))) (read ((subI 36 args 5))) (read ((subI 37 args 6))) ((subI 23 (splitOn "\n~\n" contents) 0)) ((subI 24 (splitOn "\n~\n" contents) 1)) (lookUp [makeTable 0 0, makeTable 1 0, makeTable 2 0, makeTable 3 0]))
  hClose h0
  hClose h1
  
