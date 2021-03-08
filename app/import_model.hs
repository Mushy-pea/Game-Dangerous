-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This is a development tool that transforms a simplified form of the OBJ file format into the model import format used by the engine.

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

subI :: Int -> [a] -> Int -> a
subI location ls i =
  if i >= length ls then error ("List index too large.  location: " ++ show location ++ " index: " ++ show i ++ " max: " ++ show ((length ls) - 1))
  else ls !! i

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

-- These two functions are used to generate the indices needed for element drawing
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
      test = (subI 0 (splitOn " " x) 0)
      f_ind0 = faceIndex (div order 2) 0
      f_ind1 = faceIndex (div order 3) 0
  in
  if test == "usemtl" then genIndices xs mode n num_i acc
  else if test == "v" then genIndices xs mode n num_i acc
  else if test == "vn" then genIndices xs mode n num_i acc
  else if test == "vt" then genIndices xs mode n num_i acc
  else if mode == 0 then genIndices xs mode (n + (div order 2)) (num_i + length f_ind0) (acc ++ showInts (map (+ (n - 2)) f_ind0))
  else genIndices xs mode (n + (div order 3)) (num_i + length f_ind1) (acc ++ showInts (map (+ (n - 2)) f_ind1))

-- These two functions deal with reading vertex attribute data from the OBJ file and transforming this into the form used by the engine for rendering
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
  let face_vert = (((subI 1 (face model) 0)) * 3) - 3
      face_tex = (((subI 2 (face model) 1)) * 2) - 2
      face_norm = (((subI 3 (face model) 2)) * 3) - 3
      face_norm' = (((subI 4 (face model) 1)) * 3) - 3
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
  if x == "usemtl" then addColour0 (drop 1 xs) mat (addColour1 mat ((subI 5 xs 0)))
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

procModDescrip :: [[Char]] -> [Char]
procModDescrip [] = []
procModDescrip (x0:x1:x2:x3:x4:x5:xs) = x0 ++ ", " ++ x1 ++ ", " ++ x2 ++ ", " ++ x3 ++ ", " ++ x4 ++ ", " ++ x5 ++ "&" ++ procModDescrip xs

main = do
  args <- getArgs
  h0 <- openFile ((subI 6 args 0)) ReadMode
  h1 <- openFile ((subI 7 args 1)) ReadMode
  h2 <- openFile ((subI 8 args 2)) WriteMode
  contents0 <- hGetContents h0
  contents1 <- hGetContents h1
  unrollData (splitOn "\n~\n" contents0) (splitOn "\n~\n" contents1) h2 blank_loader [] [] 0 0 0
  hClose h0
  hClose h1
  hClose h2

unrollData :: [[Char]] -> [[Char]] -> Handle -> Loader -> [Char] -> [Char] -> Int -> Int -> Int -> IO ()
unrollData [] _ h2 load_acc vert_acc ind_acc e_count i_count mat_count = hPutStr h2 (init_ (tex_image load_acc) ++ "~" ++ init (procModDescrip (init (splitOn ", " (mod_descrip load_acc)))) ++ "~" ++ init_ (data_layout load_acc) ++ "~" ++ init_ vert_acc ++ "~" ++ init_ ind_acc)
unrollData (x0:x1:xs) mat h2 load_acc vert_acc ind_acc e_count i_count mat_count =
  let mode = detMode (read ((subI 9 (splitOn ", " x0) 1)))
      col_attrib = addColour0 (splitOneOf "\n /" (dropDouble x1 '/' 'x')) (getColour (splitOneOf "\n " ((subI 10 mat mat_count)))) []
      pl_vert0 = placeVert (getVert (splitOneOf "\n /" (dropDouble x1 '/' 'x')) 0 mode blank_model) mode [] [] [] col_attrib 0
      pl_vert1 = placeVert (getVert (splitOneOf "\n /" (dropDouble x1 '/' 'x')) 0 mode blank_model) mode [] [] [] [] 0
      vertices0 = fst pl_vert0
      vertices1 = fst pl_vert1
      indices = genIndices (splitOn "\n" (dropDouble x1 '/' 'x')) mode 1 0 []
      m_data = (splitOn ", " x0)
      l_data0 = load_acc {mod_descrip = mod_descrip load_acc ++ ((subI 11 m_data 0)) ++ ", " ++ show (snd pl_vert0) ++ ", " ++ concat_ 0 (drop 1 m_data) ++ ", ", data_layout = data_layout load_acc ++ showInts [e_count, numElem 0 (snd pl_vert0), i_count, snd indices]}
      l_data1 = load_acc {tex_image = tex_image load_acc ++ (concat_ 0 (drop 5 (splitOn ", " x0))) ++ ", ",mod_descrip = mod_descrip load_acc ++ ((subI 12 m_data 0)) ++ ", " ++ show (snd pl_vert0) ++ ", " ++ (concat_ 0 (take 4 (drop 1 m_data))) ++ ", ", data_layout = data_layout load_acc ++ showInts [e_count, numElem 1 (snd pl_vert1), i_count, snd indices]}
  in do
  if mode == 0 then unrollData xs mat h2 l_data0 (vert_acc ++ fst pl_vert0 ++ ", ") (ind_acc ++ fst indices) (e_count + numElem 0 (snd pl_vert0)) (i_count + snd indices) (mat_count + 1)
  else unrollData xs mat h2 l_data1 (vert_acc ++ fst pl_vert1 ++ ", ") (ind_acc ++ fst indices) (e_count + numElem 1 (snd pl_vert1)) (i_count + snd indices) mat_count

