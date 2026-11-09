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

-- These functions are used to generate the indices needed for element drawing.
-- Faces are triangulated by ear clipping so that concave polygons (such as the U shaped end boards of the bed model) are split correctly.
-- A fan triangulation is only valid for convex polygons and fills in the concave regions of other shapes.
type Vec3 = (Double, Double, Double)

readD :: [Char] -> Double
readD ('+':s) = readD s
readD ('-':s) = negate (readD s)
readD ('.':s) = read ("0." ++ s)
readD s = read s

-- Collects the vertex positions declared by the "v" lines of a model block.
getPositions :: [[Char]] -> [Vec3]
getPositions [] = []
getPositions (x:xs) =
  case words x of
    ("v":a:b:c:_) -> (readD a, readD b, readD c) : getPositions xs
    _ -> getPositions xs

-- Returns the (1 based) position indices referenced by a face line, e.g. "f 1/1/3 2/1/3 3/1/3" -> [1, 2, 3].
faceVerts :: [Char] -> [Int]
faceVerts x = map (read . head . splitOn "/") (drop 1 (words x))

-- Projects a planar polygon onto the coordinate plane it is least oblique to, using the Newell method normal.
project :: [Vec3] -> [(Double, Double)]
project ps =
  let pairs = zip ps (tail ps ++ [head ps])
      nx = sum [(y0 - y1) * (z0 + z1) | ((_, y0, z0), (_, y1, z1)) <- pairs]
      ny = sum [(z0 - z1) * (x0 + x1) | ((x0, _, z0), (x1, _, z1)) <- pairs]
      nz = sum [(x0 - x1) * (y0 + y1) | ((x0, y0, _), (x1, y1, _)) <- pairs]
  in
  if abs nx >= abs ny && abs nx >= abs nz then [(y, z) | (_, y, z) <- ps]
  else if abs ny >= abs nz then [(z, x) | (x, _, z) <- ps]
  else [(x, y) | (x, y, _) <- ps]

cross2 :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
cross2 (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

eps :: Double
eps = 1e-10

-- Inclusive point in triangle test, independent of the triangle's winding.
inTriangle :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
inTriangle a b c p =
  let d = [cross2 a b p, cross2 b c p, cross2 c a p]
  in not (any (< (- eps)) d && any (> eps) d)

-- Returns 1 based indices into the face's vertex list, three per triangle, preserving the face's winding order.
-- Triangles and convex quads give the same result as the previous fan triangulation.
triangulate :: [Vec3] -> [Int]
triangulate ps =
  let k = length ps
      pts = project ps
      area = sum [x0 * y1 - x1 * y0 | ((x0, y0), (x1, y1)) <- zip pts (tail pts ++ [head pts])]
      orient = if area < 0 then (-1) else 1
  in
  if k < 3 then []
  else if k == 3 then [1, 2, 3]
  else earClip pts orient [1 .. k]

earClip :: [(Double, Double)] -> Double -> [Int] -> [Int]
earClip pts orient remaining =
  let n = length remaining
      pt j = pts !! (j - 1)
      corners i = (remaining !! (mod (i - 1) n), remaining !! i, remaining !! (mod (i + 1) n))
      isEar i =
        let (a, b, c) = corners i
            (pa, pb, pc) = (pt a, pt b, pt c)
            others = [pt j | j <- remaining, j /= a, j /= b, j /= c, pt j /= pa, pt j /= pb, pt j /= pc]
        in orient * cross2 pa pb pc > eps && not (any (inTriangle pa pb pc) others)
      -- If no ear is found the face is degenerate (e.g. self intersecting), so clip the first vertex to guarantee termination.
      ear = case filter isEar [0 .. n - 1] of
              [] -> 0
              (e:_) -> e
      (prev, cur, next) = corners ear
  in
  if n < 3 then []
  else if n == 3 then remaining
  else [cur, next, prev] ++ earClip pts orient (take ear remaining ++ drop (ear + 1) remaining)

genIndices :: [[Char]] -> [Vec3] -> Int -> Int -> Int -> [Char] -> ([Char], Int)
genIndices [] positions mode n num_i acc = (acc, num_i)
genIndices (x:xs) positions mode n num_i acc =
  let order = ((length (splitOneOf " /" x)) - 1)
      test = (subI 0 (splitOn " " x) 0)
      f_ind = triangulate (map (\i -> subI 13 positions (i - 1)) (faceVerts x))
  in
  if test == "usemtl" then genIndices xs positions mode n num_i acc
  else if test == "v" then genIndices xs positions mode n num_i acc
  else if test == "vn" then genIndices xs positions mode n num_i acc
  else if test == "vt" then genIndices xs positions mode n num_i acc
  else if mode == 0 then genIndices xs positions mode (n + (div order 2)) (num_i + length f_ind) (acc ++ showInts (map (+ (n - 2)) f_ind))
  else genIndices xs positions mode (n + (div order 3)) (num_i + length f_ind) (acc ++ showInts (map (+ (n - 2)) f_ind))

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
      indices = genIndices (splitOn "\n" (dropDouble x1 '/' 'x')) (getPositions (splitOn "\n" x1)) mode 1 0 []
      m_data = (splitOn ", " x0)
      l_data0 = load_acc {mod_descrip = mod_descrip load_acc ++ ((subI 11 m_data 0)) ++ ", " ++ show (snd pl_vert0) ++ ", " ++ concat_ 0 (drop 1 m_data) ++ ", ", data_layout = data_layout load_acc ++ showInts [e_count, numElem 0 (snd pl_vert0), i_count, snd indices]}
      l_data1 = load_acc {tex_image = tex_image load_acc ++ (concat_ 0 (drop 5 (splitOn ", " x0))) ++ ", ",mod_descrip = mod_descrip load_acc ++ ((subI 12 m_data 0)) ++ ", " ++ show (snd pl_vert0) ++ ", " ++ (concat_ 0 (take 4 (drop 1 m_data))) ++ ", ", data_layout = data_layout load_acc ++ showInts [e_count, numElem 1 (snd pl_vert1), i_count, snd indices]}
  in do
  if mode == 0 then unrollData xs mat h2 l_data0 (vert_acc ++ fst pl_vert0 ++ ", ") (ind_acc ++ fst indices) (e_count + numElem 0 (snd pl_vert0)) (i_count + snd indices) (mat_count + 1)
  else unrollData xs mat h2 l_data1 (vert_acc ++ fst pl_vert1 ++ ", ") (ind_acc ++ fst indices) (e_count + numElem 1 (snd pl_vert1)) (i_count + snd indices) mat_count

