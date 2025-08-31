module Main where

import System.IO
import System.Environment
import Control.Exception
import Data.List.Split
import Data.Array.IArray
import BuildModel
import CompileGPLC

countTokens :: Array (Int, Int) Token -> Int -> Int -> Int -> Int -> Int -> (Int, Int)
countTokens tokenArr i j iMax cDataBlock c
  | i > iMax = (c + 3, cDataBlock)
  | j > jMax = countTokens tokenArr (i + 1) 0 iMax cDataBlock c
  | otherwise =
    if token == defToken || content token == "pass_msg" then countTokens tokenArr i (j + 1) iMax cDataBlock c
    else if content token == "--signal" then countTokens tokenArr i (j + 1) iMax cDataBlock (c - 1)
    else if content token == "~" then countTokens tokenArr i (j + 1) iMax c c
    else countTokens tokenArr i (j + 1) iMax cDataBlock (c + 1)
  where bd = bounds tokenArr
        jMax = snd (snd bd)
        token = tokenArr ! (i, j)

main = do
  args <- getArgs
  source <- bracket (openFile (args !! 0) ReadMode) (hClose)
                    (\h -> do c <- hGetContents h; putStr ("\nsource file size: " ++ show (length c)); return c)
  loadTokenArray ((splitOn "\n~\n" source) !! 1) (read (args !! 1))

loadTokenArray :: [Char] -> Int -> IO ()
loadTokenArray source sigBlockSize =
  let splitSource = splitOn " " source
      arrayDim = detArrayDim splitSource 0 0 0
      emptyTokenArray = array ((0, 0), (fst__ arrayDim, (snd__ arrayDim) - 1))
                              [((i, j), defToken) | i <- [0..fst__ arrayDim], j <- [0..(snd__ arrayDim) - 1]]
      blockArr = labelIfBlocks splitSource (fst__ arrayDim) ((snd__ arrayDim) - 1)
      tokenArr = tokenise splitSource emptyTokenArray blockArr 1 0 0
  in do
  analyseTokens tokenArr sigBlockSize

analyseTokens :: Array (Int, Int) Token -> Int -> IO ()
analyseTokens tokenArr sigBlockSize = do
  putStr "\nEnter line number: "
  hFlush stdout
  lineNum <- getLine
  if lineNum == "exit" then return ()
  else do
    report tokenArr (read lineNum) sigBlockSize
    analyseTokens tokenArr sigBlockSize

report :: Array (Int, Int) Token -> Int -> Int -> IO ()
report tokenArr lineNum sigBlockSize =
  let result = countTokens tokenArr 0 0 (lineNum - 4) 0 0
  in do
  putStr ("Bytecode length to this point: " ++ show (fst result - snd result + sigBlockSize) ++ "\n")

