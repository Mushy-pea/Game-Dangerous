module Main where

import System.IO
import System.Environment
import Control.Exception
import Data.List.Split
import Data.Array.IArray
import qualified Data.Sequence as SEQ
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

annotateTokens :: Array (Int, Int) Token -> Int -> Int -> Int -> Int -> Int -> Int -> SEQ.Seq [Char] -> SEQ.Seq [Char]
annotateTokens tokenArr i j iMax c outputLine outputColumn annotation
  | i > iMax = annotation
  | j > jMax = annotateTokens tokenArr (i + 1) 0 iMax c (outputLine + 3) 0 annotation
  | otherwise =
    if token == defToken then annotateTokens tokenArr (i + 1) 0 iMax c (outputLine + 3) 0 annotation
    else if content token == "pass_msg" then
      annotateTokens tokenArr i (j + 1) iMax c outputLine (outputColumn + columnJump) (annotation SEQ.|> nextOutput)
    else if content token == "--signal" then
      annotateTokens tokenArr i (j + 1) iMax (c - 1) outputLine (outputColumn + columnJump) (annotation SEQ.|> nextOutput)
    else if content token == "~" then annotateTokens tokenArr i (j + 1) iMax c outputLine outputColumn annotation
    else annotateTokens tokenArr i (j + 1) iMax (c + 1) outputLine (outputColumn + columnJump) (annotation SEQ.|> nextOutput)
  where bd = bounds tokenArr
        jMax = snd (snd bd)
        token = tokenArr ! (i, j)
        whiteText = "\\033[37m"
        blueText = "\\033[34m"
        placeSourceCursor = "\\u001b[" ++ show outputLine ++ ";" ++ show outputColumn ++ "H"
        placeNoteCursor = "\\u001b[" ++ show (outputLine + 1) ++ ";" ++ show outputColumn ++ "H"
        note = "{ " ++ show c ++ " }"
        tokenLength = length (content token)
        noteLength = length note
        columnJump = if tokenLength >= noteLength then tokenLength + 1
                     else noteLength + 1
        nextOutput = placeSourceCursor ++ whiteText ++ content token ++ placeNoteCursor ++ blueText ++ note

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
analyseTokens tokenArr sigBlockSize =
  let annotation = annotateTokens tokenArr 0 0 (fst (snd (bounds tokenArr))) sigBlockSize 0 0 SEQ.empty
  in do
  putStr "\nEnter line number: "
  hFlush stdout
  lineNum <- getLine
  if lineNum == "exit" then return ()
  else if lineNum == "annotate" then do
    report1 annotation 0 ((SEQ.length annotation) - 1)
    analyseTokens tokenArr sigBlockSize
  else do
    report0 tokenArr (read lineNum) sigBlockSize
    analyseTokens tokenArr sigBlockSize

report0 :: Array (Int, Int) Token -> Int -> Int -> IO ()
report0 tokenArr lineNum sigBlockSize =
  let result = countTokens tokenArr 0 0 (lineNum - 4) 0 0
  in do
  putStr ("Bytecode length to this point: " ++ show (fst result - snd result + sigBlockSize) ++ "\n")

report1 :: SEQ.Seq [Char] -> Int -> Int -> IO ()
report1 annotation i iMax
  | i > iMax = return ()
  | otherwise = do
    putStr (SEQ.index annotation i)
    report1 annotation (i + 1) iMax
  


