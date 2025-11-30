module Main where

import System.IO
import System.Environment
import Control.Exception
import Data.List.Split
import Data.Array.IArray
import qualified Data.Sequence as SEQ
import qualified Data.Foldable as FOLD
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

labelTokens :: Array (Int, Int) Token -> Int -> Int -> Int -> Int -> Int -> SEQ.Seq [Char] -> SEQ.Seq [Char]
labelTokens tokenArr i j iMax c sigBlockSizePlus labeledTokens
  | i > iMax = labeledTokens
  | j > jMax = labelTokens tokenArr (i + 1) 0 iMax c sigBlockSizePlus labeledTokens
  | otherwise =
    if token == defToken then labelTokens tokenArr (i + 1) 0 iMax c sigBlockSizePlus (labeledTokens SEQ.|> "<br>")
    else if content token == "pass_msg" then
      labelTokens tokenArr i (j + 1) iMax c sigBlockSizePlus (labeledTokens SEQ.|> nextElement)
    else if content token == "--signal" then
      labelTokens tokenArr i (j + 1) iMax (c - 1) sigBlockSizePlus (labeledTokens SEQ.|> nextElement)
    else if content token == "~" then labelTokens tokenArr i (j + 1) iMax sigBlockSizePlus sigBlockSizePlus labeledTokens
    else labelTokens tokenArr i (j + 1) iMax (c + 1) sigBlockSizePlus (labeledTokens SEQ.|> nextElement)
  where bd = bounds tokenArr
        jMax = snd (snd bd)
        token = tokenArr ! (i, j)
        label = "<span style=\"color: Blue\">" ++ show c ++ ": </span>"
        token_ = "<span style=\"color: Black\">" ++ content token ++ "</span> "
        nextElement = label ++ token_

main = do
  args <- getArgs
  source <- bracket (openFile (args !! 0) ReadMode) (hClose)
                    (\h -> do c <- hGetContents h; putStr ("\nsource file size: " ++ show (length c)); return c)
  loadTokenArray ((splitOn "\n~\n" source) !! 1) (read (args !! 2)) (args !! 1)

loadTokenArray :: [Char] -> Int -> [Char] -> IO ()
loadTokenArray source sigBlockSize outputFile =
  let splitSource = splitOn " " source
      arrayDim = detArrayDim splitSource 0 0 0
      emptyTokenArray = array ((0, 0), (fst__ arrayDim, (snd__ arrayDim) - 1))
                              [((i, j), defToken) | i <- [0..fst__ arrayDim], j <- [0..(snd__ arrayDim) - 1]]
      blockArr = labelIfBlocks splitSource (fst__ arrayDim) ((snd__ arrayDim) - 1)
      tokenArr = tokenise splitSource emptyTokenArray blockArr 1 0 0
  in do
  analyseTokens tokenArr sigBlockSize outputFile

analyseTokens :: Array (Int, Int) Token -> Int -> [Char] -> IO ()
analyseTokens tokenArr sigBlockSize outputFile =
  let labeledTokens = labelTokens tokenArr 0 0 (fst (snd (bounds tokenArr))) 0 (sigBlockSize + 3) SEQ.empty
      templateFile = "C:\\Users\\steve\\Software-Projects\\Game-Dangerous\\index.html"
  in do
  putStr "\nEnter line number: "
  hFlush stdout
  lineNum <- getLine
  if lineNum == "exit" then return ()
  else if lineNum == "annotate" then do
    template <- bracket (openFile templateFile ReadMode) (hClose)
                        (\h -> do c <- hGetContents h; putStr ("\ntemplate file size: " ++ show (length c)); return c)
    report1 labeledTokens (splitOn "\n" template) outputFile
    analyseTokens tokenArr sigBlockSize outputFile
  else do
    report0 tokenArr (read lineNum) sigBlockSize
    analyseTokens tokenArr sigBlockSize outputFile

report0 :: Array (Int, Int) Token -> Int -> Int -> IO ()
report0 tokenArr lineNum sigBlockSize =
  let result = countTokens tokenArr 0 0 (lineNum - 4) 0 0
  in do
  putStr ("Bytecode length to this point: " ++ show (fst result - snd result + sigBlockSize) ++ "\n")

report1 :: SEQ.Seq [Char] -> [[Char]] -> [Char] -> IO ()
report1 labeledTokens template outputFile =
  let header = (template !! 0) ++ "\n" ++ (template !! 1) ++ "\n" ++ (template !! 2) ++ "\n" ++ (template !! 3) ++ "\n" ++
               (template !! 4) ++ "\n" ++ (template !! 5) ++ "\n" ++ (template !! 6) ++ "\n" ++ (template !! 7) ++ "\n"
      footer = (template !! 8) ++ "\n" ++ (template !! 9) ++ "\n" ++ (template !! 10) ++ "\n"
  in do
  bracket (openFile outputFile WriteMode) (hClose)
          (\h -> hPutStr h (header ++ (concat (FOLD.toList labeledTokens)) ++ footer))

