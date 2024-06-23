-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This module is a launcher for the engine.  Users can change engine settings relative to those in a given configuration file 
-- through an interactive text prompt.  This is implemented by the launcher passing command line arguments to the engine that 
-- override the defaults in the configuration file.

module Main where

import System.IO
import System.Process
import Control.Exception
import Data.Maybe

getSignalTrace :: Handle -> ProcessHandle -> [[Char]] -> IO [[Char]]
getSignalTrace h_out ph acc = do
  line <- hGetLine h_out
  exit_code <- getProcessExitCode ph
  if isJust exit_code then return (reverse (line : acc))
  else getSignalTrace h_out ph (line : acc)

main = do
  putStr "\nThanks for choosing to play Game :: Dangerous!"
  (h_in, h_out, _, ph) <- createProcess (shell "./Game-Dangerous-exe") {std_in = CreatePipe, std_out = CreatePipe}
  signal_trace <- getSignalTrace (fromJust h_out) ph []
  bracket (openFile "log.txt" WriteMode) (hClose) (\h -> hPutStr h (show signal_trace))
  
  
