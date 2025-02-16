-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This module is a launcher for the engine.  Users can change engine settings relative to those in a given configuration file 
-- through an interactive text prompt.  This is implemented by the launcher passing command line arguments to the engine that 
-- override the defaults in the configuration file.

module Main where

import System.IO
import System.Environment
import System.Process
import System.Exit
import Control.Exception
import Data.Maybe
import Data.Array.IArray
import Data.List.Split
import Data.IORef
import BuildModel

data Question = Question {questionText :: [Char], questionPointers :: [([Char], Int)]}

rootQuestionText = "\n\nPlease enter one of the following keywords to proceed"
                   ++ "\n\nstart: start Game :: Dangerous engine"
                   ++ "\nmap: Enter the starting map file"
                   ++ "\nsaveFile: Specify a saved game file to attempt to load at engine start"
                   ++ "\ncontrolKey: Specify a key binding"
                   ++ "\nresolution: Specify the initial horizontal resolution"

rootQuestionPointers = [("map", 1), ("saveFile", 2), ("controlKey", 3), ("resolution", 4), ("start", 22)]

mapQuestionText = "Which map would you like to load (map1 - map4)?\n"

saveQuestionText = "Which save file would you like to load (1 - 6)?\n"

controlKeyQuestionText = "Which control key would you like to change? :\n"
                         ++ "pause\n"
                         ++ "forward\n"
                         ++ "back\n"
                         ++  "strafe left\n"
                         ++  "strafe right\n"
                         ++  "turn left\n"
                         ++  "turn right\n"
                         ++  "jump\n"
                         ++  "light torch\n"
                         ++  "switch view (Toggle 1st and 3rd person camera view)\n"
                         ++  "rotate view\n"
                         ++  "fire"

controlKeyQuestionPointers = [("pause", 5), ("forward", 6), ("back", 7), ("strafe left", 8), ("strafe right", 9), ("turn left", 10), 
                              ("turn right", 11), ("jump", 12), ("light torch", 13), ("switch view", 14), ("rotate view", 15), ("fire", 16)]

resolutionQuestionText = "Which of the following resolutions do you wish the engine to attempt at launch? :\n"
                          ++ "[1024 * 768]\n"
                          ++ "[1920 * 1080]\n"
                          ++ "[2560 * 1440]\n"
                          ++ "[3200 * 1800]\n"
                          ++ "[3840 * 2160]\n"

resolutionQuestionPointers = [("[1024 * 768]", 17), ("[1920 * 1080]", 18), ("[2560 * 1440]", 19), ("[3200 * 1800]", 20), ("[3840 * 2160]", 21)]

rootQuestion = Question {questionText = rootQuestionText, questionPointers = rootQuestionPointers}

mapQuestion = Question {questionText = mapQuestionText, questionPointers = []}

saveQuestion = Question {questionText = saveQuestionText, questionPointers = []}

controlKeyQuestion = Question {questionText = controlKeyQuestionText, questionPointers = controlKeyQuestionPointers}

resolutionQuestion = Question {questionText = resolutionQuestionText, questionPointers = resolutionQuestionPointers}

pauseQuestion = Question {questionText = "Enter key to bind to [Pause]: ", questionPointers = []}

forwardQuestion = Question {questionText = "Enter key to bind to [Forward]: ", questionPointers = []}

backQuestion = Question {questionText = "Enter key to bind to [Back]: ", questionPointers = []}

strafeLeftQuestion = Question {questionText = "Enter key to bind to [Strafe left]: ", questionPointers = []}

strafeRightQuestion = Question {questionText = "Enter key to bind to [Strafe right]: ", questionPointers = []}

turnLeftQuestion = Question {questionText = "Enter key to bind to [Turn left]: ", questionPointers = []}

turnRightQuestion = Question {questionText = "Enter key to bind to [Turn right]: ", questionPointers = []}

jumpQuestion = Question {questionText = "Enter key to bind to [Jump]: ", questionPointers = []}

lightTorchQuestion = Question {questionText = "Enter key to bind to [Light torch]: ", questionPointers = []}

switchViewQuestion = Question {questionText = "Enter key to bind to [Switch view]: ", questionPointers = []}

rotateViewQuestion = Question {questionText = "Enter key to bind to [Rotate view]: ", questionPointers = []}

fireQuestion = Question {questionText = "Enter key to bind to [Fire]: ", questionPointers = []}

resolution768 = Question {questionText = "[1024 * 768] resolution selected.", questionPointers = []}

resolution1080 = Question {questionText = "[1920 * 1080] resolution selected.", questionPointers = []}

resolution1440 = Question {questionText = "[2560 * 1440] resolution selected.", questionPointers = []}

resolution1800 = Question {questionText = "[3200 * 1800] resolution selected.", questionPointers = []}

resolution2160 = Question {questionText = "[3840 * 2160] resolution selected.", questionPointers = []}

startGamePrompt = Question {questionText = "Press [Enter] to start the game ...", questionPointers = []}

questionNodes = array (0, 22) [(0, rootQuestion), (1, mapQuestion), (2, saveQuestion), (3, controlKeyQuestion),
                               (4, resolutionQuestion), (5, pauseQuestion), (6, forwardQuestion), (7, backQuestion),
                               (8, strafeLeftQuestion), (9, strafeRightQuestion), (10, turnLeftQuestion), (11, turnRightQuestion),
                               (12, jumpQuestion), (13, lightTorchQuestion), (14, switchViewQuestion), (15, rotateViewQuestion),
                               (16, fireQuestion), (17, resolution768), (18, resolution1080), (19, resolution1440),
                               (20, resolution1800), (21, resolution2160), (22, startGamePrompt)]

mapNode :: [Char] -> [Char] -> [Char]
mapNode cmd_line answer = cmd_line ++ " --map_file " ++ answer ++ ".dan"

saveFileNode :: [Char] -> [Char] -> [Char]
saveFileNode cmd_line answer = cmd_line ++ " --current_save " ++ answer

pauseNode :: [Char] -> [Char] -> [Char]
pauseNode cmd_line answer = cmd_line ++ " --cb_PAUSE " ++ answer

forwardNode :: [Char] -> [Char] -> [Char]
forwardNode cmd_line answer = cmd_line ++ " --cb_FORWARD " ++ answer

backNode :: [Char] -> [Char] -> [Char]
backNode cmd_line answer = cmd_line ++ " --cb_BACK " ++ answer

strafeLeftNode :: [Char] -> [Char] -> [Char]
strafeLeftNode cmd_line answer = cmd_line ++ " --cb_STRAFE_LEFT " ++ answer

strafeRightNode :: [Char] -> [Char] -> [Char]
strafeRightNode cmd_line answer = cmd_line ++ " --cb_STRAFE_RIGHT " ++ answer

turnLeftNode :: [Char] -> [Char] -> [Char]
turnLeftNode cmd_line answer = cmd_line ++ " --cb_TURN_LEFT " ++ answer

turnRightNode :: [Char] -> [Char] -> [Char]
turnRightNode cmd_line answer = cmd_line ++ " --cb_TURN_RIGHT " ++ answer

jumpNode :: [Char] -> [Char] -> [Char]
jumpNode cmd_line answer = cmd_line ++ " --cb_JUMP " ++ answer

lightTorchNode :: [Char] -> [Char] -> [Char]
lightTorchNode cmd_line answer = cmd_line ++ " --cb_LIGHT_TORCH " ++ answer

switchViewNode :: [Char] -> [Char] -> [Char]
switchViewNode cmd_line answer = cmd_line ++ " --cb_SWITCH_VIEW " ++ answer

rotateViewNode :: [Char] -> [Char] -> [Char]
rotateViewNode cmd_line answer = cmd_line ++ " --cb_ROTATE VIEW " ++ answer

fireNode :: [Char] -> [Char] -> [Char]
fireNode cmd_line answer = cmd_line ++ " --cb_FIRE " ++ answer

resolution768Node :: [Char] -> [Char] -> [Char]
resolution768Node cmd_line answer = cmd_line ++ " --resolution_x 1024 --resolution_y 768"

resolution1080Node :: [Char] -> [Char] -> [Char]
resolution1080Node cmd_line answer = cmd_line ++ " --resolution_x 1920 --resolution_y 1080"

resolution1440Node :: [Char] -> [Char] -> [Char]
resolution1440Node cmd_line answer = cmd_line ++ " --resolution_x 2560 --resolution_y 1440"

resolution1800Node :: [Char] -> [Char] -> [Char]
resolution1800Node cmd_line answer = cmd_line ++ " --resolution_x 3200 --resolution_y 1800"

resolution2160Node :: [Char] -> [Char] -> [Char]
resolution2160Node cmd_line answer = cmd_line ++ " --resolution_x 3840 --resolution_y 2160"

nullNode :: [Char] -> [Char] -> [Char]
nullNode cmd_line answer = cmd_line

actionNodes = array (0, 22) [(0, nullNode), (1, mapNode), (2, saveFileNode), (3, nullNode),
                             (4, nullNode), (5, pauseNode), (6, forwardNode), (7, backNode), 
                             (8, strafeLeftNode), (9, strafeRightNode), (10, turnLeftNode), (11, turnRightNode), 
                             (12, jumpNode), (13, lightTorchNode), (14, switchViewNode), (15, rotateViewNode), 
                             (16, fireNode), (17, resolution768Node), (18, resolution1080Node), (19, resolution1440Node), 
                             (20, resolution1800Node), (21, resolution2160Node), (22, nullNode)]

identifyAnswer :: [([Char], Int)] -> [Char] -> Int
identifyAnswer [] answer = -1
identifyAnswer (x:xs) answer
  | fst x == answer = snd x
  | otherwise = identifyAnswer xs answer

cycleSaveIndex :: Int -> Int
cycleSaveIndex (-1) = 1
cycleSaveIndex 1 = 2
cycleSaveIndex 2 = 3
cycleSaveIndex 3 = 4
cycleSaveIndex 4 = 5
cycleSaveIndex 5 = 6
cycleSaveIndex 6 = 1

main = do
  args <- getArgs
  cfg_file <- bracket (openFile (args !! 0) ReadMode) (hClose)
                      (\h -> do c <- hGetContents h; putStr ("\ncfg file size: " ++ show (length c)); return c)
  putStr "\n\nThanks for choosing to play Game :: Dangerous!  This interactive launcher allows game engine options to be set before starting."
  startEngine (listArray (0, 97) (splitOneOf "=\n" (tailFile cfg_file)))

startEngine :: Array Int [Char] -> IO ()
startEngine conf_reg =
  let cfg' = cfg conf_reg 0
  in do
  cmdLine <- newIORef ""
  if (splitOn " " (cfg' "version_and_platform_string")) !! 0 == "Windows" then do
    cmd_line <- interactiveLoop questionNodes actionNodes "Game-Dangerous-exe config.txt"
    writeIORef cmdLine cmd_line
  else if (splitOn " " (cfg' "version_and_platform_string")) !! 0 == "Linux" then do
    cmd_line <- interactiveLoop questionNodes actionNodes "./Game-Dangerous-exe config.txt"
    writeIORef cmdLine cmd_line
  else do
    putStr ("\nUnsupported platform specified in configuration file.")
    exitSuccess
  cmd_line <- readIORef cmdLine
  putStr ("\nStarting engine with command line: " ++ cmd_line)
  (h_in, h_out, _, ph) <- createProcess (shell cmd_line) {std_in = CreatePipe, std_out = CreatePipe}
  next_map <- getSignalTrace (fromJust h_out) ph
  if next_map == 0 then return ()
  else putStr ("\nMap required: " ++ show next_map)

-- engineStartLoop :: Array Int [Char] -> [Char] -> Int -> Int -> Bool -> IO ()
-- engineStartLoop conf_reg cmd_line map_index save_index auto_start =
--   let cfg' = cfg conf_reg 0
--       auto_start_args = " --map_file " show map_index ++ ".dan" ++ " --current_save " ++ show save_index
--   in do
--   if auto_start then do
--     putStr ("\nStarting engine with command line: " ++ cmd_line ++ auto_start_args)
--     (h_in, h_out, _, ph) <- createProcess (shell (cmd_line ++ auto_start_args)) {std_in = CreatePipe, std_out = CreatePipe}
--     next_map <- getSignalTrace (fromJust h_out) ph
--     if next_map == 0 then return ()
--     else engineStartLoop conf_reg cmd_line next_map (cycleSaveIndex save_index) True
--   else if (splitOn " " (cfg' "version_and_platform_string")) !! 0 == "Windows" then do
--     cmd_line <- interactiveLoop questionNodes actionNodes "Game-Dangerous-exe config.txt"
--     putStr ("\nStarting engine with command line: " ++ cmd_line)
--   else if (splitOn " " (cfg' "version_and_platform_string")) !! 0 == "Linux" then do
--     putStr ("\nStarting engine with command line: " ++ "./Game-Dangerous-exe " ++ cfg' "config_file" ++ " " ++ arg0 ++ " " ++ arg1)
--     (h_in, h_out, _, ph) <- createProcess (shell ("./Game-Dangerous-exe " ++ cfg' "config_file" ++ " " ++ arg0 ++ " " ++ arg1)) {std_in = CreatePipe, std_out = CreatePipe}
--     next_map <- getSignalTrace (fromJust h_out) ph
--     if next_map == 0 then return ()
--     else do
--       putStr ("\nsave_index: " ++ show save_index)
--       startEngine conf_reg next_map (cycleSaveIndex save_index)
--   else
--     putStr ("\nUnsupported platform specified in configuration file.")

interactiveLoop :: Array Int Question -> Array Int ([Char] -> [Char] -> [Char]) -> [Char] -> IO [Char]
interactiveLoop questions actions cmd_line = do
  choice <- questionTree questionNodes False 0 ""
  if fst choice == -1 then do
    putStr "\nThe launcher can't understand the input.  Please refer to the README file for guidance."
    interactiveLoop questions actions cmd_line
  else if fst choice == 22 then return cmd_line
  else interactiveLoop questions actions (((actions ! (fst choice)) cmd_line) $ (snd choice))

questionTree :: Array Int Question -> Bool -> Int -> [Char] -> IO (Int, [Char])
questionTree nodes endNode i answer =
  let currentNode = nodes ! i
  in do
  if endNode then return (i, answer)
  else if i == -1 then return (-1, answer)
  else if questionPointers currentNode == [] then do
    putStrLn (questionText currentNode)
    hFlush stdout
    answer_ <- getLine
    questionTree nodes True i answer_
  else do
    putStrLn (questionText currentNode)
    hFlush stdout
    answer_ <- getLine
    questionTree nodes False (identifyAnswer (questionPointers currentNode) answer_) answer_

getSignalTrace :: Handle -> ProcessHandle -> IO Int
getSignalTrace h_out ph = do
  line <- hGetLine h_out
  exit_code <- getProcessExitCode ph
  if take 19 line == "Map file required: " then return (read [line !! 19])
  else if isJust exit_code then return 0
  else do
    putStrLn line
    getSignalTrace h_out ph

