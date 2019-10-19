-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module Index_wrapper where

import qualified Prelude as PREL

-- This function is a wrapper for the list index (!!) function, which shows where an index too large exception has happened.
(!!) :: ([a], PREL.Int) -> PREL.Int -> a
(ls, location) !! i =
  if i PREL.>= PREL.length ls then PREL.error ("List index too large.  location: " PREL.++ PREL.show location PREL.++ " index: " PREL.++ PREL.show i PREL.++ " max: " PREL.++ PREL.show ((PREL.length ls) PREL.- 1))
  else ls PREL.!! i

-- The commenting out of these two functions can be inverted to switch off the debugging provided by the index wrapper system and save most of the runtime overhead.
--(ls, location) !! i = ls PREL.!! i
