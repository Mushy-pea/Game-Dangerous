-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module Index_wrapper1 where

import qualified Prelude as PREL

-- These types are used within the GPLC bytecode interpreter and their names correspond to the GPLC types they are used to represent.
newtype GPLC_int = GPLC_int PREL.Int

newtype GPLC_flag = GPLC_flag PREL.Int

newtype GPLC_float = GPLC_float PREL.Int

newtype GPLC_general = GPLC_general PREL.Int

errString0 = "List index too large.  location: "
errString1 = " index: "
errString2 = " max: "

-- This instances of this class are a wrapper for the list index (!!) function, which shows where an index too large exception has happened.
class Index b where
  (!!) :: ([a], PREL.Int) -> b -> a
  
instance Index PREL.Int where
  (ls, location) !! i =
    if i PREL.>= PREL.length ls then
      PREL.error (errString0 PREL.++ PREL.show location PREL.++ errString1 PREL.++ PREL.show i PREL.++ errString2 PREL.++ PREL.show ((PREL.length ls) PREL.- 1))
    else ls PREL.!! i

instance Index GPLC_int where
  (ls, location) !! (GPLC_int i) =
    if i PREL.>= PREL.length ls then
      PREL.error (errString0 PREL.++ PREL.show location PREL.++ errString1 PREL.++ PREL.show i PREL.++ errString2 PREL.++ PREL.show ((PREL.length ls) PREL.- 1))
    else ls PREL.!! i

instance Index GPLC_flag where
  (ls, location) !! (GPLC_flag i) =
    if i PREL.>= PREL.length ls then
      PREL.error (errString0 PREL.++ PREL.show location PREL.++ errString1 PREL.++ PREL.show i PREL.++ errString2 PREL.++ PREL.show ((PREL.length ls) PREL.- 1))
    else ls PREL.!! i

instance Index GPLC_float where
  (ls, location) !! (GPLC_float i) =
    if i PREL.>= PREL.length ls then
      PREL.error (errString0 PREL.++ PREL.show location PREL.++ errString1 PREL.++ PREL.show i PREL.++ errString2 PREL.++ PREL.show ((PREL.length ls) PREL.- 1))
    else ls PREL.!! i

instance Index GPLC_general where
  (ls, location) !! (GPLC_general i) =
    if i PREL.>= PREL.length ls then
      PREL.error (errString0 PREL.++ PREL.show location PREL.++ errString1 PREL.++ PREL.show i PREL.++ errString2 PREL.++ PREL.show ((PREL.length ls) PREL.- 1))
    else ls PREL.!! i

--(ls, location) !! i = ls PREL.!! i

-- The commenting out of the above two functions can be inverted to switch off the debugging provided by the index wrapper system
-- and save the runtime overhead.

-- current max location: 649


