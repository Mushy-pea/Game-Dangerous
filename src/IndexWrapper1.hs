-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module IndexWrapper1 where

import qualified Prelude as PREL
import Data.Array.IArray
import Data.Maybe
import BuildModel

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

  boundsCheck :: Array (PREL.Int, PREL.Int, PREL.Int) a -> (b, b, b) -> [PREL.Char] -> Maybe [PREL.Char]

instance Index PREL.Int where
  (ls, location) !! i = listIndexWrapper ls location i
  
  boundsCheck arr (w, u, v) function = arrayIndexWrapper arr (w, u, v) function

instance Index GPLC_int where
  (ls, location) !! (GPLC_int i) = listIndexWrapper ls location i
  
  boundsCheck arr (GPLC_int w, GPLC_int u, GPLC_int v) function = arrayIndexWrapper arr (w, u, v) function

listIndexWrapper :: [a] -> PREL.Int -> PREL.Int -> a
listIndexWrapper ls location i
  | i PREL.>= PREL.length ls =
    PREL.error (errString0 PREL.++ PREL.show location PREL.++ errString1 PREL.++ PREL.show i PREL.++ errString2 PREL.++ PREL.show ((PREL.length ls) PREL.- 1))
  | PREL.otherwise = ls PREL.!! i

arrayIndexWrapper :: Array (PREL.Int, PREL.Int, PREL.Int) a -> (PREL.Int, PREL.Int, PREL.Int) -> [PREL.Char] -> Maybe [PREL.Char]
arrayIndexWrapper arr (w, u, v) function
  | w PREL.< fst__ (PREL.fst bd) PREL.|| w PREL.> fst__ (PREL.snd bd) PREL.|| u PREL.< snd__ (PREL.fst bd) PREL.||
    u PREL.> snd__ (PREL.snd bd) PREL.|| v PREL.< third_ (PREL.fst bd) PREL.|| v PREL.> third_ (PREL.snd bd) =
    Just ("Array index out of bounds in function " PREL.++ function PREL.++ ".  array bounds: " PREL.++ PREL.show bd PREL.++
          " index: " PREL.++ PREL.show (w, u, v))
  | PREL.otherwise = Nothing
  where bd = bounds arr

-- current max location: 654


