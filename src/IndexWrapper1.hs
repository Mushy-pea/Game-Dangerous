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

-- This class acts as an interface to the listIndexWrapper and arrayBoundsCheck functions.  These functions provide debugging information for list index (!!)
-- exceptions and bounds checking for array read / write operations, respectively.
class Index b where
  (!!) :: [a] -> b -> a

  boundsCheck :: Array (PREL.Int, PREL.Int, PREL.Int) a -> (b, b, b) -> [PREL.Char] -> Maybe [PREL.Char]

instance Index PREL.Int where
  ls !! i = ls PREL.!! i
  
  boundsCheck arr (w, u, v) function = arrayBoundsCheck arr (w, u, v) function PREL.False

instance Index GPLC_int where
  ls !! (GPLC_int i) = ls PREL.!! i
  
  boundsCheck arr (GPLC_int w, GPLC_int u, GPLC_int v) function = arrayBoundsCheck arr (w, u, v) function PREL.True

instance Index GPLC_flag where
  ls !! (GPLC_flag i) = ls PREL.!! i

instance Index GPLC_float where
  ls !! (GPLC_float i) = ls PREL.!! i
  
instance Index GPLC_general where
  ls !! (GPLC_general i) = ls PREL.!! i

arrayBoundsCheck :: Array (PREL.Int, PREL.Int, PREL.Int) a -> (PREL.Int, PREL.Int, PREL.Int) -> [PREL.Char] -> PREL.Bool -> Maybe [PREL.Char]
arrayBoundsCheck arr (w, u, v) function context_GPLC
  | w PREL.< fst__ (PREL.fst bd) PREL.|| w PREL.> fst__ (PREL.snd bd) PREL.|| u PREL.< snd__ (PREL.fst bd) PREL.||
    u PREL.> snd__ (PREL.snd bd) PREL.|| v PREL.< third_ (PREL.fst bd) PREL.|| v PREL.> third_ (PREL.snd bd) =
    if context_GPLC then
      Just ("Array index out of bounds in GPLC opcode " PREL.++ function PREL.++ ".  array bounds: " PREL.++ PREL.show bd PREL.++
            " index: " PREL.++ PREL.show (w, u, v))
    else
      Just ("Array index out of bounds in function " PREL.++ function PREL.++ ".  array bounds: " PREL.++ PREL.show bd PREL.++
            " index: " PREL.++ PREL.show (w, u, v))
  | PREL.otherwise = Nothing
  where bd = bounds arr

