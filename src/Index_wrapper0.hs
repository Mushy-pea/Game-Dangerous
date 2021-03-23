-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

module Index_wrapper0 where

import qualified Prelude as PREL

errString0 = "List index too large.  location: "
errString1 = " index: "
errString2 = " max: "

(ls, location) !! i =
  if i PREL.>= PREL.length ls then
    PREL.error (errString0 PREL.++ PREL.show location PREL.++ errString1 PREL.++ PREL.show i PREL.++ errString2 PREL.++ PREL.show ((PREL.length ls) PREL.- 1))
  else ls PREL.!! i

--(ls, location) !! i = ls PREL.!! i

-- The commenting out of the above two functions can be inverted to switch off the debugging provided by the index wrapper system
-- and save the runtime overhead.

-- current max location: 648

