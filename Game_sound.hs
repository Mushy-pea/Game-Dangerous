-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This module implements the sound sub system.  Its functions are called from Main (for system initialisation) and Game_logic (for playing samples in game).
-- When trying to use the library Sound.OpenAL.AL I found that it didn't behave as the documentation on Hackage indicated.
-- GHC reported "No instance for GeneratableObjectName Buffer" despite this instance being declared in the relevant module.
-- An equivalent error occured for GeneratableObjectName Source.  As a work around for this version I have written my own functions
-- for generating buffer and source names, which required overiding the library's module structure in places to bring certain things
-- in hidden modules into scope.  This is why the AL_buffer module is required.

module Game_sound where

import Prelude hiding ((!!))
import Index_wrapper
import System.IO
import Foreign
import Foreign.C.Types
import Wave
import Data.StateVar
import Data.Maybe
import Data.Array.IArray
import qualified Data.ByteString as BS
import Graphics.Rendering.OpenGL.GL.Tensor
import Sound.OpenAL.AL.BasicTypes
import Sound.OpenAL.AL.Listener
import Sound.OpenAL.AL.StringQueries
import Sound.OpenAL.AL.Source
import Sound.OpenAL.ALC
import Unsafe.Coerce
import Build_model
import AL_buffer

-- See notes at the top of the module.
foreign import ccall unsafe "alGenSources"
   alGenSources :: Sound.OpenAL.AL.BasicTypes.ALsizei -> Ptr Sound.OpenAL.AL.BasicTypes.ALuint -> IO ()

foreign import ccall unsafe "alDeleteSources"
   alDeleteSources :: Sound.OpenAL.AL.BasicTypes.ALsizei -> Ptr Game_sound.Source -> IO ()

foreign import ccall unsafe "alIsSource"
   alIsSource :: Game_sound.Source -> IO Sound.OpenAL.AL.BasicTypes.ALboolean

newtype Source = Source Sound.OpenAL.AL.BasicTypes.ALuint
   deriving ( Eq, Ord, Show )

instance Storable Game_sound.Source where
   sizeOf    ~(Source b) = sizeOf b
   alignment ~(Source b) = alignment b
   peek                  = peek1 Source . castPtr
   poke ptr   (Source b) = poke1 (castPtr ptr) b

-- Initialise the OpenAL context.
init_al_context :: IO ()
init_al_context = do
  def <- get defaultDeviceSpecifier
  audio_dev <- openDevice def
  if isNothing audio_dev == True then error "\nFailed to initialise an OpenAL context..."
  else return ()
  context <- createContext (fromJust audio_dev) []
  currentContext $= context

-- Generate and link the required set of OpenAL source and buffer objects.
init_al_effect0 :: [[Char]] -> [Char] -> Array Int Game_sound.Source -> IO (Array Int Game_sound.Source)
init_al_effect0 sample_list path src_array = do
  buf <- gen_buffer0 0 (div (length sample_list) 2) []
  src <- gen_source0 0 (div (length sample_list) 2) []
  src_array_ <- init_al_effect1 sample_list buf src path src_array
  return src_array_

init_al_effect1 :: [[Char]] -> [Buffer] -> [Game_sound.Source] -> [Char] -> Array Int Game_sound.Source -> IO (Array Int Game_sound.Source)
init_al_effect1 [] [] [] path src_array = return src_array
init_al_effect1 (x0:x1:xs) (y:ys) (z:zs) path src_array = do
  sample_data <- load_snd_buf0 x1 path
  load_snd_buf1 y sample_data
  link_source z y
  init_al_effect1 xs ys zs path (src_array // [(read x0, z)])

-- Custom source generation functions (bug fix).
gen_source1 :: IO Game_sound.Source
gen_source1 = do
  p_src <- mallocBytes 4
  alGenSources 1 p_src
  src <- peek p_src
  free p_src
  return (Source src)

gen_source0 :: Int -> Int -> [Game_sound.Source] -> IO [Game_sound.Source]
gen_source0 c limit acc = do
  if c == limit then return acc
  else do
    src <- gen_source1
    gen_source0 (c + 1) limit (acc ++ [src])

-- These four functions deal with loading sound samples from WAV files into OpenAL buffers and linking buffers to sources.
load_snd_buf0 :: [Char] -> [Char] -> IO (BS.ByteString, Int, Bool)
load_snd_buf0 file path = do
  wave_file <- BS.readFile (path ++ file)
  if head file == '_' then return (wave_file, BS.length wave_file, True)
  else return (wave_file, BS.length wave_file, False)

load_snd_buf1 :: Buffer -> (BS.ByteString, Int, Bool) -> IO ()
load_snd_buf1 buf (bs, len, mode) = do
  BS.useAsCString (BS.drop 44 bs) (load_snd_buf2 mode buf len)

load_snd_buf2 :: Bool -> Buffer -> Int -> Ptr CChar -> IO ()
load_snd_buf2 mode buf len p_waveFile =
  if mode == True then (bufferData buf) $= (BufferData (MemoryRegion p_waveFile (fromIntegral len)) Stereo16 32000)
  else (bufferData buf) $= (BufferData (MemoryRegion p_waveFile (fromIntegral len)) Stereo16 44100)

link_source :: Game_sound.Source -> Buffer -> IO ()
link_source src buf = do
  queueBuffers (unsafeCoerce src) (unsafeCoerce buf)
  (sourcePosition (unsafeCoerce src)) $= (Vertex3 0 0 1)

play_ :: Game_sound.Source -> IO ()
play_ src = play [unsafeCoerce src]
