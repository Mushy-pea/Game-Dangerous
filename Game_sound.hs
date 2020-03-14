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
  sample_data <- load_snd_buf0 [x1] path []
  load_snd_buf1 [y] sample_data
  link_source [z] [y]
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
load_snd_buf0 :: [[Char]] -> [Char] -> [(WAVESamples, Int, Bool)] -> IO [(WAVESamples, Int, Bool)]
load_snd_buf0 [] path acc = return acc
load_snd_buf0 (x:xs) path acc = do
  wave_data <- getWAVEFile (path ++ x)
  if head x == '_' then load_snd_buf0 xs path (acc ++ [(waveSamples wave_data, length (waveSamples wave_data), True)])
  else load_snd_buf0 xs path (acc ++ [(waveSamples wave_data, length (waveSamples wave_data), False)])

load_snd_buf1 :: [Buffer] -> [(WAVESamples, Int, Bool)] -> IO ()
load_snd_buf1 [] [] = return ()
load_snd_buf1 (x:xs) (y:ys) = do
  p_buf <- mallocBytes (4 * snd__ y)
  load_snd_buf2 (fst__ y) p_buf 0
  if third_ y == True then (bufferData x) $= (BufferData (MemoryRegion p_buf (fromIntegral (4 * snd__ y))) Stereo16 32000)
  else (bufferData x) $= (BufferData (MemoryRegion p_buf (fromIntegral (4 * snd__ y))) Stereo16 44100)
  free p_buf
  load_snd_buf1 xs ys

load_snd_buf2 :: WAVESamples -> Ptr a -> Int -> IO ()
load_snd_buf2 [] p_buf i = return ()
load_snd_buf2 (x:xs) p_buf i = do
  poke (plusPtr p_buf i) (fromIntegral (div ((x, 608) !! 0) 65536) :: Int16)
  poke (plusPtr p_buf (i + 2)) (fromIntegral (div ((x, 609) !! 1) 65536) :: Int16)
  load_snd_buf2 xs p_buf (i + 4)

link_source :: [Game_sound.Source] -> [Buffer] -> IO ()
link_source [] [] = return ()
link_source (x:xs) (y:ys) = do
  queueBuffers (unsafeCoerce x) (unsafeCoerce [y])
  (sourcePosition (unsafeCoerce x)) $= (Vertex3 0 0 1)
  link_source xs ys

play_ :: Game_sound.Source -> IO ()
play_ src = play [unsafeCoerce src]
