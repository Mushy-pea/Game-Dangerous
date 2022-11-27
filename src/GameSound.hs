-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

-- This module implements the sound sub system.  Its functions are called from Main (for system initialisation) and Game_logic (for playing samples in game).
-- When trying to use the library Sound.OpenAL.AL I found that it didn't behave as the documentation on Hackage indicated.
-- GHC reported "No instance for GeneratableObjectName Buffer" despite this instance being declared in the relevant module.
-- An equivalent error occured for GeneratableObjectName Source.  As a work around for this version I have written my own functions
-- for generating buffer and source names, which required overiding the library's module structure in places to bring certain things
-- in hidden modules into scope.  This is why the AL_buffer module is required.

module GameSound where

import System.IO
import Foreign
import Foreign.C.Types
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
import AlBuffer

-- See notes at the top of the module.
foreign import ccall unsafe "alGenSources"
   alGenSources :: Sound.OpenAL.AL.BasicTypes.ALsizei -> Ptr Sound.OpenAL.AL.BasicTypes.ALuint -> IO ()

foreign import ccall unsafe "alDeleteSources"
   alDeleteSources :: Sound.OpenAL.AL.BasicTypes.ALsizei -> Ptr GameSound.Source -> IO ()

foreign import ccall unsafe "alIsSource"
   alIsSource :: GameSound.Source -> IO Sound.OpenAL.AL.BasicTypes.ALboolean

newtype Source = Source Sound.OpenAL.AL.BasicTypes.ALuint
   deriving ( Eq, Ord, Show )

instance Storable GameSound.Source where
   sizeOf    ~(Source b) = sizeOf b
   alignment ~(Source b) = alignment b
   peek                  = peek1 Source . castPtr
   poke ptr   (Source b) = poke1 (castPtr ptr) b

-- Initialise the OpenAL context.
initAlContext :: IO ()
initAlContext = do
  def <- get defaultDeviceSpecifier
  putStr ("\nAudio device: " ++ fromJust def)
  audio_dev <- openDevice def
  if isNothing audio_dev == True then putStr "\nWarning: Failed to open an audio device."
  else return ()
  context <- createContext (fromJust audio_dev) []
  if isNothing context == True then putStr "\nWarning: Failed to create an OpenAL context."
  else return ()
  currentContext $= context

-- Generate and link the required set of OpenAL source and buffer objects.
initAlEffect0 :: [[Char]] -> [Char] -> Array Int GameSound.Source -> IO (Array Int GameSound.Source)
initAlEffect0 sample_list path src_array = do
  buf <- genBuffer0 0 (div (length sample_list) 2) []
  src <- genSource0 0 (div (length sample_list) 2) []
  src_array_ <- initAlEffect1 sample_list buf src path src_array
  return src_array_

initAlEffect1 :: [[Char]] -> [Buffer] -> [GameSound.Source] -> [Char] -> Array Int GameSound.Source -> IO (Array Int GameSound.Source)
initAlEffect1 [] [] [] path src_array = return src_array
initAlEffect1 (x0:x1:xs) (y:ys) (z:zs) path src_array = do
  sample_data <- loadSndBuf0 [x1] path []
  loadSndBuf1 [y] sample_data
  linkSource [z] [y]
  initAlEffect1 xs ys zs path (src_array // [(read x0, z)])

-- Custom source generation functions (bug fix).
genSource1 :: IO GameSound.Source
genSource1 = do
  p_src <- mallocBytes 4
  alGenSources 1 p_src
  src <- peek p_src
  free p_src
  return (Source src)

genSource0 :: Int -> Int -> [GameSound.Source] -> IO [GameSound.Source]
genSource0 c limit acc = do
  if c == limit then return acc
  else do
    src <- genSource1
    genSource0 (c + 1) limit (acc ++ [src])

-- These four functions deal with loading sound samples from WAV files into OpenAL buffers and linking buffers to sources.
loadSndBuf0 :: [[Char]] -> [Char] -> [(BS.ByteString, Int, Bool)] -> IO [(BS.ByteString, Int, Bool)]
loadSndBuf0 [] path acc = return acc
loadSndBuf0 (x:xs) path acc = do
  waveFile <- BS.readFile (path ++ x)
  if head x == '_' then loadSndBuf0 xs path (acc ++ [(waveFile, BS.length waveFile, True)])
  else loadSndBuf0 xs path (acc ++ [(waveFile, BS.length waveFile, False)])

loadSndBuf1 :: [Buffer] -> [(BS.ByteString, Int, Bool)] -> IO ()
loadSndBuf1 [] [] = return ()
loadSndBuf1 (x:xs) ((bs, len, mode):ys) = do
  BS.useAsCString (BS.drop 44 bs) (loadSndBuf2 mode x len)
  loadSndBuf1 xs ys
  
loadSndBuf2 :: Bool -> Buffer -> Int -> Ptr CChar -> IO ()
loadSndBuf2 mode buf len p_waveFile =
  if mode == True then (bufferData buf) $= (BufferData (MemoryRegion p_waveFile (fromIntegral (len - 44))) Stereo16 32000)
  else (bufferData buf) $= (BufferData (MemoryRegion p_waveFile (fromIntegral (len - 44))) Stereo16 44100)

linkSource :: [GameSound.Source] -> [Buffer] -> IO ()
linkSource [] [] = return ()
linkSource (x:xs) (y:ys) = do
  queueBuffers (unsafeCoerce x) (unsafeCoerce [y])
  (sourcePosition (unsafeCoerce x)) $= (Vertex3 0 0 1)
  linkSource xs ys

play_ :: GameSound.Source -> IO ()
play_ src = play [unsafeCoerce src]

