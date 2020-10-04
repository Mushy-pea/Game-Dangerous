-- See comments in Game_sound

module AL_buffer where

import Foreign.C.Types
import Data.StateVar ( StateVar, makeStateVar )
import Foreign.Marshal.Alloc ( alloca, mallocBytes, free )
import Foreign.Ptr ( Ptr, nullPtr, castPtr )
import qualified Sound.OpenAL.ALC.Context as ALC

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.ObjectName ( ObjectName(..), GeneratableObjectName(..) )
import Foreign.Marshal.Array ( withArrayLen, peekArray, allocaArray )
import Foreign.Storable ( Storable(..) )

data MemoryRegion a = MemoryRegion (Ptr a) ALsizei
   deriving ( Eq, Ord, Show )

data BufferData a = BufferData (MemoryRegion a) Format ALC.Frequency
   deriving ( Eq, Ord, Show )

bufferData :: Buffer -> StateVar (BufferData a)
bufferData buffer = makeStateVar (getBufferData buffer) (setBufferData buffer)

getBufferData :: Buffer -> IO (BufferData a)
getBufferData buffer = do
   channels <- getBufferi buffer Channels
   bits <- getBufferi buffer Bits
   raw <- return nullPtr -- ToDo: AL_DATA query missing!!!
   size <- getBufferi buffer Size
   frequency <- getBufferi buffer Frequency
   return $ BufferData (MemoryRegion  raw size) (makeFormat channels bits) frequency

setBufferData :: Buffer -> BufferData a -> IO ()
setBufferData buffer (BufferData (MemoryRegion raw size) format frequency) =
      alBufferData buffer (marshalFormat format) raw size (round frequency)

foreign import ccall unsafe "alBufferData"
   alBufferData :: Buffer -> ALenum -> Ptr a -> ALsizei -> ALsizei -> IO ()

--------------------------------------------------------------------------------

-- ToDo: What about IMAADPCMMono16, IMAADPCMStereo16, Vorbis...?
makeFormat :: ALint -> ALint -> Format
makeFormat 1  8 = Mono8
makeFormat 2  8 = Stereo8
makeFormat 1 16 = Mono16
makeFormat 2 16 = Stereo16
makeFormat channels bits =
   error ("makeFormat: illegal values " ++ show (channels, bits))

--------------------------------------------------------------------------------

data BufferQuery =
     Frequency
   | Size
   | Bits
   | Channels

marshalBufferQuery :: BufferQuery -> ALenum
marshalBufferQuery x = case x of
   Frequency -> al_FREQUENCY
   Size -> al_SIZE
   Bits -> al_BITS
   Channels -> al_CHANNELS

--------------------------------------------------------------------------------

getBufferi :: Num a => Buffer -> BufferQuery -> IO a
getBufferi buffer query =
   alloca $ \buf -> do
      alGetBufferi buffer (marshalBufferQuery query) buf
      peek1 fromIntegral buf

foreign import ccall unsafe "alGetBufferi"
   alGetBufferi :: Buffer -> ALenum -> Ptr ALint -> IO ()

--------------------------------------------------------------------------------

newtype Buffer = Buffer { bufferID :: ALuint }
   deriving ( Eq, Ord, Show )
 
-- | A dummy buffer.

nullBuffer :: Buffer
nullBuffer = Buffer 0

marshalBuffer :: Maybe Buffer -> ALuint
marshalBuffer = bufferID . maybe nullBuffer id

unmarshalBuffer :: ALuint -> Maybe Buffer
unmarshalBuffer b =
   if b == bufferID nullBuffer then Nothing else Just (Buffer b)

instance Storable Buffer where
   sizeOf    ~(Buffer b) = sizeOf b
   alignment ~(Buffer b) = alignment b
   peek                  = peek1 Buffer . castPtr
   poke ptr   (Buffer b) = poke1 (castPtr ptr) b

--------------------------------------------------------------------------------

foreign import ccall unsafe "alGenBuffers"
   alGenBuffers :: ALsizei -> Ptr ALuint -> IO ()

foreign import ccall unsafe "alDeleteBuffers"
   alDeleteBuffers :: ALsizei -> Ptr Buffer -> IO ()

foreign import ccall unsafe "alIsBuffer"
   alIsBuffer :: Buffer -> IO ALboolean

-- Custom buffer generation functions (bug fix)
gen_buffer1 :: IO Buffer
gen_buffer1 = do
  p_buf <- mallocBytes 4
  alGenBuffers 1 p_buf
  buf <- peek p_buf
  free p_buf
  return Buffer {bufferID = buf}

gen_buffer0 :: Int -> Int -> [Buffer] -> IO [Buffer]
gen_buffer0 c limit acc = do
  if c == limit then return acc
  else do
    buf <- gen_buffer1
    gen_buffer0 (c + 1) limit (acc ++ [buf])

-----------------------------------------------------------

data Format =
     Mono8
   | Mono16
   | Stereo8
   | Stereo16
   deriving ( Eq, Ord, Show )

marshalFormat :: Format -> ALenum
marshalFormat x = case x of
   Mono8 -> al_FORMAT_MONO8
   Mono16 -> al_FORMAT_MONO16
   Stereo8 -> al_FORMAT_STEREO8
   Stereo16 -> al_FORMAT_STEREO16

unmarshalFormat :: ALenum -> Format
unmarshalFormat x
   | x == al_FORMAT_MONO8 = Mono8
   | x == al_FORMAT_MONO16 = Mono16
   | x == al_FORMAT_STEREO8 = Stereo8
   | x == al_FORMAT_STEREO16 = Stereo16
   | otherwise = error ("unmarshalFormat: illegal value " ++ show x)

-------------------------------------------------------------

{-# INLINE poke1 #-}
poke1 :: Storable a => Ptr a -> a -> IO ()
poke1 ptr x =
   pokeElemOff ptr 0 x

--------------------------------------------------------------------------------

{-# INLINE peek1 #-}
peek1 :: Storable a => (a -> b) -> Ptr a -> IO b
peek1 f ptr = do
   x <- peekElemOff ptr 0
   return $ f x

{-# INLINE peek3 #-}
peek3 :: Storable a => (a -> a -> a -> b) -> Ptr a -> IO b
peek3 f ptr = do
   x <- peekElemOff ptr 0
   y <- peekElemOff ptr 1
   z <- peekElemOff ptr 2
   return $ f x y z

{-# INLINE peek6 #-}
peek6 :: Storable a => (a -> a -> a -> b) -> Ptr a -> IO (b, b)
peek6 f ptr = do
   x <- peekElemOff ptr 0
   y <- peekElemOff ptr 1
   z <- peekElemOff ptr 2
   u <- peekElemOff ptr 3
   v <- peekElemOff ptr 4
   w <- peekElemOff ptr 5
   return $ (f x y z, f u v w)

-------------------------------------------------------------------------------

al_FALSE, al_TRUE :: ALboolean
al_FALSE                            = 0x0000
al_TRUE                             = 0x0001

al_NO_ERROR, al_INVALID_NAME, al_INVALID_ENUM, al_INVALID_VALUE,
   al_INVALID_OPERATION, al_OUT_OF_MEMORY :: ALenum
al_NO_ERROR                         = 0x0000
al_INVALID_NAME                     = 0xA001
al_INVALID_ENUM                     = 0xA002
al_INVALID_VALUE                    = 0xA003
al_INVALID_OPERATION                = 0xA004
al_OUT_OF_MEMORY                    = 0xA005

--------------------------------------------------------------------------------

al_DISTANCE_MODEL, al_DOPPLER_FACTOR, al_SPEED_OF_SOUND :: ALenum
al_DISTANCE_MODEL                   = 0xD000
al_DOPPLER_FACTOR                   = 0xC000
al_SPEED_OF_SOUND                   = 0xC003

al_VERSION, al_RENDERER, al_VENDOR, al_EXTENSIONS :: ALenum
al_VERSION                          = 0xB002
al_RENDERER                         = 0xB003
al_VENDOR                           = 0xB001
al_EXTENSIONS                       = 0xB004

al_NONE, al_INVERSE_DISTANCE, al_INVERSE_DISTANCE_CLAMPED, al_LINEAR_DISTANCE,
   al_LINEAR_DISTANCE_CLAMPED, al_EXPONENT_DISTANCE,
   al_EXPONENT_DISTANCE_CLAMPED :: ALenum
al_NONE                             = 0x0000
al_INVERSE_DISTANCE                 = 0xD001
al_INVERSE_DISTANCE_CLAMPED         = 0xD002
al_LINEAR_DISTANCE                  = 0xD003
al_LINEAR_DISTANCE_CLAMPED          = 0xD004
al_EXPONENT_DISTANCE                = 0xD005
al_EXPONENT_DISTANCE_CLAMPED        = 0xD006

--------------------------------------------------------------------------------

al_POSITION, al_VELOCITY, al_GAIN :: ALenum
al_POSITION                         = 0x1004
al_VELOCITY                         = 0x1006
al_GAIN                             = 0x100A

al_ORIENTATION :: ALenum
al_ORIENTATION                      = 0x100F

al_SOURCE_RELATIVE, al_SOURCE_TYPE, al_LOOPING, al_BUFFER, al_BUFFERS_QUEUED,
   al_BUFFERS_PROCESSED, al_MIN_GAIN, al_MAX_GAIN, al_REFERENCE_DISTANCE,
   al_ROLLOFF_FACTOR, al_MAX_DISTANCE, al_PITCH, al_DIRECTION,
   al_CONE_INNER_ANGLE, al_CONE_OUTER_ANGLE, al_CONE_OUTER_GAIN, al_SEC_OFFSET,
   al_SAMPLE_OFFSET, al_BYTE_OFFSET, al_SOURCE_STATE :: ALenum
al_SOURCE_RELATIVE                  = 0x0202
al_SOURCE_TYPE                      = 0x1027
al_LOOPING                          = 0x1007
al_BUFFER                           = 0x1009
al_BUFFERS_QUEUED                   = 0x1015
al_BUFFERS_PROCESSED                = 0x1016
al_MIN_GAIN                         = 0x100D
al_MAX_GAIN                         = 0x100E
al_REFERENCE_DISTANCE               = 0x1020
al_ROLLOFF_FACTOR                   = 0x1021
al_MAX_DISTANCE                     = 0x1023
al_PITCH                            = 0x1003
al_DIRECTION                        = 0x1005
al_CONE_INNER_ANGLE                 = 0x1001
al_CONE_OUTER_ANGLE                 = 0x1002
al_CONE_OUTER_GAIN                  = 0x1022
al_SEC_OFFSET                       = 0x1024
al_SAMPLE_OFFSET                    = 0x1025
al_BYTE_OFFSET                      = 0x1026
al_SOURCE_STATE                     = 0x1010

al_UNDETERMINED, al_STATIC, al_STREAMING :: ALint
al_UNDETERMINED                     = 0x1030
al_STATIC                           = 0x1028
al_STREAMING                        = 0x1029

al_INITIAL, al_PLAYING, al_PAUSED, al_STOPPED :: ALint
al_INITIAL                          = 0x1011
al_PLAYING                          = 0x1012
al_PAUSED                           = 0x1013
al_STOPPED                          = 0x1014

--------------------------------------------------------------------------------

al_FREQUENCY, al_SIZE, al_BITS, al_CHANNELS :: ALenum
al_FREQUENCY                        = 0x2001
al_SIZE                             = 0x2004
al_BITS                             = 0x2002
al_CHANNELS                         = 0x2003

al_FORMAT_MONO8, al_FORMAT_MONO16, al_FORMAT_STEREO8,
   al_FORMAT_STEREO16 :: ALenum
al_FORMAT_MONO8                     = 0x1100
al_FORMAT_MONO16                    = 0x1101
al_FORMAT_STEREO8                   = 0x1102
al_FORMAT_STEREO16                  = 0x1103

--------------------------------------------------------------------------------

alc_FALSE, alc_TRUE :: ALCboolean
alc_FALSE                           = 0x0000
alc_TRUE                            = 0x0001

alc_FREQUENCY, alc_REFRESH, alc_SYNC, alc_MONO_SOURCES,
   alc_STEREO_SOURCES :: ALCint
alc_FREQUENCY                       = 0x1007
alc_REFRESH                         = 0x1008
alc_SYNC                            = 0x1009
alc_MONO_SOURCES                    = 0x1010
alc_STEREO_SOURCES                  = 0x1011

alc_NO_ERROR, alc_INVALID_DEVICE, alc_INVALID_CONTEXT, alc_INVALID_ENUM,
   alc_INVALID_VALUE, alc_INVALID_OPERATION, alc_OUT_OF_MEMORY :: ALCenum
alc_NO_ERROR                        = 0x0000
alc_INVALID_DEVICE                  = 0xA001
alc_INVALID_CONTEXT                 = 0xA002
alc_INVALID_ENUM                    = 0xA003
alc_INVALID_VALUE                   = 0xA004
alc_INVALID_OPERATION               = 0xA006
alc_OUT_OF_MEMORY                   = 0xA005

alc_DEFAULT_DEVICE_SPECIFIER, alc_DEVICE_SPECIFIER, alc_EXTENSIONS,
   alc_CAPTURE_DEFAULT_DEVICE_SPECIFIER, alc_CAPTURE_DEVICE_SPECIFIER :: ALCenum
alc_DEFAULT_DEVICE_SPECIFIER        = 0x1004
alc_DEVICE_SPECIFIER                = 0x1005
alc_EXTENSIONS                      = 0x1006
alc_CAPTURE_DEFAULT_DEVICE_SPECIFIER= 0x0311
alc_CAPTURE_DEVICE_SPECIFIER        = 0x0310

alc_ATTRIBUTES_SIZE, alc_ALL_ATTRIBUTES, alc_MAJOR_VERSION, alc_MINOR_VERSION,
   alc_CAPTURE_SAMPLES :: ALCenum
alc_ATTRIBUTES_SIZE                 = 0x1002
alc_ALL_ATTRIBUTES                  = 0x1003
alc_MAJOR_VERSION                   = 0x1000
alc_MINOR_VERSION                   = 0x1001
alc_CAPTURE_SAMPLES                 = 0x0312

---------------------------------------------------------------------------------

-- AL types

-- | 8-bit boolean
type ALboolean = CChar

-- | Character
type ALchar = CChar

-- | Signed 8-bit 2\'s complement integer
type ALbyte = CSChar

-- | Unsigned 8-bit integer
type ALubyte = CUChar

-- | Signed 16-bit 2\'s complement integer
type ALshort = CShort

-- | Unsigned 16-bit integer
type ALushort = CUShort

-- | Signed 32-bit 2\'s complement integer
type ALint = CInt

-- | Unsigned 32-bit integer
type ALuint = CUInt

-- | Non-negatitve 32-bit binary integer size
type ALsizei = CInt

-- | Enumerated 32-bit value
type ALenum = CInt

-- | 32-bit IEEE754 floating-point
type ALfloat = CFloat

-- | 64-bit IEEE754 floating-point
type ALdouble = CDouble

--------------------------------------------------------------------------------
-- ALC types

-- | 8-bit boolean
type ALCboolean = CChar

-- | Character
type ALCchar = CChar

-- | Signed 8-bit 2\'s complement integer
type ALCbyte = CSChar

-- | Unsigned 8-bit integer
type ALCubyte = CUChar

-- | Signed 16-bit 2\'s complement integer
type ALCshort = CShort

-- | Unsigned 16-bit integer
type ALCushort = CUShort

-- | Signed 32-bit 2\'s complement integer
type ALCint = CInt

-- | Unsigned 32-bit integer
type ALCuint = CUInt

-- | Non-negatitve 32-bit binary integer size
type ALCsizei = CInt

-- | Enumerated 32-bit value
type ALCenum = CInt

-- | 32-bit IEEE754 floating-point
type ALCfloat = CFloat

-- | 64-bit IEEE754 floating-point
type ALCdouble = CDouble

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcCloseDevice() returns an ALCboolean, before it was void.
-- To break a dependency cycle, we have to define the Device type here, too.

-- | The abstract device type.

newtype Device = Device ALCdevice
   deriving ( Eq, Ord, Show )

newtype ALCdevice = ALCdevice (Ptr ALCdevice)
   deriving ( Eq, Ord, Show )

nullDevice :: Device
nullDevice = Device (ALCdevice nullPtr)

marshalDevice :: Device -> ALCdevice
marshalDevice (Device device) = device

unmarshalDevice :: ALCdevice -> Maybe Device
unmarshalDevice device =
   if device == marshalDevice nullDevice then Nothing else Just (Device device)

-- | 'closeDevice' allows the application (i.e. the client program) to
-- disconnect from a device (i.e. the server). It returns 'True' for success and
-- 'False' for failure. Once closed, the 'Device' is invalid.
--
-- /Note:/ Older OpenAL implementations will always report a success!

closeDevice :: MonadIO m => Device -> m Bool
-- inlined unmarshalALCboolean here to break dependency cycle
closeDevice = liftIO . fmap (/= 0) . alcCloseDevice . marshalDevice

foreign import ccall unsafe "alcCloseDevice"
   alcCloseDevice :: ALCdevice -> IO ALCboolean

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcProcessContext() returns void for all platforms, before it
-- returned ALCcontext* on Linux. To break a dependency cycle, we have to define
-- the Context type here, too.

-- | The abstract context type.

data Context = Context ALCcontext
   deriving ( Eq, Ord, Show )

newtype ALCcontext = ALCcontext (Ptr ALCcontext)
   deriving ( Eq, Ord, Show )

nullContext :: Context
nullContext = Context (ALCcontext nullPtr)

marshalContext :: Context -> ALCcontext
marshalContext (Context context) = context

unmarshalContext :: ALCcontext -> Maybe Context
unmarshalContext context =
   if context == marshalContext nullContext then Nothing else Just (Context context)

--------------------------------------------------------------------------------
-- In ancient times, alcProcessContext returned ALCcontext.

foreign import ccall unsafe "alcProcessContext"
   alcProcessContext :: ALCcontext -> IO ()

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcMakeContextCurrent() returns void, before it was ALCenum on
-- Linux and ALCboolean on other platforms.

foreign import ccall unsafe "alcMakeContextCurrent"
   alcMakeContextCurrent :: ALCcontext -> IO ALCboolean

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcDestroyContext() returns void, before it returned ALCenum
-- on Linux.

foreign import ccall unsafe "alcDestroyContext"
   alcDestroyContext :: ALCcontext -> IO ()

