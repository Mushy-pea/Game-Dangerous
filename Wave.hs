module Wave (
  WAVE(..), WAVEHeader(..), WAVESample, WAVESamples,
  hGetWAVE, getWAVEFile,
  hPutWAVE, putWAVEFile,
  sampleToDouble, doubleToSample
) where

import Control.Monad
import System.IO
import Data.Char
import Data.Int
import Data.Word
import Data.Bits
import Data.List
import qualified Data.ByteString.Lazy as BS

-- |For internal use only; the header as it appears on-disk.
-- The interface cleans this up to remove redundancy and
-- make things easier to understand.
data WAVERawHeader = WAVERawHeader { rawNumChannels :: Int,
                                     rawSampleRate :: Int,
                                     rawByteRate :: Int,
                                     rawBlockAlign :: Int,
                                     rawBitsPerSample :: Int,
                                     rawFrames :: Maybe Int }

-- |Descriptive information for the audio source.
data WAVEHeader = WAVEHeader { waveNumChannels :: Int  -- ^Samples per frame.
                             , waveFrameRate :: Int -- ^Frames per second.
                             , waveBitsPerSample :: Int -- ^Number of
                                 -- significant bits of left-justified value.
                             , waveFrames :: Maybe Int -- ^If present,
                                 -- number of frames in the stream.
                                 -- Otherwise, can be (inefficiently)
                                 -- inferred from the length of the
                                 -- stream.
                             }

-- |Each sample is a left-justified signed integer, with
-- significant bits as given in the header.
type WAVESample = Int32

-- |A stream is a list of frames, each of which is a list of
-- samples with one sample per channel.
type WAVESamples = [[WAVESample]]

-- |The header and stream read or written.
data WAVE = WAVE { waveHeader :: WAVEHeader,
                   waveSamples :: WAVESamples }

bits_to_bytes :: (Integral a) => a -> a
bits_to_bytes n = (n + 7) `div` 8

collect :: Int -> [a] -> [[a]]
collect n [] = []
collect n s = h : collect n s'
    where (h, s') = splitAt n s

-- |Utility routine for working with audio data in floating
-- point format.
sampleToDouble :: WAVESample -> Double
sampleToDouble v =
    let maxb = fromIntegral (maxBound :: WAVESample)
        minb = fromIntegral (minBound :: WAVESample) in
    if v >= 0
    then fromIntegral v / maxb
    else -(fromIntegral v) / minb

-- |Utility routine for working with audio data in floating
-- point format.
doubleToSample :: Double -> WAVESample
doubleToSample v =
    let maxb = fromIntegral (maxBound :: WAVESample)
        minb = fromIntegral (minBound :: WAVESample) in
    if v >= 0
    then (fromIntegral . floor . (* maxb)) (min v 1)
    else (fromIntegral . ceiling . (* minb)) (min (-v) 1)

bs_to_string :: BS.ByteString -> String
bs_to_string b =  map (chr . fromIntegral) (BS.unpack b)

match :: Handle -> String -> IO ()
match h s = do
  b <- BS.hGet h (length s)
  unless (bs_to_string b == s)
         (error ("mismatched format string '" ++ s ++ "'"))

convert_nbytes_lend :: (Num a) => [Word8] -> a
convert_nbytes_lend bs =
  foldl accum 0 (reverse bs)
  where
    accum a b = 256 * a + fromIntegral b

get_nbytes_lend :: Handle -> Int -> IO Int
get_nbytes_lend h n = do
  bytes <- BS.hGet h n
  return (convert_nbytes_lend (BS.unpack bytes))

get_word_lend :: Handle -> IO Int
get_word_lend h = get_nbytes_lend h 4

get_halfword_lend :: Handle -> IO Int
get_halfword_lend h = get_nbytes_lend h 2

get_wave_header :: Handle -> IO WAVERawHeader
get_wave_header h = do
  size <- get_word_lend h
  audio_format <- get_halfword_lend h
  unless (audio_format == 1)
         (error "PCM only for now")
  unless (size == 16)
         (error "bad PCM chunk size")
  num_channels <- get_halfword_lend h
  frame_rate <- get_word_lend h
  byte_rate <- get_word_lend h
  block_align <- get_halfword_lend h
  bits_per_sample <- get_halfword_lend h
  return (WAVERawHeader { rawNumChannels = num_channels,
                          rawSampleRate = frame_rate,
                          rawByteRate = byte_rate,
                          rawBlockAlign = block_align,
                          rawBitsPerSample = bits_per_sample,
                          rawFrames = Nothing })
  
skip_chunk :: Handle -> IO ()
skip_chunk h = do
  size <- get_word_lend h
  hSeek h RelativeSeek (fromIntegral size)

get_wave_data :: Handle -> WAVERawHeader -> IO (WAVERawHeader, WAVESamples)
get_wave_data h hd = do
  size <- get_word_lend h
  let bits_per_sample = rawBitsPerSample hd
  let bytes_per_sample = bits_to_bytes bits_per_sample
  when (rawBlockAlign hd /= bytes_per_sample * rawNumChannels hd)
       (error "internal error: align and bits disagree")
  let frames = size `div` rawBlockAlign hd
  let count = frames * rawNumChannels hd
  samples <- case bytes_per_sample of
               1 -> do
                 bytes <- BS.hGet h count
                 return (map convert_byte (BS.unpack bytes))
               n | n <= 4 -> do
                 bytes <- BS.hGet h (count * n)
                 let words = collect n (BS.unpack bytes)
                 return (map (convert_multibyte n) words)
               _ -> error "max 32 bits per sample for now"
  let samples' = map (mask bits_per_sample) samples
  return (hd { rawFrames = Just frames },
          collect (rawNumChannels hd) samples')
  where
    convert_byte =
        (`shift` 24) .
        (fromIntegral :: Int8 -> WAVESample) .
        (fromIntegral :: Word8 -> Int8)
    convert_multibyte n =
        (`shift` (32 - 8 * n)) .
        (convert_nbytes_lend :: [Word8] -> WAVESample)
    mask bits v =
        (v .&. (((1 `shift` bits) - 1) `shift` (32 - bits)))

  
cook_header :: WAVERawHeader -> WAVEHeader
cook_header (WAVERawHeader { rawNumChannels = nc,
                             rawSampleRate = sr,
                             rawBitsPerSample = bps,
                             rawBlockAlign = ba,
                             rawFrames = Just s }) =
    WAVEHeader { waveNumChannels = nc,
                 waveFrameRate = sr,
                 waveBitsPerSample = bps,
                 waveFrames = Just s }

get_chunks :: Handle -> Maybe WAVERawHeader -> Maybe WAVESamples -> IO WAVE
get_chunks _ (Just hd) (Just s) =
    return WAVE { waveHeader = cook_header hd,
                  waveSamples = s }
get_chunks h mh ms = do
    s <- get_chunk_header
    process_chunk s mh ms
    where
      get_chunk_header = do
        bs <- BS.hGet h 4
        return (bs_to_string bs)
      process_chunk "fmt " Nothing Nothing = do
        nh <- get_wave_header h
        get_chunks h (Just nh) ms
      process_chunk "fmt " _ _ =
          error "duplicate fmt chunk in WAVE"
      process_chunk "data" (Just nh) Nothing = do
        (nh', nd) <- get_wave_data h nh
        get_chunks h (Just nh') (Just nd)
      process_chunk "data" _ _ =
          error "no fmt chunk or duplicate data chunk in WAVE"
      process_chunk _ nh ms = do
        skip_chunk h
        get_chunks h nh ms

-- |Read the WAVE file at the given handle and return the audio data.
hGetWAVE :: Handle -> IO WAVE
hGetWAVE h = do
    hSetBinaryMode h True
    hSetBuffering h (BlockBuffering Nothing)
    match h "RIFF"
    size <- get_word_lend h
    match h "WAVE"
    wav <- get_chunks h Nothing Nothing
    return wav

-- |Read the WAVE file at the given path and return the audio data.
getWAVEFile :: String -> IO WAVE
getWAVEFile fn = do
    h <- openFile fn ReadMode
    wav <- hGetWAVE h
    hClose h
    return wav

unconvert_nbytes_lend :: Int -> Int -> [Word8]
unconvert_nbytes_lend 0 _ = []
unconvert_nbytes_lend n v =
    (fromIntegral (v .&. 255)) :
      (unconvert_nbytes_lend (n - 1) (v `shift` (-8)))

put_nbytes_lend :: Handle -> Int -> Int -> IO ()
put_nbytes_lend h n v = do
  let bytes = BS.pack (unconvert_nbytes_lend n v)
  BS.hPut h bytes

put_word_lend :: Handle -> Int -> IO ()
put_word_lend h v = put_nbytes_lend h 4 v

put_halfword_lend :: Handle -> Int -> IO ()
put_halfword_lend h v = put_nbytes_lend h 2 v

put_wave_header :: Handle -> WAVEHeader -> IO ()
put_wave_header h hd = do
  put_halfword_lend h 1   --- PCM
  let num_channels = waveNumChannels hd
  put_halfword_lend h num_channels
  let frame_rate = waveFrameRate hd
  put_word_lend h frame_rate
  let bytes_per_sample = bits_to_bytes (waveBitsPerSample hd)
  let block_align = bytes_per_sample * num_channels
  let byte_rate = frame_rate * block_align
  put_word_lend h byte_rate
  put_halfword_lend h block_align
  put_halfword_lend h (waveBitsPerSample hd)

put_wave_data :: Handle -> WAVEHeader -> [WAVESample] -> IO ()
put_wave_data h hd sa = do
  let nb = bits_to_bytes (waveBitsPerSample hd)
  when (nb <= 0 || nb > 4)
       (error "supported sample sizes 1..32 bits for now")
  let saa = map ((flip shift) (8 * nb - 32)) sa
  let ba = if nb == 1
           then map (fromIntegral . (.&. 255) . (+ 128)) saa
           else concatMap (unconvert_nbytes_lend nb . fromIntegral) saa
  let bytes = BS.pack ba
  BS.hPut h bytes

-- |Write the given audio data to the given handle as a WAVE file.
hPutWAVE :: Handle -> WAVE -> IO ()
hPutWAVE h wav = do
  hSetBinaryMode h True
  hSetBuffering h (BlockBuffering Nothing)
  ---
  let header = waveHeader wav
  let samples = waveSamples wav
  let frame_count = case waveFrames header of
                       Just n -> n
                       Nothing -> length samples
  let frame_samples = frame_count * waveNumChannels header
  let header_size = 2 + 2 + 4 + 4 + 2 + 2
  let bytes_per_sample = bits_to_bytes (waveBitsPerSample header)
  let data_size = frame_samples * bytes_per_sample
  ---
  hPutStr h "RIFF"
  put_word_lend h (4 + header_size + 8 + data_size + 8)
  hPutStr h "WAVE"
  ---
  hPutStr h "fmt "
  put_word_lend h header_size
  put_wave_header h header
  ---
  hPutStr h "data"
  put_word_lend h data_size
  put_wave_data h header (concat samples)

-- |Write the given audio data to the given path as a WAVE file.
putWAVEFile :: String -> WAVE -> IO ()
putWAVEFile fn wav = do
    h <- openFile fn WriteMode
    hPutWAVE h wav
    hClose h
