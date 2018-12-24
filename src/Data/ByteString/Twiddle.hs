{-# LANGUAGE MagicHash  #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict     #-}

module Data.ByteString.Twiddle
  (
  ) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Bits
import Data.ByteString.Internal (ByteString(..), accursedUnutterablePerformIO)
import Data.Word (Word8, Word64)
import GHC.Base
import GHC.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Text.Printf

showBinary x = printf "%b" x ++ "\n"

-- | A 'Twiddle' allows for bit-twiddling optimisations on a 'ByteString' by
--   splitting it up into six pointers:
--   
--   * 1. A 'Word8' pointer to the beginning of the 'ByteString'
--   * 2. A 'Word8' pointer to the nearest 8-byte-aligned offset from (2)
--   * 3. A 'Word64' pointer to the spot right after (2)
--   * 4. A 'Word64' pointer to the last 8-byte-aligned offset from the end of the 'ByteString'
--   * 5. A 'Word8' pointer that is the same as (4), but signifies the end of the 'Word64' region
--   * 6. A 'Word8' pointer to the end of the 'ByteString'
--
--   Since the 'ByteString' has been aligned, we now have access to a region (between (3) and (4)) where
--   we can map functions over that region at 8x the speed of a 'Word8' mapping.
data Twiddle = Twiddle
  { startPrefix :: Ptr Word8
  , endPrefix   :: Ptr Word8
  , startMid    :: Ptr Word64
  , endMid      :: Ptr Word64
  , startPost   :: Ptr Word8
  , endPost     :: Ptr Word8
  }

alignPtrPos, alignPtrNeg :: Ptr a -> Ptr a
alignPtrPos addr@(Ptr a) = case remAddr# a 8# of
  0# -> addr
  n  -> Ptr (plusAddr# a (8# -# n))
alignPtrNeg addr@(Ptr a) = case remAddr# a 8# of
  0# -> addr
  n  -> Ptr (plusAddr# a (negateInt# n))

-- | Create a 'Twiddle'. The result is 'Nothing' if the 'ByteString' was empty.
thumb :: ByteString -> Maybe Twiddle
thumb (PS _ _ 0) = Nothing
thumb b@(PS fp (I# o#) len@(I# l#)) = Just . accursedUnutterablePerformIO . withForeignPtr fp $
  \(Ptr addr#) -> do
    let 
      startPre'  = Ptr (plusAddr# addr# o#)
      endPre'    = alignPtrPos startPre'
      startMid'  = castPtr endPre'
      endMid'    = castPtr startPost'
      startPost' = alignPtrNeg endPost'
      endPost'   = Ptr (plusAddr# addr# (o# +# l#))
    pure (Twiddle startPre' endPre' startMid' endMid' startPost' endPost')

-- duplicate 01101110 = 01101110 01101110 01101110 01101110 01101110 01101110 01101110 01101110
duplicate :: Word8 -> Word64
duplicate w =
  let w' = fromIntegral w
      w0 = unsafeShiftL w' 56
      w1 = unsafeShiftL w' 48
      w2 = unsafeShiftL w' 40
      w3 = unsafeShiftL w' 32
      w4 = unsafeShiftL w' 24
      w5 = unsafeShiftL w' 16
      w6 = unsafeShiftL w'  8
      w7 = unsafeShiftL w'  0
  in (w0 + w1 + w2 + w3 + w4 + w5 + w6 + w7)