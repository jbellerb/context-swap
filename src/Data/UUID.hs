{- |
Module      :  Data.UUID
Copyright   :  (c) jae beller 2024
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable

A straightforward implementation of v1 (RFC 4122/DCE 1.1) UUIDs.
-}

module Data.UUID
    ( -- * Limitations
      -- $limitations

      -- * Types
      UUID (..)
    ) where

import Data.Bits (shiftR, (.&.))
import Data.Char (intToDigit)
import Data.List (intersperse)
import Data.Word (Word8, Word16, Word32)
import Foreign.Storable (Storable (..))

{- $limitations

No effort is made to validate the correctness of provided UUIDs and nothing
is provided for generating UUIDs. This merely exists as a type to store the
GUIDs necessary for setting and retrieving UEFI variables.
-}

-- | A v1 (RFC 4122/DCE 1.1) UUID type.
data UUID = UUID
    { uuidTimeLow :: !Word32 -- ^ The low field of the timestamp
    , uuidTimeMid :: !Word16 -- ^ The middle field of the timestamp
    , uuidTimeHiAndVersion :: !Word16 -- ^ The high field of the timestamp
                                      -- multiplexed with the version number
    , uuidClockSeqHiAndReserved :: !Word8 -- ^ The high field of the clock
                                          -- sequence multiplexed with the
                                          -- variant
    , uuidClockSeqLow :: !Word8 -- ^ The low field of the clock sequence

      -- | Octets of the spatially unique node identifier (usually a MAC address)
    , uuidNode0 :: !Word8
    , uuidNode1 :: !Word8
    , uuidNode2 :: !Word8
    , uuidNode3 :: !Word8
    , uuidNode4 :: !Word8
    , uuidNode5 :: !Word8
    }
    deriving (Eq, Ord)

instance Show UUID where
    show uuid = showWord 32 (uuidTimeLow uuid) $ showChar '-'
        $ showWord 16 (uuidTimeMid uuid) $ showChar '-'
        $ showWord 16 (uuidTimeHiAndVersion uuid) $ showChar '-'
        $ showWord 8 (uuidClockSeqHiAndReserved uuid)
        $ showWord 8 (uuidClockSeqLow uuid) $ showChar '-'
        $ showWord 8 (uuidNode0 uuid) $ showWord 8 (uuidNode1 uuid)
        $ showWord 8 (uuidNode2 uuid) $ showWord 8 (uuidNode3 uuid)
        $ showWord 8 (uuidNode4 uuid) $ showWord 8 (uuidNode5 uuid) ""
      where
        showWord len word s = foldr ((:) . hexChar word) s [len-4,len-8..0]
        hexChar word offset =
            intToDigit $ fromIntegral ((word `shiftR` offset) .&. 0x0f)

-- | Layout following [DCE 1.1, Appendix A]
-- (https://pubs.opengroup.org/onlinepubs/9629399/apdxa.htm). Individual words
-- are stored in platform-native endianness.
instance Storable UUID where
    sizeOf _ = 16
    alignment _ = 4

    peek p = UUID
        <$> peekByteOff p 0  -- uuidTimeLow :: Word32
        <*> peekByteOff p 4  -- uuidTimeMid :: Word16
        <*> peekByteOff p 6  -- uuidTimeHiAndVersion :: Word16
        <*> peekByteOff p 8  -- uuidClockSeqHiAndReserved :: Word8
        <*> peekByteOff p 9  -- uuidClockSeqLow :: Word8
        <*> peekByteOff p 10 -- uuidNode0 :: Word8
        <*> peekByteOff p 11 -- uuidNode1 :: Word8
        <*> peekByteOff p 12 -- uuidNode2 :: Word8
        <*> peekByteOff p 13 -- uuidNode3 :: Word8
        <*> peekByteOff p 14 -- uuidNode4 :: Word8
        <*> peekByteOff p 15 -- uuidNode5 :: Word8

    poke p uuid = do
        pokeByteOff p 0 (uuidTimeLow uuid)
        pokeByteOff p 4 (uuidTimeMid uuid)
        pokeByteOff p 6 (uuidTimeHiAndVersion uuid)
        pokeByteOff p 8 (uuidClockSeqHiAndReserved uuid)
        pokeByteOff p 9 (uuidClockSeqLow uuid)
        pokeByteOff p 10 (uuidNode0 uuid)
        pokeByteOff p 11 (uuidNode1 uuid)
        pokeByteOff p 12 (uuidNode2 uuid)
        pokeByteOff p 13 (uuidNode3 uuid)
        pokeByteOff p 14 (uuidNode4 uuid)
        pokeByteOff p 15 (uuidNode5 uuid)
