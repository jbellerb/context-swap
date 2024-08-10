{- |
Module      :  System.Boot.UEFI.Internal.Ioctl.TH
Copyright   :  (c) jae beller 2024
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable

Template Haskell helper functions for deriving ioctl constants.
-}

module System.Boot.UEFI.Internal.Ioctl.TH
    ( -- * Helper functions
      iowr
    ) where

import Data.Bits (shiftL, (.&.), (.|.))

ioc :: Int -> Int -> Int -> Int -> Int
ioc inout group num len =
    inout
        .|. ((len .&. 0x1fff) `shiftL` 16)
        .|. (fromEnum group `shiftL` 8)
        .|. num

-- | Generate an ioctl request capable of both read and write.
iowr :: Char -- ^ Group identifier
  -> Int -- ^ Command
  -> Int -- ^ Struct size
  -> Int -- ^ ioctl request
iowr group = ioc 0xC0000000 (fromEnum group)
