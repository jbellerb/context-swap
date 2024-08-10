{-# LANGUAGE CPP #-}

{- |
Module      :  System.Boot.UEFI.Internal
Copyright   :  (c) jae beller 2024
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (see package documentation)

Platform specific implementations of 'System.Boot.UEFI.MonadEfi'. This
module exports whatever backend has been determined at build time to be most
appropriate.
-}

module System.Boot.UEFI.Internal
    ( -- * OS Support
      -- $support

      -- * Efi monad
      Efi
    , runEfi

      -- * EfiT monad transformer
    , EfiT (..)
    , runEfiT
    ) where

#if defined(openbsd_HOST_OS) || defined(freebsd_HOST_OS) \
    || defined(netbsd_HOST_OS) || defined(dragonflybsd_HOST_OS)
import System.Boot.UEFI.Internal.Ioctl (EfiT (..), runEfiT)
#endif

{- $support

The following backends are currently provided:

+-----------------------------------+-----------+-----------+
| Implementation                    | Target OS | Quality   |
+===================================+===========+===========+
| "System.Boot.UEFI.Internal.Ioctl" | OpenBSD¹  | Excellent |
+-----------------------------------+-----------+-----------+

¹ /Experimental (untested) support is also provided for FreeBSD, NetBSD, and \
DragonflyBSD./
-}

-- | A basic UEFI monad over 'IO'.
type Efi = EfiT IO

-- | Evaluate an 'Efi' monad as an 'IO' action.
runEfi :: Efi a -> IO a
runEfi = runEfiT
