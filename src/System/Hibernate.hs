{-# LANGUAGE CPP #-}

{- |
Module      :  System.Hibernate
Copyright   :  (c) jae beller 2024
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (see package documentation)

Start hibernation. This module aims to be as simple as possible (UEFI was
complicated enough). On Linux this will call "systemctl hibernate", and on
BSD-like systems this will call "ZZZ".
TODO: Windows support.
-}

module System.Hibernate
    ( -- * Hibernate
      hibernate
    ) where

import System.Process (callCommand)

-- | Hibernate the system. 
hibernate :: IO ()
#if defined(linux_HOST_OS)
hibernate = callCommand "systemctl hibernate"
#elif defined(openbsd_HOST_OS) || defined(freebsd_HOST_OS) \
    || defined(netbsd_HOST_OS) || defined(dragonflybsd_HOST_OS)
hibernate = callCommand "ZZZ"
#else
hibernate = undefined
#endif
