{- |
Module      :  System.Hibernate
Copyright   :  (c) jae beller 2024
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (see package documentation)

Start hibernation. This module aims to be as simple as possible (UEFI was
complicated enough). On unix-like systems this will call ZZZ through the shell.
TODO: Windows support.
-}

module System.Hibernate
    ( -- * Hibernate
      hibernate
    ) where

import System.Process (callCommand)

-- | Hibernate the system. 
hibernate :: IO ()
hibernate = callCommand "ZZZ"
