{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  App.Options
Copyright   :  (c) jae beller 2024
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable

Helpers for parsing command line options in the cswap utility.
-}

module App.Options
    ( -- * Command line options
      Options (..)
    , defaultOptions
    , options

      -- * Usage info
    , usage
    ) where

import Data.Text (pack, Text)
import Data.Word (Word16)
import System.Console.GetOpt (ArgDescr (..), OptDescr (..))

-- | Generate a helpful usage string for when an invalid argument was given.
usage :: Text -- ^ Invoked program name
  -> Text -- ^ Usage string
usage p = "usage: " <> p <> " [-LnqVvX] [target]"

-- | Options available to select.
data Options = Options
    { optClearNext :: Bool -- ^ Clear the currently set boot override (-X)
    , optNoHibernate :: Bool -- ^ Change the boot override without hibernating
                             -- (-q)
    , optNoTarget :: Bool -- ^ Hibernate without changing the boot override (-n)
    , optShowBootOrder :: Bool -- ^ List the current boot order and exit (-L)
    , optShowVersion :: Bool -- ^ Show the program version and exit (-V)
    , optTarget :: Maybe (Either Text Word16) -- ^ The boot target to set the
                                              -- next boot to
    , optVerbose :: Bool -- ^ Enable verbose logging (-v)
    }
    deriving (Show)

-- | Set the boot target to the highest target in the current boot order that
-- isn't the currently booted target and hibernate.
defaultOptions :: Options
defaultOptions = Options
    { optClearNext = False
    , optNoHibernate = False
    , optNoTarget = False
    , optShowBootOrder = False
    , optShowVersion = False
    , optTarget = Nothing
    , optVerbose = False
    }

-- | List of options for 'System.Console.GetOpt.getOpt' to look for when parsing
-- args.
options :: [OptDescr (Options -> Options)]
options =
    [ Option ['V'] []
        ( NoArg (\opt -> opt{optShowVersion = True}) )
        "show version and exit"
    , Option ['L'] []
        ( NoArg (\opt -> opt{optShowBootOrder = True}) )
        "list current boot order and exit"
    , Option ['X'] []
        ( NoArg (\opt -> opt{optClearNext = True}) )
        "clear the current boot override and exit"
    , Option ['v'] []
        ( NoArg (\opt -> opt{optVerbose = True}) )
        "print debug information"
    , Option ['q'] []
        ( NoArg (\opt -> opt{optNoHibernate = True}) )
        "change boot without hibernating"
    , Option ['n'] []
        ( NoArg (\opt -> opt{optNoTarget = True}) )
        "hibernate without changing boot"
    ]
