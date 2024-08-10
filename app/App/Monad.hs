{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  App.Monad
Copyright   :  (c) jae beller 2024
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable

The main monad to run the app in. This monad stack supports IO, exceptions (with
'Text' error messages), UEFI operations, and retrieving the supplied options.
-}

module App.Monad
    ( -- * App monad
      App (..)
    , runApp
    ) where

import App.Options (Options)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Text (Text)
import Data.Text.IO (hPutStrLn)
import System.Boot.UEFI (MonadEfi)
import System.Boot.UEFI.Internal.Ioctl (EfiT, runEfiT)
import System.Exit (exitFailure)
import System.IO (stderr)

-- | Monad transformer stack holding the app's state.
newtype App a = App { unApp :: ReaderT Options (ExceptT Text (EfiT IO)) a }
    deriving
        ( Functor, Applicative, Monad, MonadIO
        , MonadEfi, MonadError Text, MonadReader Options
        )

-- | Evaluate the app monad given the program name (for generating error
-- messages) and a set of options.
runApp :: Text -- ^ Invoked program name
    -> Options -- ^ Program options
    -> App a
    -> IO a
runApp progName options m = do
    res <- runEfiT $ runExceptT $ runReaderT (unApp m) options
    case res of
        Left s -> do
            hPutStrLn stderr (progName <> ": " <> s)
            exitFailure
        Right res -> pure res
