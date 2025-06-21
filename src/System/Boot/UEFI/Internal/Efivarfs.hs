{- |
Module      :  System.Boot.UEFI.Internal.Efivarfs
Copyright   :  (c) jae beller 2025
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (requires *BSD)

Platform specific support for Linux systems using efivarfs to read and write
variables.
-}

module System.Boot.UEFI.Internal.Efivarfs
    ( -- * EfiT monad transformer
      -- $efit
      EfiT (..)
    , runEfiT

      -- * efivarfs implementation details
    , efivarfsDir
    , efivarfsVarPath
    ) where

import Control.Exception (handle)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Identity (IdentityT, runIdentityT)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer (MonadWriter)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.Text.Encoding (decodeUtf16LE)
import Data.Text (unpack)
import Data.UUID (UUID)
import Data.Word (Word32)
import Foreign.C.Error (Errno (..), getErrno)
import System.Boot.UEFI (MonadEfi (..))
import System.Directory (removeFile)
import System.IO (IOMode (..))

{- $efit

Only 'EfiT' needs to be defined, as 'System.Boot.UEFI.Internal.Efi' can
trivially be implemented in terms of it.
-}

efivarfsDir = "/sys/firmware/efi/efivars"

-- | Locate a UEFI variable in the system efivarfs.
efivarfsVarPath :: UUID -- ^ The vendor GUID the variable is defined under
    -> BS.ByteString -- ^ The UCS-2LE encoded name of the variable
    -> FilePath -- ^ The path for accessing the variable as a file
efivarfsVarPath efiVarVendor nameString = efivarfsDir <> "/" <> filename
  where
    filename = unpack (decodeUtf16LE nameString) <> "-" <> show efiVarVendor

handleErrno :: (MonadIO m) => IO a -> EfiT m (Either Errno a)
handleErrno = liftIO . handle wrapErrno . (Right <$>)
  where
    wrapErrno :: IOError -> IO (Either Errno a)
    wrapErrno = const $ Left <$> getErrno

-- | Monad transformer for performing UEFI actions over an existing transformer
-- stack. The underlying monad must implement 'MonadIO' to be able to evaluate
-- this.
newtype EfiT m a = EfiT { unEfiT :: IdentityT m a }
    deriving
        ( Functor , Applicative , Monad, MonadIO
        , MonadError e, MonadReader r, MonadState s, MonadTrans, MonadWriter w
        )

-- | Evaluate the 'EfiT' transformer as an 'IO' action in the underlying monad.
runEfiT :: (MonadIO m) => EfiT m a -> m a
runEfiT = runIdentityT . unEfiT

instance (MonadIO m) => MonadEfi (EfiT m) where
    clearEfiVar = clearEfiVarEfivarfsImpl
    getEfiVar = getEfiVarEfivarfsImpl
    setEfiVar = setEfiVarEfivarfsImpl

clearEfiVarEfivarfsImpl ::
    (MonadIO m) => UUID -> BS.ByteString -> EfiT m (Either Errno ())
clearEfiVarEfivarfsImpl efiVarVendor nameString =
    handleErrno $ removeFile $ efivarfsVarPath efiVarVendor nameString

getEfiVarEfivarfsImpl ::
    (MonadIO m) => UUID -> BS.ByteString -> EfiT m (Either Errno BS.ByteString)
getEfiVarEfivarfsImpl efiVarVendor nameString = handleErrno $ do
    rawData <- BS.readFile $ efivarfsVarPath efiVarVendor nameString
    pure $ BS.drop 4 rawData

setEfiVarEfivarfsImpl ::
    (MonadIO m) =>
    UUID -> BS.ByteString -> Word32 -> BS.ByteString -> EfiT m (Either Errno ())
setEfiVarEfivarfsImpl efiVarVendor nameString efiVarAttrib dataString =
    handleErrno $
        BSB.writeFile (efivarfsVarPath efiVarVendor nameString) rawData
  where
    rawData = BSB.word32LE efiVarAttrib <> BSB.byteString dataString
