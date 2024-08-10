{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  System.Boot.UEFI.Internal.Ioctl
Copyright   :  (c) jae beller 2024
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (requires *BSD)

Platform specific support for BSD systems using ioctl to read and write
variables. I have only tested OpenBSD, however the others should work if I read
their manuals ([1]
(https://man.freebsd.org/cgi/man.cgi?query=efidev&sektion=4&apropos=0&manpath=FreeBSD+14.1-RELEASE+and+Ports),
[2](https://man.dragonflybsd.org/?command=efidev&section=ANY)) (or lack [thereof]
(http://cvsweb.netbsd.org/bsdweb.cgi/src/sys/sys/efiio.h?rev=1.2.4.1&content-type=text/x-cvsweb-markup))
properly.
-}

module System.Boot.UEFI.Internal.Ioctl
    ( -- * EfiT monad transformer
      -- $efit
      EfiT (..)
    , runEfiT

      -- * ioctl implementation details
    , ioctl

      -- ** Requests
    , efiIocVarGet
    , efiIocVarSet

      -- ** Structs
    , EfiVarIoc (..)
    ) where

import Control.Monad.Except (ExceptT (..), liftEither, MonadError, runExceptT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (mapReaderT, MonadReader (..), ReaderT, runReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer (MonadWriter)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (toForeignPtr, unsafeWithForeignPtr)
import Data.UUID (UUID)
import Data.Word (Word8, Word16, Word32)
import Foreign.C (CInt)
import Foreign.C.Error (Errno (..), getErrno)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (copyBytes, with)
import Foreign.Ptr (castPtr, nullPtr, Ptr)
import Foreign.Storable (Storable (..))
import qualified Language.Haskell.TH.Syntax as TH (lift)
import System.Boot.UEFI (MonadEfi (..))
import System.Boot.UEFI.Internal.Ioctl.TH (iowr)
import System.IO (openFile, IOMode (..))
import System.Posix (Fd)
import System.Posix.IO (closeFd, handleToFd)

{- $efit

Only 'EfiT' needs to be defined, as 'System.Boot.UEFI.Internal.Efi' can
trivially be implemented in terms of it.
-}

#include <sys/ioctl.h>
#if defined(__OpenBSD__)
#include <dev/efi/efiio.h>
#else
#include <sys/efiio.h>
#endif

#if defined(__NetBSD__)
#define EFIIOC_GROUP 'e'
#define EFIIOC_VAR_GET_CMD 4
#define EFIIOC_VAR_SET_CMD 7
#else
#define EFIIOC_GROUP 'E'
#if defined(__OpenBSD__)
#define EFIIOC_VAR_GET_CMD 2
#define EFIIOC_VAR_SET_CMD 4
#endif
#if defined(__FreeBSD__) || defined(__DragonflyBSD__)
#define EFIIOC_VAR_GET_CMD 4
#define EFIIOC_VAR_SET_CMD 6
#endif
#endif

-- While C2HS can pull in constants from C header files, since EFIIOC_VAR_*
-- are defined with a #define macro it produces junk C expressions that Haskell
-- can't understand. Instead I've implemented as much of the macro as needed to
-- get this to work in Template Haskell.
efiIocVarGet =
    $(TH.lift $ iowr
        {#const EFIIOC_GROUP #}
        {#const EFIIOC_VAR_GET_CMD #}
        {#sizeof efi_var_ioc #}
    ) :: Int
efiIocVarSet =
    $(TH.lift $ iowr
        {#const EFIIOC_GROUP #}
        {#const EFIIOC_VAR_SET_CMD #}
        {#sizeof efi_var_ioc #}
    ) :: Int

-- | Call ioctl on an open file descriptor
ioctl :: (Storable d) => Fd -- ^ File descriptor
    -> Int -- ^ ioctl request
    -> Ptr d -- ^ Pointer to the device data structure
    -> IO (Either Errno ()) -- ^ Any possible errors that may have occurred
ioctl f req d = ioctl_ f req (castPtr d)
  where
    {#fun variadic ioctl[void*] as ioctl_
        { fromIntegral `Fd'
        , `Int'
        , `Ptr ()'
        } -> `Either Errno ()' eitherIoctlErrno* #}
    eitherIoctlErrno :: CInt -> IO (Either Errno ())
    eitherIoctlErrno res
        | res == -1 = Left <$> getErrno
        | otherwise = pure $ Right ()

withTermString :: BS.ByteString -> (Int -> Ptr Word16 -> IO a) -> IO a
withTermString s f = allocaBytes (len + 2) $ \bufPtr -> do
    unsafeWithForeignPtr bsFPtr $ \bsPtr -> copyBytes bufPtr bsPtr len
    pokeByteOff bufPtr len (0 :: Word16)
    f (len + 2) (castPtr bufPtr)
  where
    (bsFPtr, _, len) = toForeignPtr s

-- | Monad transformer for performing UEFI actions over an existing transformer
-- stack. The underlying monad must implement 'MonadIO' to be able to evaluate
-- this.
newtype EfiT m a = EfiT { unEfiT :: ReaderT Fd m a }
    deriving
        ( Functor , Applicative , Monad, MonadIO
        , MonadError e, MonadState s, MonadTrans, MonadWriter w
        )

-- | Evaluate the 'EfiT' transformer as an 'IO' action in the underlying monad.
runEfiT :: (MonadIO m) => EfiT m a -> m a
runEfiT m = do
    fd <- liftIO $ handleToFd =<< openFile "/dev/efi" ReadMode
    res <- runReaderT (unEfiT m) fd
    liftIO $ closeFd fd
    pure res

instance (MonadIO m) => MonadEfi (EfiT m) where
    clearEfiVar = clearEfiVarIoctlImpl
    getEfiVar = getEfiVarIoctlImpl
    setEfiVar = setEfiVarIoctlImpl

-- | Explicit 'MonadReader' instance to skip the internal 'ReaderT' that holds
-- the \/dev\/efi file descriptor.
instance (MonadReader r m) => MonadReader r (EfiT m) where
    ask = lift ask
    local f x = EfiT $ mapReaderT (local f) (unEfiT x)

clearEfiVarIoctlImpl ::
    (MonadIO m) => UUID -> BS.ByteString -> EfiT m (Either Errno ())
clearEfiVarIoctlImpl efiVarVendor nameString = do
    fd <- EfiT ask
    liftIO $ withTermString nameString $ \efiVarNameSize efiVarName ->
        with
            ( EfiVarIoc
                { efiVarName
                , efiVarNameSize
                , efiVarVendor
                , efiVarAttrib = 0
                , efiVarData = nullPtr
                , efiVarDataSize = 0
                }
            )
            (ioctl fd efiIocVarSet)

getEfiVarIoctlImpl ::
    (MonadIO m) => UUID -> BS.ByteString -> EfiT m (Either Errno BS.ByteString)
getEfiVarIoctlImpl efiVarVendor nameString = do
    fd <- EfiT ask
    liftIO $ withTermString nameString $ \efiVarNameSize efiVarName -> do
        with
            ( EfiVarIoc
                { efiVarName
                , efiVarNameSize
                , efiVarVendor
                , efiVarAttrib = 0
                , efiVarData = nullPtr
                , efiVarDataSize = 0
                }
            )
            (getData fd)
  where
    getDataSize fd varPtr = do
        ExceptT $ ioctl fd efiIocVarGet varPtr
        efiVarDataSize <$> liftIO (peek varPtr)
    getData fd varPtr = runExceptT $ do
        dataSize <- getDataSize fd varPtr
        ExceptT $ allocaBytes dataSize $ \dataPtr -> runExceptT $ do
            liftIO $ pokeByteOff varPtr {#offsetof efi_var_ioc.data #} dataPtr
            ExceptT $ ioctl fd efiIocVarGet varPtr
            liftIO $ BS.packCStringLen (castPtr dataPtr, dataSize)

setEfiVarIoctlImpl ::
    (MonadIO m) =>
    UUID -> BS.ByteString -> Word32 -> BS.ByteString -> EfiT m (Either Errno ())
setEfiVarIoctlImpl efiVarVendor nameString efiVarAttrib dataString = do
    fd <- EfiT ask
    liftIO $ withTermString nameString $ \efiVarNameSize efiVarName ->
        BS.useAsCStringLen dataString $ \(efiVarData, efiVarDataSize) ->
            with
                EfiVarIoc
                    { efiVarName
                    , efiVarNameSize
                    , efiVarVendor
                    , efiVarAttrib
                    , efiVarData = castPtr efiVarData
                    , efiVarDataSize
                    }
                (ioctl fd efiIocVarSet)

-- | Struct for storing the ioctl parameters for both EFIIOC_VAR_GET and
-- EFIIOC_VAR_SET.
data EfiVarIoc = EfiVarIoc
    { efiVarName :: !(Ptr Word16) -- ^ Pointer to the variable name in UCS-2
                                  -- characters
    , efiVarNameSize :: !Int -- ^ Number of bytes in 'efiVarName', including the
                             -- two terminating null bytes
    , efiVarVendor :: !UUID -- ^ Vendor GUID for the variable
    , efiVarAttrib :: !Word32 -- ^ Bitset of variable attributes
    , efiVarData :: !(Ptr Word8) -- ^ Pointer to the value
    , efiVarDataSize :: !Int -- ^ Number of bytes in 'efiVarData'
    }

instance Storable EfiVarIoc where
    sizeOf _ = {#sizeof efi_var_ioc #}
    alignment _ = {#alignof efi_var_ioc #}

    peek p = EfiVarIoc
        <$> (castPtr <$> {#get efi_var_ioc->name #} p)
        <*> (fromIntegral <$> {#get efi_var_ioc->namesize #} p)
        <*> peekByteOff p {#offsetof efi_var_ioc->vendor #}
        <*> (fromIntegral <$> {#get efi_var_ioc->attrib #} p)
        <*> (castPtr <$> {#get efi_var_ioc->data #} p)
        <*> (fromIntegral <$> {#get efi_var_ioc->datasize #} p)

    poke p ioc = do
        {#set efi_var_ioc.name #} p $ castPtr $ efiVarName ioc
        {#set efi_var_ioc.namesize #} p $ fromIntegral $ efiVarNameSize ioc
        pokeByteOff p {#offsetof efi_var_ioc.vendor #} $ efiVarVendor ioc
        {#set efi_var_ioc.attrib #} p $ fromIntegral $ efiVarAttrib ioc
        {#set efi_var_ioc.data #} p $ castPtr $ efiVarData ioc
        {#set efi_var_ioc.datasize #} p $ fromIntegral $ efiVarDataSize ioc
