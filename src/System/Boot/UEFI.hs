{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  System.Boot.UEFI
Copyright   :  (c) jae beller 2024
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable

Utilities for manipulating UEFI variables. All actions must be performed under
an instance of 'MonadEfi', which abstracts away platform-specific behavior of
interacting with UEFI. Implementations of 'MonadEfi' for supported systems are
provided in "System.Boot.UEFI.Internal".
-}

module System.Boot.UEFI
    ( -- * MonadEfi class
      -- $monadefi
      MonadEfi (..)

      -- * Constants
      -- ** UEFI vendor GUIDs
    , efiGlobalVariable
      -- ** UEFI variable attribute bitmasks
    , efiVariableNonVolatile
    , efiVariableBootserviceAccess
    , efiVariableRuntimeAccess
    , efiVariableHardwareErrorRecord
    , efiVariableTimeBasedAuthenticatedWriteAccess
    , efiVariableAppendWrite
    , efiVariableEnhancedAuthenticatedAccess

      -- * Getting and setting boot variables
      -- ** Boot order
    , clearBootNext
    , getBootCurrent
    , getBootNext
    , getBootOrder
    , setBootNext
      -- ** Boot targets
    , BootOption (..)
    , getBootOption
    ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T (pack)

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.RWS (RWST)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer (WriterT)
import Data.Bifunctor (bimap, first)
import Data.Bits (Bits, shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Builder (toLazyByteString, word16LE)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf16LE, encodeUtf16LE)
import Data.UUID (UUID (..))
import Data.Word (Word16, Word32)
import Foreign.C.Error (eNOENT, Errno, errnoToIOError)

{- $monadefi

To abstract over platform differences, all operations involving UEFI variables
must be run in an instance of 'MonadEfi'. Implementations of 'MonadEfi' must
expose the capability to clear, get, and set variables. These are considered
low-level interfaces and should be used carefully. Note that while the name
field is given as a 'BS.ByteString', its contents must be UCS-2LE encoded text.
It's unclear to me if they meant UTF-16LE, since the [UEFI specification]
(https://uefi.org/specs/UEFI/2.10/02_Overview.html) defers to Unicode 2.1,
however calling the encoding "UCS-2" was deprecated in Unicode 2.0 when
surrogate pairs were added. As a precaution, avoid attempt to create variables
with emoji names.
-}

efiGlobalVariable =
    UUID 0x8BE4DF61 0x93CA 0x11d2 0xAA 0x0D 0x00 0xE0 0x98 0x03 0x2B 0x8C

efiVariableNonVolatile = 0x00000001 :: Word32
efiVariableBootserviceAccess = 0x00000002 :: Word32
efiVariableRuntimeAccess = 0x00000004 :: Word32
efiVariableHardwareErrorRecord = 0x00000008 :: Word32
efiVariableTimeBasedAuthenticatedWriteAccess = 0x00000020 :: Word32
efiVariableAppendWrite = 0x00000040 :: Word32
efiVariableEnhancedAuthenticatedAccess = 0x00000080 :: Word32

-- | A monad representing computations that interact with UEFI variables.
class Monad m => MonadEfi m where
    -- | Clear a UEFI variable by replacing it with an empty value.
    clearEfiVar :: UUID -- ^ The vendor GUID the variable is defined under
        -> BS.ByteString -- ^ The UCS-2LE encoded name of the variable
        -> m (Either Errno ()) -- ^ Any possible errors that may have occurred
    default clearEfiVar ::
        (MonadTrans t, MonadEfi m1, m ~ t m1) =>
        UUID -> BS.ByteString -> m (Either Errno ())
    clearEfiVar vendor name = lift $ clearEfiVar vendor name

    -- | Get a UEFI variable.
    getEfiVar :: UUID -- ^ The vendor GUID the variable is defined under
        -> BS.ByteString -- ^ The UCS-2LE encoded name of the variable
        -> m (Either Errno BS.ByteString) -- ^ The raw bytes of the UEFI
                                          -- variable, or an error if one
                                          -- occurred.
    default getEfiVar ::
        (MonadTrans t, MonadEfi m1, m ~ t m1) =>
        UUID -> BS.ByteString -> m (Either Errno BS.ByteString)
    getEfiVar vendor name = lift $ getEfiVar vendor name

    -- | Create a UEFI variable or modify the value of an existing variable.
    setEfiVar :: UUID -- ^ The vendor GUID the variable is defined under
        -> BS.ByteString -- ^ The UCS-2LE encoded name of the variable
        -> Word32 -- ^ A bitset representing the attributes to set on the
                  -- variable
        -> BS.ByteString -- ^ The value of the variable as bytes. See the [UEFI
                         -- Specification]
                         -- (https://uefi.org/specs/UEFI/2.10/index.html) for
                         -- specifics.
        -> m (Either Errno ()) -- ^ Any possible errors that may have occurred
    default setEfiVar ::
        (MonadTrans t, MonadEfi m1, m ~ t m1) =>
        UUID -> BS.ByteString -> Word32 -> BS.ByteString -> m (Either Errno ())
    setEfiVar vendor name attrib dataString =
        lift $ setEfiVar vendor name attrib dataString

instance (MonadEfi m) => MonadEfi (ContT r m)
instance (MonadEfi m) => MonadEfi (ExceptT e m)
instance (MonadEfi m) => MonadEfi (ReaderT r m)
instance (MonadEfi m, Monoid w) => MonadEfi (RWST r w s m)
instance (MonadEfi m) => MonadEfi (StateT s m)
instance (MonadEfi m, Monoid w) => MonadEfi (WriterT w m)

-- | Clear the currently set boot override. May throw an error if no override
-- is currently set.
clearBootNext :: (MonadEfi m) => m (Either Text ())
clearBootNext =
    ignoreErr <$> clearEfiVar efiGlobalVariable (encodeUtf16LE "BootNext")
  where
    ignoreErr (Left e) = if e == eNOENT then Right () else Left (showErrno e)
    ignoreErr (Right x) = Right x

-- | Get the numeric ID of the target used to boot the system.
getBootCurrent :: (MonadEfi m) => m (Either Text Word16)
getBootCurrent =
    bimap showErrno fromBytes
        <$> getEfiVar efiGlobalVariable (encodeUtf16LE "BootCurrent")

-- | Get the next target override the system is configured to boot. Returns an
-- error if no override is set. In that case, the first target in the boot order
-- is used.
getBootNext :: (MonadEfi m) => m (Either Text Word16)
getBootNext =
    bimap showErrno fromBytes
        <$> getEfiVar efiGlobalVariable (encodeUtf16LE "BootNext")

-- | Get the full entry describing a boot target by its numeric ID.
getBootOption :: (MonadEfi m) => Word16 -> m (Either Text BootOption)
getBootOption target = do
    let name = encodeUtf16LE "Boot" <> encodeUtf16LE (wordToHex target)
    runExceptT $ do
        rawOpt <- ExceptT $ first showErr <$> getEfiVar efiGlobalVariable name
        let attr = fromBytes $ BS.take 4 rawOpt
            pathListLen = fromBytes $ BS.take 2 $ BS.drop 4 rawOpt
            desc = decodeUtf16LE $ (BS.take =<< findNull) $ BS.drop 6 rawOpt
        pure $ BootOption target attr pathListLen desc
  where
    showErr e
        | e == eNOENT = "boot option does not exist"
        | otherwise = showErrno e
    wordToHex word =
        T.pack $ foldr
            (((:) . hexDigit) . fromIntegral . (.&. 0x0f) . shiftR word)
            []
            [12,8..0]
    hexDigit i
        | (i >= 0) && (i <= 9) = toEnum (fromEnum '0' + i)
        | (i >= 10) && (i <= 15) = toEnum (fromEnum 'A' + i - 10)
    findNull = maybe 0 (* 2) . elemIndex (BS.pack [0, 0]) . chunk 2

-- | Get the current boot order as an ordered list of numeric IDs.
getBootOrder :: (MonadEfi m) => m (Either Text [Word16])
getBootOrder =
    bimap showErrno (fmap fromBytes . chunk 2)
        <$> getEfiVar efiGlobalVariable (encodeUtf16LE "BootOrder")

-- | Set the next target override for the system to boot by its numeric ID.
setBootNext :: (MonadEfi m) => Word16 -> m (Either Text ())
setBootNext =
    (first showErrno <$>)
        . setEfiVar efiGlobalVariable (encodeUtf16LE "BootNext") attrib
        . toByteString
  where
    attrib =
        efiVariableNonVolatile
            .|. efiVariableBootserviceAccess
            .|. efiVariableRuntimeAccess
    toByteString = BS.toStrict . toLazyByteString . word16LE

-- | An individual target available to the UEFI boot manager.
data BootOption = BootOption
    { bootId :: !Word16 -- ^ The numeric ID corresponding to a boot target
    , bootAttributes :: !Word32 -- ^ A bitset representing the boot target's
                                -- attributes
    , bootFilePathListLength :: !Word16 -- ^ TODO: Parse boot file path lists
    , bootDescription :: !Text -- ^ The boot target description

    -- TODO:
    -- , bootFilePath :: [BootFilePath]
    -- , bootOptionalData :: BS.ByteString
    }

chunk :: Int -> BS.ByteString -> [BS.ByteString]
chunk k bs
    | BS.null bs = []
    | otherwise = BS.take k bs : chunk k (BS.drop k bs)

fromBytes :: (Bits a, Integral a) => BS.ByteString -> a
fromBytes = BS.foldr (\i acc -> (acc `shiftL` 8) .|. fromIntegral i) 0

showErrno :: Errno -> Text
showErrno e = T.pack $ show (errnoToIOError "" e Nothing Nothing)
