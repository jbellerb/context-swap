{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Main
Copyright   :  (c) jae beller 2024
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable

A command-line tool for modifying EFI boot order and hibernating.
-}

module Main where

import App.Monad (App, runApp)
import App.Options (defaultOptions, Options (..), options, usage)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except (handleError, liftEither, tryError, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bifunctor (first, second)
import Data.Bits (shiftR, (.&.))
import Data.List (find, intersperse, lookup)
import Data.Text (pack, Text)
import qualified Data.Text.IO as T (hPutStr, hPutStrLn, putStr, putStrLn)
import Data.Version (showVersion)
import Data.Word (Word16)
import Paths_context_swap (version)
import System.Boot.UEFI
    ( BootOption (..), clearBootNext, getBootCurrent, getBootNext
    ,  getBootOption, getBootOrder, setBootNext
    )
import System.Console.GetOpt (ArgOrder (..), getOpt)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.Hibernate (hibernate)
import System.IO (stderr)
import Text.Read (readEither)

-- | Parse the given command-line arguments as 'Options'.
parseArgs :: IO (Either (Maybe Text) Options)
parseArgs = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (o, n, []) -> case n of
            [] -> pure $ Right $ applyArgs Nothing o
            [t] -> pure $ Right $ applyArgs (parseTarget t) o
            _ -> pure $ Left Nothing
        (_, _, e) -> pure $ Left (Just $ mconcat (fmap pack e))
  where
    applyArgs target = foldr ($) defaultOptions{optTarget=target}
    parseTarget s = Just $ first (const $ pack s) (readEither s)

-- | Entrypoint. Parse the command line args and either print the requested
-- information or run the set boot and hibernate routine.
main :: IO ()
main = do
    progName <- pack <$> getProgName
    options <- parseArgs
    case options of
        Left e -> do
            forM_ e (T.hPutStr stderr)
            T.hPutStrLn stderr (usage progName)
            exitFailure
        Right o -> runApp progName o $ case o of
            Options{optShowVersion = True} -> liftIO $ do
                T.putStrLn $
                    progName <> " version " <> pack (showVersion version)
                exitSuccess
            Options{optShowBootOrder = True} -> bootOrder
            Options{optClearNext = True} -> clearBootNext >>= liftEither
            _ -> app

-- | Fetch the current boot order from UEFI.
fetchBootList :: App [(Text, BootOption)]
fetchBootList = do
    order <- getBootOrder >>= liftEither
    forM order $ \id -> do
        opt <- getBootOption id >>= liftEither
        pure (bootDescription opt, opt)

-- | List the current boot order and exit.
bootOrder :: App ()
bootOrder = do
    liftIO $ T.putStrLn "Boot Order:"
    fetchBootList >>= mapM_
        ( \(name, BootOption{bootId=id}) ->
            liftIO $ T.putStrLn $ "  " <> name <> " (" <> showHex16 id <> ")"
        )
    handleError (const $ pure ()) $ do
        next <- getBootNext >>= liftEither
        name <- bootDescription <$> (getBootOption next >>= liftEither)
        liftIO $ T.putStrLn "\nNext boot:"
        liftIO $ T.putStrLn $ "  " <> name <> " (" <> showHex16 next <> ")"
    liftIO exitSuccess

-- | The core of the functionality. After finding what target to set, change the
-- boot override to that (or not if -n was given) and then hibernate (or not if
-- -q was given).
app :: App ()
app = do
    verbose <- asks optVerbose
    noHibernate <- asks optNoHibernate
    noTarget <- asks optNoTarget
    unless noTarget $ do
        target <-
            resolveTarget verbose >>=
                maybe (throwError "boot option does not exist") pure
        when verbose $ liftIO $ do
            T.putStr "setting next boot to "
            T.putStr $ bootDescription target
            T.putStrLn $ " (" <> showHex16 (bootId target) <> ")"
        setBootNext (bootId target) >>= liftEither
    unless noHibernate $ do
        liftIO hibernate

-- | Determine what target to set the next boot to. If an ID was given, use
-- that. If a name was given with no ID, search the boot order for a matching
-- target and use that. If neither were given, find the highest entry in the
-- boot order that isn't what is currently booted.
resolveTarget :: Bool -> App (Maybe BootOption)
resolveTarget verbose = do
    bootList <- fetchBootList
    when verbose $ liftIO $ do
        T.putStr "current boot order: "
        mapM_ T.putStr (intersperse ", " (fmap fst bootList) <> ["\n"])
    target <- asks optTarget
    case target of
        (Just (Right n)) -> pure $ find ((n ==) . bootId) $ fmap snd bootList
        (Just (Left s)) -> pure $ lookup s bootList
        Nothing -> do
            currentId <- getBootCurrent >>= liftEither
            when verbose $ liftIO $ do
                T.putStr "currently booted: "
                T.putStrLn $ showHex16 currentId
            pure $ find ((currentId /=) . bootId) $ fmap snd bootList

-- | Show 'Word16' as 'Text', padded with zeros in from to 4 character and
-- preceded by "0x".
showHex16 :: Word16 -> Text
showHex16 w = "0x" <> pack (foldr (((:) . hexChar) . extract w) [] [12,8..0])
  where
    hexChar i
        | (i >= 0) && (i <= 9) = toEnum (fromEnum '0' + i)
        | (i >= 10) && (i <= 15) = toEnum (fromEnum 'a' + i - 10)
    extract w = fromIntegral . (.&. 0x0f) . shiftR w
