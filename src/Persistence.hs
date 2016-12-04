{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}



-- module

module Persistence (loadCards, saveCards) where



-- imports

import           Control.Applicative
import           Control.Monad.Except
import           Data.Aeson           (eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import           System.Directory
import           System.FilePath

import           Card



-- exported functions

loadCards :: (MonadIO m, MonadError String m, Functor m) => FilePath -> String -> m (Maybe Cards)
loadCards path app = do
    file   <- liftIO $ getFile path app
    exists <- liftIO $ doesFileExist file
    if exists
        then liftM Just $ decodeM =<< liftIO (B.readFile file)
        else return Nothing

saveCards :: (MonadIO m, MonadError e m) => FilePath -> String -> Cards -> m ()
saveCards path app cards = do
    file <- liftIO $ getFile path app
    liftIO $ B.writeFile file $ encode cards



-- internal functions

decodeM :: MonadError String m => B.ByteString -> m Cards
decodeM = either throwError return . eitherDecode

getFile :: FilePath -> String -> IO FilePath
getFile path app = do
  dirName <- getAppUserDataDirectory app
  createDirectoryIfMissing True dirName
  return $ dirName </> path
