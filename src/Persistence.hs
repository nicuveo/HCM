{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}



-- module

module Persistence ( getFilePath
                   , loadCardMap
                   , loadQuantityMap
                   , saveQuantityMap
                   ) where



-- imports

import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           System.Directory
import           System.FilePath

import           CardMaps



-- exported functions

getFilePath :: String -> FilePath -> IO FilePath
getFilePath app path = do
  dirName <- getAppUserDataDirectory app
  createDirectoryIfMissing True dirName
  return $ dirName </> path

loadCardMap :: (MonadIO m, MonadError String m, Functor m) => String -> FilePath -> m (Maybe CardMap)
loadCardMap = loadMap

loadQuantityMap :: (MonadIO m, MonadError String m, Functor m) => String -> FilePath -> m (Maybe QuantityMap)
loadQuantityMap = loadMap

saveQuantityMap :: MonadIO m => String -> FilePath -> QuantityMap -> m ()
saveQuantityMap app path cmap = do
    file <- liftIO $ getFilePath app path
    liftIO $ B.writeFile file $ encode cmap




-- internal functions

loadMap :: (MonadIO m, MonadError String m, Functor m, FromJSON a) => String -> FilePath -> m (Maybe a)
loadMap app path = do
    file   <- liftIO $ getFilePath app path
    exists <- liftIO $ doesFileExist file
    if exists
        then fmap Just $ decodeM =<< liftIO (B.readFile file)
        else return Nothing

decodeM :: (MonadError String m, FromJSON a) => B.ByteString -> m a
decodeM = either throwError return . eitherDecode'
