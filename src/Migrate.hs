{-# LANGUAGE OverloadedStrings          #-}



-- module

module Migrate (migrate) where



-- import

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map             as M
import qualified Data.Vector          as V
import           System.Directory
import           System.FilePath

import CardMaps
import Log
import Persistence



-- constants

oldFile = "cards.json"
v0File  = "quantity.v0.json"



-- exported function

migrate :: String -> IO ()
migrate app = do
    migrateOldToV0 app



-- internal; old format

newtype FormatOld = FormatOld { getV0 :: QuantityMap }

instance FromJSON FormatOld where
    parseJSON = withArray "format: old cards" $ fmap (FormatOld . M.fromList) . sequence . V.toList . fmap parseOldCard

parseOldCard = withObject "format: old card" $ \o -> do
                                                     ci <- o .: "id"
                                                     cq <- o .: "quantity"
                                                     return (ci, cq)

migrateOldToV0 :: String -> IO ()
migrateOldToV0 app = do
    file   <- getFilePath app oldFile
    exists <- doesFileExist file
    when exists $ do
        logInfo "found old file, migrating to v0"
        old <- either fail return . eitherDecode' =<< B.readFile file
        saveQuantityMap app v0File $ getV0 old
        renameFile file $ file ++ ".bak"
