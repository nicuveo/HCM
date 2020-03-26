{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}



-- module

module CardMaps ( CardMap
                , CardMapW(..)
                , QuantityMap
                , findByName
                , updateQuantity
                , dumpQuantity
                , missingQuantity
                ) where

import           Control.Monad.Except
import           Data.Aeson
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Vector          as V

import           Card



-- exported types

type IdMap a     = M.Map CardId a
type CardMap     = IdMap Card
type QuantityMap = IdMap CardQuantity

newtype CardMapW = CardMapW { getCardMap :: CardMap }



-- instances

instance FromJSON CardMapW where
  parseJSON = withArray "CardMap" $ return
                                  . CardMapW
                                  . V.foldl' insertIfResult M.empty
                                  . fmap fromJSON
    where insertIfResult m (Success c) = insertCard c m
          insertIfResult m (Error   _) = m



-- exported functions

findByName :: MonadError String m => CardMap -> CardName -> m CardId
findByName cmap cname =
  case M.size matches of
    1 -> return $ cardId $ head $ M.elems matches
    0 -> throwError $ "found no card named " ++ getCardName cname
    _ -> throwError $ "found more than one card named " ++ getCardName cname ++ "!!?"
  where matches = M.filter (\c -> cardName c == cname) cmap

updateQuantity :: QuantityMap -> CardMap -> CardMap
updateQuantity = flip $ M.foldlWithKey' combine
    where combine c i q = M.adjust (setQuantity q) i c

dumpQuantity :: CardMap -> QuantityMap
dumpQuantity = M.mapMaybe cardQuantity

missingQuantity :: CardMap -> CardMap
missingQuantity = M.filter $ isNothing . cardQuantity



-- internal functions

insertCard :: Card -> CardMap -> CardMap
insertCard c cm = M.insert (cardId c) c cm
