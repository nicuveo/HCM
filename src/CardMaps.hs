{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}



-- module

module CardMaps where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy hiding (head, concat, map, pack, unpack)
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Vector          as V

import Card

type IdMap a     = M.Map CardId a
type CardMap     = IdMap Card
type QuantityMap = IdMap CardQuantity

instance {-# OVERLAPPING #-} FromJSON CardMap where
    parseJSON = withArray "CardMap" $
                fmap (V.foldl' insertCard M.empty) . sequence . fmap parseJSON



-- exported functions

findByName :: MonadError String m => CardMap -> CardName -> m CardId
findByName cmap cname = case M.size matches of
                            1 -> return $ cardId $ head $ M.elems $ matches
                            0 -> throwError $ "found no card named " ++ getCardName cname
                            _ -> throwError $ "found more than one card named " ++ getCardName cname ++ "!!?"
    where matches = M.filter (\c -> cardName c == cname) cmap

updateQuantity :: QuantityMap -> CardMap -> CardMap
updateQuantity qs cs = M.foldlWithKey' combine cs qs
    where combine cs id q = M.adjust (setQuantity q) id cs

dumpQuantity :: CardMap -> QuantityMap
dumpQuantity = M.mapMaybe cardQuantity

missingQuantity :: CardMap -> CardMap
missingQuantity = M.filter $ isNothing . cardQuantity



-- internal functions

insertCard :: CardMap -> Maybe Card -> CardMap
insertCard m (Just c) = M.insert (cardId c) c m
insertCard m Nothing  = m

insertQuantity :: QuantityMap -> (CardId, CardQuantity) -> QuantityMap
insertQuantity m (i,q) = M.insert i q m
