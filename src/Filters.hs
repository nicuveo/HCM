{-# LANGUAGE FlexibleInstances #-}



-- module

module Filters where



-- imports

import qualified Data.Map as M

import           Card
import           CardMaps



-- types

type Predicate = Card -> Bool

class PredicateGen a where
  genPred :: a -> Predicate


instance PredicateGen CardId      where genPred x = (x ==) . cardId
instance PredicateGen CardSet     where genPred x = (x ==) . cardSet
instance PredicateGen CardClass   where genPred x = (x ==) . cardClass
instance PredicateGen CardCost    where genPred x = (x ==) . cardCost
instance PredicateGen CardRarity  where genPred x = (x ==) . cardRarity
instance PredicateGen CardName    where genPred x = (x ==) . cardName
instance PredicateGen Predicate   where genPred x = x

instance PredicateGen CardQuantity where
    genPred x = maybe True (x ==) . cardQuantity

instance PredicateGen a => PredicateGen [a] where
    genPred xs c = or [genPred x c | x <- xs]



-- functions

(@=) :: PredicateGen a => CardMap -> a -> CardMap
(@=) = flip keep

keep :: PredicateGen a => a -> CardMap -> CardMap
keep = M.filter . genPred

missing :: Predicate
missing card
  | cardRarity card == Legendary = genPred [Zero]      card
  | otherwise                    = genPred [Zero, One] card

owned :: Predicate
owned = genPred [One, Many]

standard :: Predicate
standard = isStandard . cardSet
