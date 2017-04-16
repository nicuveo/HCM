{-# LANGUAGE FlexibleInstances      #-}

module Filters where

import qualified Data.Map             as M

import           Card
import           CardMaps

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


instance {-# OVERLAPPING #-} Monoid Predicate where
    mempty = const True
    mappend p1 p2 c = p1 c && p2 c


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