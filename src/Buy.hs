-- module

module Buy (
    packValue
    ) where



-- imports

import           Control.Applicative
import           Data.IxSet

import           Card



-- exported functions

packValue :: Fractional a => CardSet -> Cards -> a
packValue s c = 5 * sum [packChance r * rarityValue (c @= s) r | r <- cardRarities]



-- internal functions

packChance :: Fractional a => CardRarity -> a
packChance Common    = 0.7142
packChance Rare      = 0.228
packChance Epic      = 0.0458
packChance Legendary = 0.012

craftValue :: Fractional a => CardRarity -> a
craftValue Common    =   40
craftValue Rare      =  100
craftValue Epic      =  400
craftValue Legendary = 1600

disenchantValue :: Fractional a => CardRarity -> a
disenchantValue Common    =   5
disenchantValue Rare      =  20
disenchantValue Epic      = 100
disenchantValue Legendary = 400

rarityValue :: Fractional a => Cards -> CardRarity -> a
rarityValue c r = disenchantValue r * p + craftValue r * (1 - p)
    where p = proba (c @= r) r

proba :: Fractional a => Cards -> CardRarity -> a
proba c r = count r c / total r c
    where total Legendary c = realToFrac $ 1 * size c
          total _         c = realToFrac $ 2 * size c
          count Legendary c = realToFrac $ sum $ min 1 . fromEnum . cardQuantity <$> toList c
          count _         c = realToFrac $ sum $         fromEnum . cardQuantity <$> toList c
