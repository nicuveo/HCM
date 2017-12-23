-- module

module Buy ( packValue
           , craftValue
           , disenchantValue
           ) where



-- imports

import qualified Data.Map as M

import           Card
import           CardMaps
import           Filters



-- exported functions

packValue :: Fractional a => CardSet -> CardMap -> a
packValue s c = 5 * sum [packChance r * rarityValue (c @= s) r | r <- cardRarities]

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



-- internal functions

packChance :: Fractional a => CardRarity -> a
packChance Common    = 0.7142
packChance Rare      = 0.228
packChance Epic      = 0.0458
packChance Legendary = 0.012

rarityValue :: Fractional a => CardMap -> CardRarity -> a
rarityValue cm r = disenchantValue r * p + craftValue r * (1 - p)
    where p = proba (cm @= r) r

proba :: Fractional a => CardMap -> CardRarity -> a
proba cm Legendary =
  if all (\c -> cardQuantity c > Just Zero) $ M.elems cm
  then 1
  else 0
proba cm _ = count / total
    where total = realToFrac $ 2 * M.size cm
          count = realToFrac $ sum $ maybe 0 fromEnum . cardQuantity <$> M.elems cm
