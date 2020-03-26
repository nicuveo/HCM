{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}



-- module

module Card ( CardSet(..)
            , CardClass(..)
            , CardCost(..)
            , CardId(..)
            , CardRarity(..)
            , CardName(..)
            , CardQuantity(..)
            , Card(..)
            , cardSets
            , cardStandardSets
            , cardClasses
            , cardRarities
            , isStandard
            , isLegendary
            , incrQuantity
            , decrQuantity
            , setQuantity
            ) where



-- imports

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Function
import           Data.Typeable



-- exported types

data CardSet = Classic
             | HallOfFame
             | GoblinsVsGnomes
             | GrandTournament
             | WhispersOldGods
             | GangsOfGadgetzan
             | JourneyToUngoro
             | KnightsFrozenThrone
             | KoboldsAndCatacombs
             | Witchwood
             | Boomsday
             | RastakhansRumble
             | RiseOfShadows
             | SaviorsOfUldum
             | DescentOfDragons
             | AshesOfOutlands
             deriving (Eq, Ord, Enum, Bounded, Typeable)

data CardClass = DemonHunter
               | Druid
               | Hunter
               | Mage
               | Paladin
               | Priest
               | Rogue
               | Shaman
               | Warlock
               | Warrior
               | Neutral
               deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

data CardRarity = Common
                | Rare
                | Epic
                | Legendary
                deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

data CardQuantity = Zero
                  | One
                  | Many
                  deriving (Eq, Ord, Enum, Bounded, Typeable)

newtype CardName = CardName { getCardName :: String }
                 deriving (Eq, Ord, FromJSON)

newtype CardCost = CardCost { getCardCost :: Int }
                 deriving (Eq, Ord, FromJSON, Typeable)

newtype CardId = CardId { getCardId :: String }
               deriving (Eq, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data Card = Card { cardId       :: CardId,
                   cardSet      :: CardSet,
                   cardClass    :: CardClass,
                   cardCost     :: CardCost,
                   cardRarity   :: CardRarity,
                   cardName     :: CardName,
                   cardQuantity :: Maybe CardQuantity
                 } deriving (Show)




-- instances

instance Show CardSet where
  show Classic             = "Classic"
  show HallOfFame          = "Hall of Fame"
  show GoblinsVsGnomes     = "GvG"
  show GrandTournament     = "TGT"
  show WhispersOldGods     = "Old Gods"
  show GangsOfGadgetzan    = "Gadgetzan"
  show JourneyToUngoro     = "Un'Goro"
  show KnightsFrozenThrone = "Frozen Throne"
  show KoboldsAndCatacombs = "Kobolds"
  show Witchwood           = "Witchwood"
  show Boomsday            = "Boomsday"
  show RastakhansRumble    = "Rastakhan's Rumble"
  show RiseOfShadows       = "Rise of Shadows"
  show SaviorsOfUldum      = "Saviors of Uldum"
  show DescentOfDragons    = "Descent of Dragons"
  show AshesOfOutlands     = "Ashes of Outlands"

instance Show CardQuantity where
  show Zero = "0"
  show One  = "1"
  show Many = "2"

instance Show CardId where
  show = getCardId

instance Show CardName where
  show = getCardName

instance Show CardCost where
  show = show . getCardCost


instance Read CardCost where
  readsPrec x s = [(CardCost i, z) | (i, z) <- readsPrec x s]


instance Eq Card where
  (==) = (==) `on` cardId

instance Ord Card where
  compare = compare `on` extract
    where extract card = ( cardClass card
                         , cardCost  card
                         , cardName  card
                         , cardId    card
                         )


instance FromJSON CardClass where
  parseJSON (String "DEMONHUNTER") = return DemonHunter
  parseJSON (String "DRUID"      ) = return Druid
  parseJSON (String "HUNTER"     ) = return Hunter
  parseJSON (String "MAGE"       ) = return Mage
  parseJSON (String "PALADIN"    ) = return Paladin
  parseJSON (String "PRIEST"     ) = return Priest
  parseJSON (String "ROGUE"      ) = return Rogue
  parseJSON (String "SHAMAN"     ) = return Shaman
  parseJSON (String "WARLOCK"    ) = return Warlock
  parseJSON (String "WARRIOR"    ) = return Warrior
  parseJSON (String "NEUTRAL"    ) = return Neutral
  parseJSON (String s)             = expecting    "CardClass" s
  parseJSON v                      = typeMismatch "CardClass" v

instance FromJSON CardRarity where
  parseJSON (String "COMMON"   ) = return Common
  parseJSON (String "RARE"     ) = return Rare
  parseJSON (String "EPIC"     ) = return Epic
  parseJSON (String "LEGENDARY") = return Legendary
  parseJSON (String s)           = expecting    "CardRarity" s
  parseJSON v                    = typeMismatch "CardRarity" v

instance FromJSON CardSet where
  parseJSON (String "EXPERT1"     ) = return Classic
  parseJSON (String "HOF"         ) = return HallOfFame
  parseJSON (String "GVG"         ) = return GoblinsVsGnomes
  parseJSON (String "TGT"         ) = return GrandTournament
  parseJSON (String "OG"          ) = return WhispersOldGods
  parseJSON (String "GANGS"       ) = return GangsOfGadgetzan
  parseJSON (String "UNGORO"      ) = return JourneyToUngoro
  parseJSON (String "ICECROWN"    ) = return KnightsFrozenThrone
  parseJSON (String "LOOTAPALOOZA") = return KoboldsAndCatacombs
  parseJSON (String "GILNEAS"     ) = return Witchwood
  parseJSON (String "BOOMSDAY"    ) = return Boomsday
  parseJSON (String "TROLL"       ) = return RastakhansRumble
  parseJSON (String "DALARAN"     ) = return RiseOfShadows
  parseJSON (String "ULDUM"       ) = return SaviorsOfUldum
  parseJSON (String "DRAGONS"     ) = return DescentOfDragons
  parseJSON (String "BLACK_TEMPLE") = return AshesOfOutlands
  parseJSON (String s)              = expecting    "CardSet" s
  parseJSON v                       = typeMismatch "CardSet" v

instance FromJSON CardQuantity where
  parseJSON = fmap toEnum . parseJSON

instance FromJSON Card where
  parseJSON = withObject "Card" $ \o ->
    Card <$> o .: "id"
         <*> o .: "set"
         <*> o .: "cardClass"
         <*> o .: "cost"
         <*> o .: "rarity"
         <*> o .: "name"
         <*> return Nothing

instance ToJSON CardQuantity where
    toJSON = toJSON . fromEnum



-- exported functions

cardSets :: [CardSet]
cardSets = [minBound..maxBound]

cardStandardSets :: [CardSet]
cardStandardSets = [ Classic
                   , RiseOfShadows
                   , SaviorsOfUldum
                   , DescentOfDragons
                   , AshesOfOutlands
                   ]

cardClasses :: [CardClass]
cardClasses = [minBound..maxBound]

cardRarities :: [CardRarity]
cardRarities = [minBound..maxBound]

isLegendary :: Card -> Bool
isLegendary c = cardRarity c == Legendary

isStandard :: CardSet -> Bool
isStandard s = s `elem` cardStandardSets

incrQuantity :: Card -> Card
incrQuantity card = card { cardQuantity = incr (cardQuantity card) (cardRarity card) }

decrQuantity :: Card -> Card
decrQuantity card = card { cardQuantity = decr $ cardQuantity card }

setQuantity :: CardQuantity -> Card -> Card
setQuantity q c
  | isLegendary c = c { cardQuantity = Just $ min One q }
  | otherwise     = c { cardQuantity = Just q }



-- internal functions

expecting :: (Monad m, Show a) => String -> a -> m b
expecting t v = fail $ "expecting a " ++ t ++ ", got: " ++ show v

incr :: Maybe CardQuantity -> CardRarity -> Maybe CardQuantity
incr Nothing     _         = Just One
incr (Just Zero) _         = Just One
incr (Just One ) Legendary = Just One
incr (Just One ) _         = Just Many
incr (Just Many) _         = Just Many

decr :: Maybe CardQuantity -> Maybe CardQuantity
decr (Just Many) = Just One
decr _           = Just Zero
