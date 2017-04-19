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

import           Control.Applicative
import           Control.Arrow
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Function
import           Data.List           (stripPrefix)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (pack, unpack)
import           Data.Typeable
import qualified Data.Vector         as V
import           Text.Read           (readMaybe)



-- exported types

data CardSet = Classic
             | HallOfFame
             | GoblinsVsGnomes
             | GrandTournament
             | WhispersOldGods
             | GangsOfGadgetzan
             | JourneyToUngoro
             deriving (Eq, Ord, Enum, Bounded, Typeable)

data CardClass = Druid
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
                 deriving (Read, Eq, Ord, FromJSON, Typeable)

newtype CardId = CardId { getCardId :: String }
               deriving (Eq, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data Card = Card {
    cardId       :: CardId,
    cardSet      :: CardSet,
    cardClass    :: CardClass,
    cardCost     :: CardCost,
    cardRarity   :: CardRarity,
    cardName     :: CardName,
    cardQuantity :: Maybe CardQuantity
    } deriving (Show)




-- instances

instance Show CardSet where
    show Classic          = "Classic"
    show HallOfFame       = "Hall of Fame"
    show GoblinsVsGnomes  = "GvG"
    show GrandTournament  = "TGT"
    show WhispersOldGods  = "Old Gods"
    show GangsOfGadgetzan = "Gadgetzan"
    show JourneyToUngoro  = "Un'Goro"

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


instance Eq Card where
    (==) = (==) `on` cardId

instance Ord Card where
    compare (Card i1 _ h1 c1 _ _ _) (Card i2 _ h2 c2 _ _ _) =
        compare (h1, c1, i1) (h2, c2, i2)


instance FromJSON CardClass where
    parseJSON (String "DRUID"  ) = return Druid
    parseJSON (String "HUNTER" ) = return Hunter
    parseJSON (String "MAGE"   ) = return Mage
    parseJSON (String "PALADIN") = return Paladin
    parseJSON (String "PRIEST" ) = return Priest
    parseJSON (String "ROGUE"  ) = return Rogue
    parseJSON (String "SHAMAN" ) = return Shaman
    parseJSON (String "WARLOCK") = return Warlock
    parseJSON (String "WARRIOR") = return Warrior
    parseJSON (String "NEUTRAL") = return Neutral
    parseJSON (String s) = expecting    "CardClass" s
    parseJSON v          = typeMismatch "CardClass" v

instance FromJSON CardRarity where
    parseJSON (String "COMMON"   ) = return Common
    parseJSON (String "RARE"     ) = return Rare
    parseJSON (String "EPIC"     ) = return Epic
    parseJSON (String "LEGENDARY") = return Legendary
    parseJSON (String s) = expecting    "CardRarity" s
    parseJSON v          = typeMismatch "CardRarity" v

instance FromJSON CardSet where
    parseJSON (String "EXPERT1") = return Classic
    parseJSON (String "HOF"    ) = return HallOfFame
    parseJSON (String "GVG"    ) = return GoblinsVsGnomes
    parseJSON (String "TGT"    ) = return GrandTournament
    parseJSON (String "OG"     ) = return WhispersOldGods
    parseJSON (String "GANGS"  ) = return GangsOfGadgetzan
    parseJSON (String "UNGORO" ) = return JourneyToUngoro
    parseJSON (String s)         = expecting    "CardSet" s
    parseJSON v                  = typeMismatch "CardSet" v

instance FromJSON CardQuantity where
    parseJSON = fmap toEnum . parseJSON

instance {-# OVERLAPPING #-} FromJSON (Maybe Card) where
    parseJSON = withObject "Card" $ \o -> do
        let set = parseMaybe (.: "set") o
        case set of
            Just s -> fmap Just $ Card
                <$> o .: "id"
                <*> return s
                <*> o .: "cardClass"
                <*> o .: "cost"
                <*> o .: "rarity"
                <*> o .: "name"
                <*> return Nothing
            _ -> return Nothing


instance ToJSON CardQuantity where
    toJSON = toJSON . fromEnum



-- exported functions

cardSets :: [CardSet]
cardSets = [minBound..maxBound]

cardStandardSets :: [CardSet]
cardStandardSets = [Classic, WhispersOldGods, GangsOfGadgetzan, JourneyToUngoro]

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
incr Nothing     _          = Just One
incr (Just Zero) _          = Just One
incr (Just One ) Legendary  = Just One
incr (Just One ) _          = Just Many
incr (Just Many) _          = Just Many

decr :: Maybe CardQuantity -> Maybe CardQuantity
decr (Just Many) = Just One
decr _           = Just Zero
