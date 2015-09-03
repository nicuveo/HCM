{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}



-- module

module Card (
    CardSet(..),
    CardClass(..),
    CardCost(..),
    CardRarity(..),
    CardName(..),
    CardQuantity(..),
    Card(..),
    Cards,
    cardSets,
    cardClasses,
    cardRarities,
    readCardsRef,
    readCardsRefM,
    ) where



-- imports

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Error
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy hiding (concat, map, pack, unpack)
import           Data.IxSet           as S
import           Data.List            (stripPrefix)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (pack, unpack)
import           Data.Typeable        hiding (Proxy)
import           Data.Vector          as V (fromList, toList)
import           Text.Read            (readMaybe)



-- exported types

data CardSet = Classic
             | GoblinsVsGnomes
             | GrandTournament
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
                  | More
                  deriving (Eq, Ord, Enum, Bounded, Typeable)

newtype CardName = CardName { runCardName :: String }
                 deriving (Eq, Ord, Typeable)

newtype CardCost = CardCost { runCardCost :: Int }
                  deriving (Eq, Ord, Typeable)

data Card = Card {
    cardSet      :: CardSet,
    cardClass    :: CardClass,
    cardCost     :: CardCost,
    cardRarity   :: CardRarity,
    cardName     :: CardName,
    cardQuantity :: CardQuantity
    } deriving (Show, Typeable)

type Cards = IxSet Card



-- instances

instance Show CardSet where
    show Classic         = "Classic"
    show GoblinsVsGnomes = "GvG"
    show GrandTournament = "TGT"

instance Show CardName where
    show = runCardName

instance Show CardCost where
    show = show . runCardCost

instance Show CardQuantity where
    show Zero = "0"
    show One  = "1"
    show More = "2"


instance Eq Card where
    c1 == c2 = n1 == n2
        where n1 = cardName c1
              n2 = cardName c2

instance Ord Card where
    compare (Card s1 h1 c1 r1 n1 _) (Card s2 h2 c2 r2 n2 _) =
        compare (h1, c1, s1, r1, n1) (h2, c2, s2, r2, n2)


instance FromJSON CardClass where
    parseJSON (String s) = maybe (expecting "CardClass" s) return $ readMaybe $ unpack s
    parseJSON v          = typeMismatch "CardClass" v

instance FromJSON CardRarity where
    parseJSON (String s) = maybe (expecting "CardRarity" s) return $ readMaybe $ unpack s
    parseJSON v          = typeMismatch "CardRarity" v

instance FromJSON Card where
    parseJSON (Object o) = Card
                           <$> (toEnum   <$> o .: "set")
                           <*> (toEnum   <$> o .: "class")
                           <*> (CardCost <$> o .: "cost")
                           <*> (toEnum   <$> o .: "rarity")
                           <*> (CardName <$> o .: "name")
                           <*> (toEnum   <$> o .: "quantity")
    parseJSON v          = typeMismatch "Card" v

instance FromJSON Cards where
    parseJSON (Array a) = S.fromList <$> sequence (V.toList $ parseJSON <$> a)
    parseJSON v         = typeMismatch "Cards" v


instance ToJSON Card where
    toJSON c = object [ "set"      .= (fromEnum    $ cardSet      c)
                      , "class"    .= (fromEnum    $ cardClass    c)
                      , "cost"     .= (runCardCost $ cardCost     c)
                      , "rarity"   .= (fromEnum    $ cardRarity   c)
                      , "name"     .= (runCardName $ cardName     c)
                      , "quantity" .= (fromEnum    $ cardQuantity c)
                      ]

instance ToJSON Cards where
    toJSON = Array . V.fromList . map toJSON . S.toList


instance Indexable Card where
  empty = ixSet [ ixFun (pure . cardSet)
                , ixFun (pure . cardClass)
                , ixFun (pure . cardCost)
                , ixFun (pure . cardRarity)
                , ixFun (pure . cardName)
                , ixFun (pure . cardQuantity)
                ]



-- exported functions

cardSets :: [CardSet]
cardSets = [minBound..maxBound]

cardClasses :: [CardClass]
cardClasses = [minBound..maxBound]

cardRarities :: [CardRarity]
cardRarities = [minBound..maxBound]

readCardsRef :: ByteString -> Either String Cards
readCardsRef s = eitherDecode' s >>= parseEither parseCards

readCardsRefM :: MonadError String m => ByteString -> m Cards
readCardsRefM = either throwError return . readCardsRef



-- internal functions

expecting :: (Monad m, Show a) => String -> a -> m b
expecting t v = fail $ "expecting a " ++ t ++ ", got: " ++ (show v)

parseCard :: CardSet -> Object -> Parser (Maybe Card)
parseCard s o = do
    collectible <- o .:? "collectible" .!= False
    if collectible
        then liftM Just $ Card
             <$> return s
             <*> o .:? "playerClass" .!= Neutral
             <*> (CardCost <$> o .: "cost")
             <*> o .:? "rarity" .!= Common
             <*> (CardName <$> o .: "name")
             <*> return Zero
        else return Nothing

parseCards :: Object -> Parser Cards
parseCards obj = S.fromList <$> concat <$> sequence (parseCardSet <$> cardSets)
    where parseCardSet set = do
              values <- obj .: pack (name set)
              let cards = withObject "Card" (parseCard set) <$> values
              catMaybes <$> sequence (V.toList cards)
          name Classic         = "Classic"
          name GoblinsVsGnomes = "Goblins vs Gnomes"
          name GrandTournament = "The Grand Tournament"
