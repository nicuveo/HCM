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
    CardId(..),
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
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy hiding (concat, map, pack, unpack)
import           Data.Function
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
             | WhispersOldGods
             | GangsOfGadgetzan
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
                 deriving (Eq, Ord, Typeable)

newtype CardCost = CardCost { getCardCost :: Int }
                 deriving (Eq, Ord, Typeable)

newtype CardId = CardId { getCardId :: String }
               deriving (Eq, Ord, Typeable)

data Card = Card {
    cardId       :: CardId,
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
    show Classic          = "Classic"
    show GoblinsVsGnomes  = "GvG"
    show GrandTournament  = "TGT"
    show WhispersOldGods  = "Old Gods"
    show GangsOfGadgetzan = "Gadgetzan"

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
    compare (Card i1 s1 h1 c1 r1 _ _) (Card i2 s2 h2 c2 r2 _ _) =
        compare (h1, c1, s1, r1, i1) (h2, c2, s2, r2, i2)


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
    parseJSON (String "EXPERT1") = return $ Classic
    parseJSON (String "GVG"    ) = return $ GoblinsVsGnomes
    parseJSON (String "TGT"    ) = return $ GrandTournament
    parseJSON (String "OG"     ) = return $ WhispersOldGods
    parseJSON (String "GANGS"  ) = return $ GangsOfGadgetzan
    parseJSON (String s)         = expecting    "CardSet" s
    parseJSON v                  = typeMismatch "CardSet" v

instance FromJSON CardQuantity where
    parseJSON = fmap toEnum . parseJSON

instance FromJSON CardId where
    parseJSON = fmap CardId . parseJSON

instance FromJSON CardName where
    parseJSON = fmap CardName . parseJSON

instance FromJSON CardCost where
    parseJSON = fmap CardCost . parseJSON

instance FromJSON Card where
    parseJSON (Object o) = do
      card <- Card
              <$> (o .: "id")
              <*> (toEnum <$> o .: "set")
              <*> (toEnum <$> o .: "class")
              <*> (o .: "cost")
              <*> (toEnum <$> o .: "rarity")
              <*> (o .: "name")
              <*> (toEnum <$> o .: "quantity")
      if cardRarity card == Legendary && cardQuantity card == Many
      then return $ card {cardQuantity = One}
      else return $ card
    parseJSON v          = typeMismatch "Card" v

instance FromJSON Cards where
    parseJSON (Array a) = S.fromList <$> sequence (V.toList $ parseJSON <$> a)
    parseJSON v         = typeMismatch "Cards" v


instance ToJSON Card where
    toJSON c = object [ "id"       .= (getCardId   $ cardId       c)
                      , "set"      .= (fromEnum    $ cardSet      c)
                      , "class"    .= (fromEnum    $ cardClass    c)
                      , "cost"     .= (getCardCost $ cardCost     c)
                      , "rarity"   .= (fromEnum    $ cardRarity   c)
                      , "name"     .= (getCardName $ cardName     c)
                      , "quantity" .= (fromEnum    $ cardQuantity c)
                      ]

instance ToJSON Cards where
    toJSON = Array . V.fromList . map toJSON . S.toList


instance Indexable Card where
  empty = ixSet [ ixFun (pure . cardId)
                , ixFun (pure . cardSet)
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

parseCard :: Object -> Parser (Maybe Card)
parseCard o = do
    let set = parseMaybe (.: "set") o
    case set of
        Just s -> liftM Just $ Card
             <$> o .: "id"
             <*> return s
             <*> o .: "playerClass"
             <*> o .: "cost"
             <*> o .: "rarity"
             <*> o .: "name"
             <*> return Zero
        _ -> return Nothing

parseCards :: Array -> Parser Cards
parseCards a = S.fromList <$> catMaybes <$> (sequence $ V.toList $ withObject "Card" parseCard <$> a)
