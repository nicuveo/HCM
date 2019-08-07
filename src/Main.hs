{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}



-- imports

import           Control.Monad.Except  hiding (fix)
import qualified Data.ByteString.Lazy  as B
import           Data.Foldable
import           Data.List             as L
import           Data.List.Split       as L
import qualified Data.Map              as M
import           Data.Maybe
import           Data.String
import qualified Data.Text.Format      as T
import qualified Data.Text.Lazy        as T
import qualified Data.Text.Lazy.IO     as T
import           Data.Typeable
import           Network.HTTP.Simple   hiding (Proxy)
import           Prelude               hiding (pred)
import           Safe
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Tabular
import           Text.Tabular.AsciiArt

import           Buy
import           Card
import           CardMaps
import           Filters
import           Log
import           Migrate
import           Persistence



-- config

cardsURL, cardsFile, quantityFile, appName :: IsString a => a
cardsURL     = "https://api.hearthstonejson.com/v1/latest/enUS/cards.collectible.json"
cardsFile    = "cards.collectible.json"
quantityFile = "quantity.v0.json"
appName      = "hcm"



-- helpers

run :: MonadIO m => ExceptT String m a -> m a
run = runExceptT >=> either (liftIO . exit) return

exit :: MonadIO m => String -> m a
exit s = logError s >> liftIO exitFailure

load :: MonadIO m => m (Maybe CardMap)
load = run $ do
    cs <- loadCardMap     appName cardsFile
    qs <- loadQuantityMap appName quantityFile
    return $ updateQuantity <$> qs <*> cs

loadOrDie :: MonadIO m => m CardMap
loadOrDie = load >>= maybe (exit "card collection not found, please update") return

loadOrDieAndCheck :: MonadIO m => m CardMap
loadOrDieAndCheck = do
  cards <- loadOrDie
  liftIO $ check cards
  return cards

save :: MonadIO m => CardMap -> m ()
save = saveQuantityMap appName quantityFile . dumpQuantity

adjustCardByName :: MonadError String m => (Card -> Card) -> CardName -> CardMap -> m CardMap
adjustCardByName f cname cmap = do
    cid <- findByName cmap cname
    return $ M.adjust f cid cmap

inputCardQuantity :: CardId -> CardMap -> IO CardMap
inputCardQuantity cid cmap = do
    newq <- ask
    let newm = M.adjust (setQuantity newq) cid cmap
    save newm
    return newm
    where ask = do
                let card = cmap M.! cid
                    ccq = fromMaybe Zero $ cardQuantity card
                T.putStr $ T.format "How many of {} {}M {} card {}? [{}] " (show $ cardClass    card,
                                                                            show $ cardCost     card,
                                                                            show $ cardRarity   card,
                                                                            show $ cardName     card,
                                                                            show $ ccq)
                hFlush stdout
                answer <- getLine
                case answer of
                    ""  -> return ccq
                    "0" -> return Zero
                    "1" -> return One
                    "2" -> return Many
                    _   -> ask

inputCardsQuantity :: [CardId] -> CardMap -> IO CardMap
inputCardsQuantity cids cmap = do
    putStrLn "Please input your collection card by card."
    putStrLn "Expected values: 0, 1, 2."
    foldl1' (>=>) (inputCardQuantity <$> cids) cmap

readPredicate :: MonadError String m => [String] -> m Predicate
readPredicate = either throwError return . fmap foldPred . mapM readPred
    where foldPred = foldl' combinePred emptyPred
          emptyPred = const True
          combinePred p1 p2 c = p1 c && p2 c
          readPred (stripPrefix "h=" -> Just fs) = fs `parsedWith` re (undefined :: CardClass)
          readPred (stripPrefix "r=" -> Just fs) = fs `parsedWith` re (undefined :: CardRarity)
          readPred (stripPrefix "c=" -> Just fs) = fs `parsedWith` re (undefined :: CardCost)
          readPred (stripPrefix "q=" -> Just fs) = fs `parsedWith` ro
          readPred (stripPrefix "s=" -> Just fs) = fs `parsedWith` rs
          readPred "standard"                    = Right standard
          readPred "missing"                     = Right missing
          readPred "owned"                       = Right owned
          readPred f                             = Left $ f ++ " is not a valid filter"
          parsedWith fs t = fmap genPred $ mapM t $ splitOn "," fs
          re :: (Read a, Typeable a) => a -> String -> Either String a
          re p s = maybe (Left $ s ++ " is not a valid " ++ show (typeOf p)) Right $ readMay s
          ro :: String -> Either String CardQuantity
          ro = re (undefined :: Int) >=> maybe (Left "quantity must be in [0..2]") Right . toEnumMay
          rs "cla" = Right Classic
          rs "hof" = Right HallOfFame
          rs "gvg" = Right GoblinsVsGnomes
          rs "tgt" = Right GrandTournament
          rs "wog" = Right WhispersOldGods
          rs "msg" = Right GangsOfGadgetzan
          rs "jtu" = Right JourneyToUngoro
          rs "ktf" = Right KnightsFrozenThrone
          rs "kac" = Right KoboldsAndCatacombs
          rs "ww"  = Right Witchwood
          rs "bd"  = Right Boomsday
          rs "rr"  = Right RastakhansRumble
          rs "ros" = Right RiseOfShadows
          rs s     = Left $ s ++ " is not a valid set"



-- commands

names :: IO ()
names = mapM_ print . sort . fmap cardName . M.elems =<< loadOrDie

list :: [String] -> IO ()
list fs = do
    pred  <- run $ readPredicate fs
    cards <- keep pred <$> loadOrDieAndCheck
    putStr $ render id id id $ Table
        (Group SingleLine [Group NoLine $ Header <$> mkHeader cards c | c <- cardClasses, not $ M.null $ cards @= c])
        (Group SingleLine $ Header <$> ["Cost", "Set", "Rarity", "Name", "Quantity"])
        [[show c, show s, show r, show n, maybe "?" show q] | (Card _ s _ c r n q) <- sort $ M.elems cards]
    where mkHeader cards c = show c : replicate (M.size (cards @= c) - 1) ""

cost :: [String] -> IO ()
cost fs = do
    pred  <- run $ readPredicate fs
    cards <- keep pred <$> loadOrDieAndCheck
    print $ sum $ map mDust $ toList cards
    where aDust c = (if cardRarity c == Legendary then 1 else 2) * round (craftValue $ cardRarity c)
          cDust c = maybe 0 fromEnum (cardQuantity c) * round (craftValue $ cardRarity c)
          mDust c = aDust c - cDust c

stats :: [CardSet] -> IO ()
stats sets = do
    cards <- keep sets <$> loadOrDieAndCheck
    putStrLn $ render id id id $ Table
        (Group SingleLine [
            Group NoLine $ Header . show <$> sets,
            Group NoLine $ Header . show <$> cardClasses,
            Header "Total"
            ])
        (Group DoubleLine [
            Group SingleLine $ Header . show <$> cardRarities,
            Header "Total"
            ])
        ([[stat (cards @= r @= s) | r <- cardRarities] ++ [stat (cards @= s)] | s <- sets]        ++
         [[stat (cards @= r @= c) | r <- cardRarities] ++ [stat (cards @= c)] | c <- cardClasses] ++
         [[stat (cards @= r)      | r <- cardRarities] ++ [stat cards]])

    putStrLn $ "    All cards dust value: " ++ show (sum $ map aDust $ toList cards)
    putStrLn $ "Current cards dust value: " ++ show (sum $ map cDust $ toList cards)
    putStrLn $ "Missing cards dust value: " ++ show (sum $ map mDust $ toList cards)
    sequence_ [T.putStrLn $ T.format "{} pack value: {}" (T.left 18 ' ' $ show s,
                                                          T.left 3  ' ' $ show $ round $ packValue s cards)
              | s <- cardStandardSets]

    where stat cards = let t = 2 * M.size cards - M.size (cards @= Legendary)
                           m = sum $ count <$> toList cards in
                       T.unpack $ T.format "{} / {} ({}%)" (T.left 3 ' ' m, T.left 3 ' ' t, T.left 3 ' ' (div (100 * m) t))
          aDust c = (if cardRarity c == Legendary then 1 else 2) * round (craftValue $ cardRarity c)
          cDust c = maybe 0 fromEnum (cardQuantity c) * round (craftValue $ cardRarity c)
          mDust c = aDust c - cDust c
          count c = let r = fromEnum $ fromMaybe Zero $ cardQuantity c in
              if cardRarity c == Legendary
                  then min r 1
                  else r

update :: IO ()
update = do
    logInfo "downloading card database"
    path <- getFilePath appName cardsFile
    httpLBS cardsURL >>= B.writeFile path . getResponseBody
    migrate appName
    cards <- loadOrDie
    let toFix = M.size $ missingQuantity cards
    when (toFix > 0) $ logInfo $ "there are " ++ show toFix ++ " new cards; run `hcm fix` to update them"

add :: CardName -> IO ()
add name = run $ loadOrDieAndCheck >>= adjustCardByName incrQuantity name >>= save

del :: CardName -> IO ()
del name = run $ loadOrDieAndCheck >>= adjustCardByName decrQuantity name >>= save

input :: [String] -> IO ()
input fs = do
    p <- run $ readPredicate fs
    m <- loadOrDieAndCheck
    void $ inputCardsQuantity (map cardId $ sort $ M.elems $ m @= p) m

fix :: IO ()
fix = do
    m <- loadOrDieAndCheck
    void $ inputCardsQuantity (map cardId $ sort $ M.elems $ missingQuantity m) m



-- main

help :: IO ()
help = putStrLn "usage: hcs cmd [args]\
\\n\
\\ncommands:\
\\n     help                    display this help\
\\n     list  [filters...]      list cards\
\\n     cost  [filters...]      display dust cost of completing the selected subset\
\\n     stats [standard|wild]   stats gathered from your collection (default: wild)\
\\n     update                  update the local card database\
\\n     add card1 [card2...]    increase the quantity of a card in your collection\
\\n     del card1 [card2...]    decrease the quantity of a card in your collection\
\\n     input [filters...]      prompts you for quantity\
\\n     fix                     runs input for all new cards\
\\n\
\\nfilters:\
\\n     h=         filter by hero        h=Druid\
\\n     r=         filter by rarity      r=Epic\
\\n     c=         filter by cost        c=2\
\\n     q=         filter by quantity    q=0\
\\n     s=         filter by set         s=gvg\
\\n     owned      cards     in collection\
\\n     missing    cards not in collection\
\\n     standard   cards that can be used in standard mode\
\\n\
\\n     filters also accept a list of values: r=Epic,Legendary\
\\n\
\\nsets:\
\\n     cla        Classic\
\\n     hof        Hall of Fame\
\\n     gvg        Goblins VS Gnomes\
\\n     tgt        The Grand Tournament\
\\n     wog        Whisper of the Old Gods\
\\n     msg        Mean Streets of Gadgetzan\
\\n     jtu        Journey to Un'Goro\
\\n     ktf        Knights of the Frozen Throne\
\\n     kac        Kobolds and Catacombs\
\\n     ww         Witchwood\
\\n     bd         Boomsday\
\\n     rr         Rastakhan's Rumble\
\\n     ros        Rise of Shadows"

check :: CardMap -> IO ()
check cm = do
  config <- getFilePath appName quantityFile
  when someEmptySets $ putStrLn ("Your card collection seems outdated.\
\\nPlease backup your collection file and run `hcm update`.\
\\nYour collection file is: " ++ config) >> exitFailure
  where someEmptySets = any null [cm @= set | set <- cardSets]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["help"  ]             -> help
        ["names" ]             -> names
        ["update"]             -> update
        ["fix"   ]             -> fix
        ["stats" ]             -> stats cardSets
        ["stats", "standard" ] -> stats cardStandardSets
        ["stats", "wild" ]     -> stats cardSets
        "list"   : fs          -> list fs
        "cost"   : fs          -> cost fs
        "input"  : fs          -> input fs
        "add"    : cs          -> mapM_ (add . CardName) cs
        "del"    : cs          -> mapM_ (del . CardName) cs
        _                      -> help >> exitFailure
