{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}



-- imports

import           Control.Applicative
import           Control.Monad.Error
import           Data.IxSet            as S
import           Data.List             as L (foldl', sort, stripPrefix, maximumBy)
import qualified Data.Text.Format      as T
import qualified Data.Text.Lazy        as T
import qualified Data.Text.Lazy.IO     as T
import           Network.HTTP.Conduit  hiding (Proxy)
import           Network.URL
import           System.Console.ANSI
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Tabular
import           Text.Tabular.AsciiArt

import           Buy
import           Card
import           Persistence



-- config

cardsURL  = "http://hearthstonejson.com/json/AllSets.json"
cardsFile = "cards.json"
appName   = "hcm"



-- helpers

run :: ErrorT String IO a -> IO a
run = runErrorT >=> either fail return

refCards :: IO Cards
refCards = run $ readCardsRefM =<< simpleHttp cardsURL

load :: IO (Maybe Cards)
load = run $ loadCards cardsFile appName

save :: Cards -> IO ()
save = run . saveCards cardsFile appName

updateCards :: Cards -> Maybe Cards -> Cards
updateCards rc = maybe rc (`S.union` rc)

exit :: String -> IO a
exit s = putStrLn s >> exitFailure

loadForSure :: IO Cards
loadForSure = load >>= maybe (exit "No card collection found, please update.") return

incr :: CardQuantity -> CardQuantity
incr Zero = One
incr One  = More
incr More = More

decr :: CardQuantity -> CardQuantity
decr Zero = Zero
decr One  = Zero
decr More = One

updateQuantity :: (CardQuantity -> CardQuantity) -> CardName -> Cards -> IO Cards
updateQuantity func name set = case getOne $ set @= name of
    Nothing -> exit $ "No card named " ++ (runCardName name) ++ "."
    Just c  -> let newQuantity = func $ cardQuantity c
                   newCard = c { cardQuantity = newQuantity } in
               return $ updateIx name newCard set

mkFilters :: [String] -> Cards -> IO Cards
mkFilters fs = L.foldl' (>=>) return $ mkFilter <$> fs
    where mkFilter (stripPrefix "h=" -> Just f) = return . (@= (read f :: CardClass))
          mkFilter (stripPrefix "r=" -> Just f) = return . (@= (read f :: CardRarity))
          mkFilter (stripPrefix "c=" -> Just f) = return . (@= (CardCost $ read f))
          mkFilter "q=0"                        = return . (@= Zero)
          mkFilter "q=1"                        = return . (@= One)
          mkFilter "q=2"                        = return . (@= More)
          mkFilter "s=c"                        = return . (@= Common)
          mkFilter "s=gvg"                      = return . (@= GoblinsVsGnomes)
          mkFilter "s=tgt"                      = return . (@= GrandTournament)
          mkFilter "owned"                      = return . (@+ [One, More])
          mkFilter f                            = const $ exit $ "unknown filter " ++ f



-- main

update :: IO ()
update = do
    ref  <- refCards
    mine <- load
    save $ updateCards ref mine

add :: CardName -> IO ()
add name = loadForSure >>= updateQuantity incr name >>= save

del :: CardName -> IO ()
del name = loadForSure >>= updateQuantity decr name >>= save

cstats :: IO ()
cstats = do
    cards <- loadForSure
    putStrLn $ render id id id $ Table
        (Group SingleLine [
            Group NoLine $ Header . show <$> cardSets,
            Group NoLine $ Header . show <$> cardClasses,
            Header "Total"
            ])
        (Group DoubleLine [
            Group SingleLine $ Header . show <$> cardRarities,
            Header "Total"
            ])
        ([[stat (cards @= r @= s) | r <- cardRarities] ++ [stat (cards @= s)] | s <- cardSets]    ++
         [[stat (cards @= r @= c) | r <- cardRarities] ++ [stat (cards @= c)] | c <- cardClasses] ++
         [[stat (cards @= r)      | r <- cardRarities] ++ [stat (cards)]])

    sequence_ [T.putStrLn $ T.format "{} pack value: {}" (T.left 7 ' ' $ show s,
                                                          T.left 3 ' ' $ show $ round $ packValue s cards)
              | s <- cardSets]

    where stat cards = let t = 2 * (size cards) - (size $ cards @= Legendary)
                           m = sum $ count <$> toList cards in
                       T.unpack $ T.format "{} / {}" (T.left 3 ' ' m, T.left 3 ' ' t)
          count c = let r = fromEnum $ cardQuantity c in
              if cardRarity c == Legendary
                  then min r 1
                  else r

input :: [String] -> IO ()
input fs = do
    let byCost = Proxy :: Proxy CardCost
    cards <- loadForSure >>= mkFilters fs
    putStrLn "Please input your collection card by card."
    putStrLn "Expected values: 0, 1, 2."
    save =<< fromList <$> (sequence $ ask <$> (sort $ toList cards))
    where ask card = do
              T.putStr $ T.format "How many of {} {}M {} card {}? [{}] " (show $ cardClass card,
                                                                          show $ cardCost card,
                                                                          show $ cardRarity card,
                                                                          show $ cardName card,
                                                                          show $ cardQuantity card)
              hFlush stdout
              answer <- getLine
              case answer of
                  ""  -> return $ card
                  "0" -> return $ card { cardQuantity = Zero }
                  "1" -> return $ card { cardQuantity = One  }
                  "2" -> return $ card { cardQuantity = More }
                  _   -> ask card

list :: [String] -> IO ()
list fs = do
    s <- loadForSure >>= mkFilters fs
    let l = sort $ toList s
        c = ["Cost", "Set", "Rarity", "Name", "Quantity"]
    putStr $ render id id id $ Table
        (Group SingleLine [Group NoLine $ Header <$> mkHeader s c | c <- cardClasses, not $ S.null $ s @= c])
        (Group SingleLine $ Header <$> c)
        [[show c, show s, show r, show n, show q] | (Card s _ c r n q) <- l]
    where mkHeader s c = (show c) : (replicate (size (s @= c) - 1) $ "")

names :: IO ()
names = do
    cards <- loadForSure
    sequence_ $ print . cardName <$> toAscList byName cards
    where byName = Proxy :: Proxy CardName

help :: IO ()
help = do
  putStrLn "usage: hcs cmd [args]\
\\n\
\\ncommands:\
\\n     help                    display this help\
\\n     add card1 [card2...]    add a card to your collection\
\\n     del card1 [card2...]    remove a card from your collection\
\\n     stats                   stats gathered from your collection\
\\n     update                  update your collection\
\\n     input [filters...]      input your collection\
\\n     list  [filters...]      list cards\
\\n\
\\nfilters:\
\\n     h=       filter by hero        h=Druid\
\\n     r=       filter by rarity      r=Epic\
\\n     c=       filter by cost        c=2\
\\n     q=       filter by quantity    q=0\
\\n     s=       filter by set         s=gvg\
\\n     owned    cards in collection"



main :: IO ()
main = do
    args <- getArgs
    case args of
        ["help"]   -> help
        ["stats"]  -> cstats
        ["names"]  -> names
        ["update"] -> update
        "list":fs  -> list fs
        "input":fs -> input fs
        "add":cs   -> sequence_ $ map (add . CardName) cs
        "del":cs   -> sequence_ $ map (del . CardName) cs
        _          -> help >> exitFailure
