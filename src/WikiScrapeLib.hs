{-# LANGUAGE OverloadedStrings #-}

module WikiScrapeLib
    (  mostfrequentwordonpage
    ) where

import Text.HTML.Scalpel
import Data.Maybe
import Data.Either
import Control.Exception
import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.List.Split
import Data.Tuple
  
mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage page = do
  allContent <- try (scrapeURL page examineTags) :: IO (Either SomeException (Maybe String)) -- scrape for content
  case allContent of
    Left e -> return Nothing -- if exception, returns nothing
    Right x -> do
      stopWords <- readFile "stopwords.txt"
      let title1stFourLetters = map (\x -> toLower x) $ take 4 $ last $ splitOn "/" page -- getting the 1st 4 letters of the title

      -- Data cleansing
      let removeApostro = unwords $ splitOn "'" $ fromJust x   -- remove apostrophes
      let toLowerCase = map (\x -> toLower x) removeApostro    -- convert to lowercase
      let removeExceptSpacesAndLowerASCII = filter (\x -> isSpace x || isAsciiLower x) toLowerCase                           -- remove all but spaces & lower ascii char
      let removeStopWords = filter (\x -> notElem x $ words stopWords) $ words removeExceptSpacesAndLowerASCII               -- remove stopwords
      let removeWordsSingleLetter = filter (\x -> notElem x $ map (\x -> [x]) ['a'..'z']) removeStopWords                    -- remove single letters
      let removeFour1stLettersOfArticleTitle = filter (\x -> not $ isPrefixOf title1stFourLetters x) removeWordsSingleLetter -- remove words with first 4 letters of title

      -- Counting frequencies of the word, sort and return the top result
      let mapping = foldl(\m w -> Map.insertWith (+) w 1 m) Map.empty removeFour1stLettersOfArticleTitle
      let swapKey = map swap $ Map.toList mapping
      let sortedKey = sort swapKey

      return $ Just $ snd $ last sortedKey

-- Examine text within <p>, <i> and <a>
examineTags :: Scraper String String
examineTags = do
  text1 <- texts "p"
  text2 <- texts "i"
  text3 <- texts "a"
  return $ unwords $ text1 ++ text2 ++ text3