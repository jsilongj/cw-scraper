{-# LANGUAGE OverloadedStrings #-}

module WikiScrapeLib
    (  mostfrequentwordonpage
    ) where

import Text.HTML.Scalpel
import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import Data.Tuple
import Data.List
import Data.List.Split
import Control.Exception
import Data.Either

mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage page = do
  totalContent <- try (scrapeURL page tagsToSelect) :: IO (Either SomeException (Maybe String)) -- scrape for content
  case totalContent of
    Left e -> return Nothing -- return Nothing if exception
    Right x -> do
      -- prep/pre-processing: read in stopwords and get first 4 letters of title
      stopWords <- readFile "stopwords.txt"
      let titleFirstFourLetters = map (\x -> toLower x) $ take 4 $ last $ splitOn "/" page

      -- processing/cleansing of data: convert to lowercase, remove all but spaces & lower ascii char, remove apostrophes, stopwords, single letters, words with first 4 letters of title
      let convertToLowerCase = map (\x -> toLower x) $ fromJust x
      let removeApostrophes = unwords $ splitOn "'" convertToLowerCase
      let removeAllButSpacesAndLowerASCII = filter (\x -> isSpace x || isAsciiLower x) removeApostrophes
      let removeStopWords = filter (\x -> notElem x $ words stopWords) $ words removeAllButSpacesAndLowerASCII
      let removeSingleLetterWords = filter (\x -> notElem x $ map (\x -> [x]) ['a'..'z']) removeStopWords
      let remove4FirstLettersOfArticleTitle = filter (\x -> not $ isPrefixOf titleFirstFourLetters x) removeSingleLetterWords

      -- count word frequency, sort and return top result
      let m1 = foldl(\m w -> Map.insertWith (+) w 1 m) Map.empty remove4FirstLettersOfArticleTitle
      let swapKeyValuePairs = map swap $ Map.toList m1
      let sortedKeyValuePairs = sort swapKeyValuePairs
      -- print sortedKeyValuePairs
      return $ Just $ snd $ last sortedKeyValuePairs

tagsToSelect :: Scraper String String
tagsToSelect = do
  -- look for content within <p>, <a> and <i> tags
  content1 <- texts "p"
  content2 <- texts "a"
  content3 <- texts "i"
  return $ unwords $ content1 ++ content2 ++ content3