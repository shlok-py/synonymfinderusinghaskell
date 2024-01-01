{-# LANGUAGE OverloadedStrings #-}
-- import Paths_haskell(getDataFileName)
import System.FilePath ()
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Maybe (listToMaybe)
-- import Distribution.Compat.Prelude (print)

-- Define a type for word-synonym pairs
data SynonymPair = SynonymPair
  { word    :: !String
  , synonym :: !String
  } deriving (Show, Eq)

-- Define how to parse a SynonymPair from CSV
instance FromRecord SynonymPair where
  parseRecord v
    | V.length v == 2 = SynonymPair <$> v .! 0 <*> v .! 1
    | otherwise       = fail "Expected exactly 2 columns"

-- Read the synonym pairs from the CSV file
readSynonyms :: FilePath -> IO (V.Vector SynonymPair)
readSynonyms filePath = do
  print filePath
  csvData <- BL.readFile filePath
  case decode NoHeader csvData of
    Left err -> error $ "Error decoding CSV: " ++ err
    Right rows -> return rows

-- Find and display the synonym for a given word
findSynonym :: V.Vector SynonymPair -> String -> Maybe String
findSynonym synonyms target =
  fmap synonym $ listToMaybe $ filter (\pair -> word pair == target) $ V.toList synonyms

-- Main function
main :: IO ()
main = do
  putStrLn "Enter a word to find its synonym:"
  userWord <- getLine

  -- synonymsFilePath <- getDataFileName "synonyms.csv"
  -- print synonymsFilePath
  synonyms <- readSynonyms "synonymfinder/app/synonyms.csv"
  -- synonyms <- readSynonyms "../app/synonyms.csv"

  case findSynonym synonyms userWord of
    Just synonym -> putStrLn $ "Synonym: " ++ synonym
    Nothing      -> putStrLn "Synonym not found."