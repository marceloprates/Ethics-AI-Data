
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.List.Split
import Data.Tuple.Utils
import Data.List
import Data.String.Utils
import Data.Char
import Text.Printf
import System.FilePath
import Data.Ord
import qualified Data.Text as T

{- Custom list of special characters which are to be removed from each string in an attempt to "clean" them -}
special_characters = "'\"!@#$%&%*()_-+=¬¹²³£¢{[]}\\§/;:<>ºª~^'`´|.·,01234567890"

clean_string = filter (\x -> not $ elem x special_characters)

main = do

	cdblp <- map (\x -> let y = map (replace "\"" "") $ splitOn "\t" x in y!!1 ) <$> drop 1 . lines <$> readFile "../DBLP/cdblp.csv"
	jdblp <- map (\x -> let y = map (replace "\"" "") $ splitOn "\t" x in y!!1 ) <$> drop 1 . lines <$> readFile "../DBLP/jdblp.csv"

	let words_cdblp = map ((map toLower) . clean_string) $ concat $ map words $ filter ((>0) . length) cdblp
	let words_jdblp = map ((map toLower) . clean_string) $ concat $ map words $ filter ((>0) . length) jdblp

	writeFile "title-keywords-ranking-conf.dat" 	$ unlines $ map (\x -> printf "%s %d" (head x) (length x)) $ sortBy (comparing length) $ groupBy (==) $ sort words_cdblp
	writeFile "title-keywords-ranking-journal.dat" 	$ unlines $ map (\x -> printf "%s %d" (head x) (length x)) $ sortBy (comparing length) $ groupBy (==) $ sort words_jdblp
