{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.List.Split
import Data.Tuple.Utils
import Data.List
import Data.String.Utils
import Data.Char
import Text.Printf
import System.FilePath
import Data.Ord

find_keywords :: String -> [String] -> [Int]
find_keywords text keywords = map (\keyword -> length $ findIndices ((==keyword)) words) (map (replace " " "-") keywords) where
	words = map (map toLower) $ splitWs $ foldr (\a b -> replace a (replace " " "-" a) b) text keywords

accumulate :: Num a => [[a]] -> [a]
accumulate = foldr (zipWith (+)) (repeat 0)

findYear x = head $ (filter (all isDigit) x) ++ ["0"]
findAbstract = maximumBy (comparing length)

main = do

	ethics		<- (sort . lines) <$> readFile "dblp/ethics_keywords.txt"
	classical	<- (sort . lines) <$> readFile "dblp/classical_keywords.txt"
	trending	<- (sort . lines) <$> readFile "dblp/trending_keywords.txt"

	forbidden	<- words <$> readFile "abstracts/forbidden_wordcloud.txt"

	let keywords = ethics

	entries_aaai 					<- map (\x -> (replace ";" "" $ replace "\n" "" $ x!!0, findAbstract x)) <$> (chunksOf 10 . init . splitOn "#") <$> readFile "abstracts/aaai.txt"
	entries_nips					<- map (\x -> (replace ";" "" $ replace "\n" "" $ x!!0, findAbstract x)) <$> (chunksOf 10 . init . splitOn "#") <$> readFile "abstracts/nips.txt"

	let entries_keywords 			= map (\(year,abstract) -> (year, find_keywords abstract keywords, length $ words $ abstract)) entries_nips
	let entries_keywords_byyear 	= map (\x -> (fst3 $ head x, (fromIntegral $ sum $ accumulate $ map snd3 x)/(fromIntegral $ length x)::Double )) $ groupBy (\a b -> (<=0) $ abs $ (read $ fst3 a) - (read $ fst3 b)) $ sortBy (comparing fst3) entries_keywords 

	--writeFile "abstracts/aaai-abstract-texts.txt" $ intercalate " " $ filter (\word -> (not $ all isDigit word) && (not $ elem (map toLower word) forbidden)) $ words $ replace "]" " " $ replace "[" " " $ replace ")" " " $ replace "(" " " $ filter (not . (`elem` "1234567890,.?!:;\"\'")) $ concat $ map snd entries_aaai
	writeFile "stats/abstracts-nips-ethics.dat" $ unlines $ map (\x -> printf "%s %0.3f" (fst x) (snd x) ) entries_keywords_byyear