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

find_keywords :: String -> [String] -> [Int]
find_keywords text keywords = map (\keyword -> length $ findIndices ((==keyword)) words) (map (replace " " "-") keywords) where
	words = map (map toLower) $ splitWs $ foldr (\a b -> replace a (replace " " "-" a) b) text keywords

accumulate :: Num a => [[a]] -> [a]
accumulate = foldr (zipWith (+)) (repeat 0)

main = do

	ethics		<- (sort . lines) <$> readFile "../DBLP/ethics_keywords.txt"
	classical	<- (sort . lines) <$> readFile "../DBLP/classical_keywords.txt"
	trending	<- (sort . lines) <$> readFile "../DBLP/trending_keywords.txt"

	let keywords = ethics

	cdblp <- map (\x -> let y = map (replace "\"" "") $ splitOn "\t" x in (y!!1,replace "/" "" $ replace " " "-" $ y!!2,y!!3) ) <$> drop 1 . lines <$> readFile "../DBLP/cdblp.csv"
	jdblp <- map (\x -> let y = map (replace "\"" "") $ splitOn "\t" x in (y!!1,replace "/" "" $ replace " " "-" $ y!!2,y!!4) ) <$> drop 1 . lines <$> readFile "../DBLP/jdblp.csv"

	let 
		cdblp_aggregated = map (\(t,v,y) -> if ai_conf v then (t,"AI",y) else if robotics_conf v then (t,"Robotics",y) else (t,v,y)) cdblp where
			ai_conf v 		= elem v ["AAAI","IJCAI","NIPS","ICML"]
			robotics_conf v = elem v ["ICRA","IROS"]

		jdblp_aggregated = map (\(t,v,y) -> if ieee_trans_ai v then (t,"IEEE-Trans.-AI",y) else if ieee_trans_robotics v then (t,"IEEE-Trans.-Robotics",y) else if ieee_trans v then (t,"IEEE-Trans.",y) else if acm_trans v then (t,"ACM-Trans.",y) else (t,v,y)) jdblp where
			ieee_trans_ai v = 
				isInfixOf "IEEE-Trans.-Affective" v 							||
				isInfixOf "IEEE-Trans.-Audio,-Speech" v 						||
				isInfixOf "IEEE-Trans.Cognitive" v 								||
				isInfixOf "IEEE-Trans.-Comput.-Intellig." v						||
				isInfixOf "IEEE-Trans.Emerging-Topics-in-Comput.-Intellig" v	||
				isInfixOf "IEEE-Trans.-Fuzzy-Systems" v							||
				isInfixOf "IEEE-Trans.-Intelligent"	v							||
				isInfixOf "IEEE-Trans.-Neural" v								

			ieee_trans_robotics v =
				isInfixOf "IEEE-Trans.-Automat." v								||
				isInfixOf "IEEE-Trans.-Automation"	v							||
				isInfixOf "IEEE-Trans.-Robotics" v

			ieee_trans v = isInfixOf "IEEE-Trans." v

			acm_trans v = isInfixOf "ACM-Trans." v

		cdblp_keywords_byvenue = map (\x -> (snd3 $ head x, map (\y -> (thd3 $ head y, (map ((/(fromIntegral $ length y)) . fromIntegral) $ accumulate $ map fst3 y)::[Double] )) $ groupBy (\a b -> (<=5) $ abs $ (read $ thd3 a)-(read $ thd3 b) ) $ sortBy (comparing thd3) x )) $ groupBy (\a b -> (snd3 a) == (snd3 b)) $ sortBy (comparing snd3) $ map (\x -> (find_keywords (fst3 x) keywords, snd3 x, thd3 x)) cdblp_aggregated
		jdblp_keywords_byvenue = map (\x -> (snd3 $ head x, map (\y -> (thd3 $ head y, (map ((/(fromIntegral $ length y)) . fromIntegral) $ accumulate $ map fst3 y)::[Double] )) $ groupBy (\a b -> (<=5) $ abs $ (read $ thd3 a)-(read $ thd3 b) ) $ sortBy (comparing thd3) x )) $ groupBy (\a b -> (snd3 a) == (snd3 b)) $ sortBy (comparing snd3) $ map (\x -> (find_keywords (fst3 x) keywords, snd3 x, thd3 x)) jdblp_aggregated

	mapM_ (\(venue,history) -> writeFile (printf "stats/conf/%s" venue)			( unlines $ (intercalate " " $ "year":keywords) : map (\(year,frequencies) -> let frequencies' = (sum frequencies) : frequencies in year ++ " " ++ (intercalate " " $ map show frequencies') ) history) ) cdblp_keywords_byvenue
	mapM_ (\(venue,history) -> writeFile (printf "stats/journal/%s" venue)		( unlines $ (intercalate " " $ "year":keywords) : map (\(year,frequencies) -> let frequencies' = (sum frequencies) : frequencies in year ++ " " ++ (intercalate " " $ map show frequencies') ) history) ) jdblp_keywords_byvenue