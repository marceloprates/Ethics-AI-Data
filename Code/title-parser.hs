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

	ethics		<- (sort . lines) <$> readFile "dblp/ethics_keywords.txt"
	classical	<- (sort . lines) <$> readFile "dblp/classical_keywords.txt"
	trending	<- (sort . lines) <$> readFile "dblp/trending_keywords.txt"

	let keywords = trending

	cdblp <- map (\x -> let y = map (replace "\"" "") $ splitOn "\t" x in (y!!1,replace "/" "" $ replace " " "-" $ y!!2,y!!3) ) <$> drop 1 . lines <$> readFile "dblp/cdblp.csv"
	jdblp <- map (\x -> let y = map (replace "\"" "") $ splitOn "\t" x in (y!!1,replace "/" "" $ replace " " "-" $ y!!2,y!!4) ) <$> drop 1 . lines <$> readFile "dblp/jdblp.csv"

	let 
		--cdblp_aggregated = map (\(t,v,y) -> if (aaai_symposium v) then (t,"AAAI-Symposium",y) else if (aaai_workshop v) then (t,"AAAI-Workshop",y) else (t,v,y)) cdblp where
		--	aaai_symposium v = (isInfixOf "AAAI-Fall-Symposium" v) || (isInfixOf "AAAI-Spring-Symposium" v)
		--	aaai_workshop v = isInfixOf "AAAI-Workshop" v

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

		cdblp_keywords_byyear = map (\x -> (thd3 $ head x, map (\y -> (snd3 $ head y, (((/(fromIntegral $ length y)) . fromIntegral) $ sum $ accumulate $ map (\z -> find_keywords (fst3 z) keywords) y)::Double, sum $ accumulate $ map (\z -> find_keywords (fst3 z) keywords) y )) $ groupBy (\a b -> (snd3 a)==(snd3 b)) $ sortBy (comparing snd3) x )) $ groupBy (\a b -> (thd3 a) == (thd3 b)) $ sortBy (comparing thd3) $ filter (\(t,v,y) -> selected v) cdblp_aggregated
		jdblp_keywords_byyear = map (\x -> (thd3 $ head x, map (\y -> (snd3 $ head y, (((/(fromIntegral $ length y)) . fromIntegral) $ sum $ accumulate $ map (\z -> find_keywords (fst3 z) keywords) y)::Double, sum $ accumulate $ map (\z -> find_keywords (fst3 z) keywords) y )) $ groupBy (\a b -> (snd3 a)==(snd3 b)) $ sortBy (comparing snd3) x )) $ groupBy (\a b -> (thd3 a) == (thd3 b)) $ sortBy (comparing thd3) $ filter (\(t,v,y) -> selected v) jdblp_aggregated

		selected v =
			v == "Artif.-Intell."
			{-v == "AAAI" 	||
			v == "IJCAI" 	||
			v == "NIPS" 	||
			v == "ICML" 	||
			v == "ICRA" 	||
			v == "IROS"-}

	print $ filter ((\x -> elem x ["Artif.-Intell."]) . fst) $ map (\x -> (snd3 $ head x, length x)) $ groupBy (\a b -> (==) (snd3 a) (snd3 b)) $ sortBy (comparing snd3) $ jdblp_aggregated

	--writeFile "table-test.dat" $ unlines $ map (\(y,x) -> printf "%s %.2f/ %d" y (snd3 $ head x) (thd3 $ head x) ) jdblp_keywords_byyear
	--mapM_ (\(venue,history) -> writeFile (printf "stats/conf/trending/%s" venue)		( unlines $ (intercalate " " $ "year":keywords) : map (\(year,frequencies) -> let frequencies' = (sum frequencies) : frequencies in year ++ " " ++ (intercalate " " $ map show frequencies') ) history) ) cdblp_keywords_byvenue
	--mapM_ (\(venue,history) -> writeFile (printf "stats/journal/trending/%s" venue)		( unlines $ (intercalate " " $ "year":keywords) : map (\(year,frequencies) -> let frequencies' = (sum frequencies) : frequencies in year ++ " " ++ (intercalate " " $ map show frequencies') ) history) ) jdblp_keywords_byvenue