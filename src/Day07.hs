{-# LANGUAGE OverloadedStrings #-}
module Day07 (
	solve_1
) where

import qualified System.IO as SysIO
import Data.Function (on)
import qualified Data.List as DL
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import Debug.Trace
import Data.Either (partitionEithers)
import Control.Monad (when, join)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (ord)

-- A dependency
data Depen = Depen !Char !Char deriving Show

req :: Depen -> Char
req (Depen r _) = r

dep :: Depen -> Char
dep (Depen _ d) = d

parseStep :: AP.Parser Depen
parseStep = do
	_ <- AP.string "Step "
	a <- AP.anyChar
	_ <- AP.string " must be finished before step "
	b <- AP.anyChar
	_ <- AP.string " can begin."
	pure $ Depen a b

solve1 :: [Depen] -> String
solve1 steps = reverse $ go [] avail prereq
	where
	avail = DL.sort $ Set.toList $ (Set.fromList $ Map.keys unlocks) `Set.difference` (Set.fromList $ Map.keys prereq)
	unlocks = Map.fromListWith (++) $ fmap(\(Depen r d) -> (r, [d])) steps
	prereq = Map.fromListWith (++) $ fmap (\(Depen r d) -> (d, [r])) steps
	go :: String -> String -> Map Char String -> String
	go acc (a:as) pr = if (not $ null acc) && (head acc == a) then go acc as pr else
		let
			n_pr = Map.map (filter (/=a)) pr
			n_avl = Map.keys $ Map.filter (null) $ n_pr
		in go (a:acc) (DL.sort $ as++n_avl) (Map.filter (not . null) n_pr)
	go acc [] pr = acc

data Worker = Free | Working Int Char deriving (Show, Eq)

updateWorker :: Worker -> Worker
updateWorker (Working n c) 
	| n == 0 = Working 0 c
	| otherwise = Working (n-1) c
updateWorker w = w

isFree :: Worker -> Bool
isFree Free = True
isFree _ = False

isReady :: Worker -> Bool
isReady (Working n c) = n == 0
isReady w = False

stepCost :: Char -> Int
stepCost c = 1 + (ord c) - (ord 'A')

solve2 :: [Depen] -> [String]
solve2 steps = reverse $ go (replicate max_worker Free) [] avail prereq
	where
	avail = DL.sort $ Set.toList $ (Set.fromList $ Map.keys unlocks) `Set.difference` (Set.fromList $ Map.keys prereq)
	unlocks = Map.fromListWith (++) $ fmap(\(Depen r d) -> (r, [d])) steps
	prereq = Map.fromListWith (++) $ fmap (\(Depen r d) -> (d, [r])) steps

	go :: [Worker] -> [Worker] -> String -> Map Char String -> [String]
	go (fw:fws) [] [] pr = []
	go fws bws [] pr = (("NMA"++show bws) :) $ let
			updt_wks = fmap updateWorker bws
			(free, busy) = DL.partition isReady updt_wks
			done_tasks = fmap (\(Working _ c) -> c) $ free
			n_pr = Map.map (\e -> Set.toList $ (Set.fromList e) `Set.difference` (Set.fromList done_tasks)) pr
			n_avl = Map.keys $ Map.filter (null) $ n_pr
		in go ((fmap (const Free) free) ++ fws) busy (DL.sort n_avl)
			(Map.filter (not . null) n_pr)

	go [] bws (a:as) pr = (("NMF" ++ show bws):) $ let
			updt_wks = fmap updateWorker bws
			(free, busy) = DL.partition isReady updt_wks
			done_tasks = fmap (\(Working _ c) -> c) $ free
			n_pr = Map.map (\e -> Set.toList $ (Set.fromList e) `Set.difference` (Set.fromList done_tasks)) pr
			n_avl = Map.keys $ Map.filter (null) $ n_pr
			n_as = Set.toList $ (Set.fromList (a:as)) `Set.difference` (Set.fromList done_tasks)
		in go
				(fmap (const Free) free)	--Add free workers
				busy 						--Keep the busy ones
				(fmap head $ DL.group $ DL.sort $ n_as ++ n_avl) --Pass available tasks
				(Map.filter (not . null) n_pr) --Pass remaining dependencies

	go (fw:fws) bws (a:as) pr = go fws ((Working (stepCost a + base_cost) a):bws) as pr
	--go nw wks acc (a:as) pr = if (not $ null acc) && (head acc == a) then go acc as pr else
	--	if nw == max_worker
	--		then let
	--			updt_wks = fmap updateWorker wks
	--			done_tasks = fmap (\(Done c) -> c) $ filter isDone updt_wks
	--			n_pr = Map.map (\e -> Set.toList $ (Set.fromList e) `Set.difference` done_tasks) pr
	--			n_avl = Map.keys $ Map.filter (null) $ n_pr
	--		in ((show wks) :) $ go (nw - (length done_tasks)) (filter (not . isDone) updt_wks)
	--			(DL.sort $ Set.toList $ (Set.fromList (a:as)) `Set.difference` done_tasks) n_pr
	--	else let
	--		n_pr = Map.map (filter (/=a)) pr
	--		n_avl = Map.keys $ Map.filter (null) $ n_pr
	--	in go (a:acc) (DL.sort $ as++n_avl) (Map.filter (not . null) n_pr)
	--go nw wks acc [] pr = acc
	base_cost = 60
	max_worker = 5

solve_1 :: IO ()
solve_1 = 
	SysIO.withFile "inputs/day07" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
		let input = partitionEithers $
			fmap (AP.parseOnly parseStep . T.pack ) file_lines
		when (length (fst input) /= 0) (putStrLn "Parse Error" >> print (fst input))
		putStrLn $ unlines $ reverse $ solve2 $ snd input
