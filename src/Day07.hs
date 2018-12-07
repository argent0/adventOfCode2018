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
solve1 steps = reverse $ go [] avail unlocks prereq
	where
	avail = DL.sort $ Set.toList $ (Set.fromList $ Map.keys unlocks) `Set.difference` (Set.fromList $ Map.keys prereq)
	unlocks = Map.fromListWith (++) $ fmap(\(Depen r d) -> (r, [d])) steps
	prereq = Map.fromListWith (++) $ fmap (\(Depen r d) -> (d, [r])) steps
	go :: String -> String -> Map Char String -> Map Char String -> String
	go acc (a:as) unlk pr = if (not $ null acc) && (head acc == a) then go acc as unlk pr else
		let
			n_unlk = a `Map.delete` unlk
			n_pr = Map.map (filter (/=a)) pr
			n_avl = Map.keys $ Map.filter (null) $ n_pr
		in go (a:acc) (DL.sort $ as++n_avl) n_unlk (Map.filter (not . null) n_pr)
	go acc [] unlk pr = acc

solve_1 :: IO ()
solve_1 = 
	SysIO.withFile "inputs/day07b" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
		let input = partitionEithers $
			fmap (AP.parseOnly parseStep . T.pack ) file_lines
		when (length (fst input) /= 0) (putStrLn "Parse Error" >> print (fst input))
		print $ solve1 $ snd input
