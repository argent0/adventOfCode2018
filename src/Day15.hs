{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Day15 (
	solve_1
) where

import Data.Ratio
import Text.Printf
import qualified System.IO as SysIO
import Data.Function (on)
import qualified Data.List as DL
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import Debug.Trace

import Data.Either (partitionEithers)
import Data.Maybe (catMaybes, isJust, fromJust)

import Control.Monad (forever, when, join, replicateM, void, foldM, foldM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (ord)
import Control.Arrow
import Control.Lens

import qualified Data.Array.Diff as Arr

import Linear hiding (trace)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Functor.Foldable --recursion-schemes
import Data.Functor.Foldable.TH --makeBaseFunctor
import Control.Comonad.Cofree

import qualified Data.Foldable as Fld

import Util.Grid

import qualified Data.Sequence as Seq

sum' :: (Foldable t, Num a) => t a -> a
sum' = DL.foldl' (+) 0

data MapElement = Wall | OpenSapce deriving (Show, Eq)

charToMapElement :: Char -> Maybe MapElement
charToMapElement '#' = Just Wall
charToMapElement '.' = Just OpenSapce
charToMapElement _ = Nothing

mapElementToChar :: MapElement -> Char
mapElementToChar OpenSapce = '.'
mapElementToChar Wall = '#'

data Creature = Goblin | Elf deriving (Show, Eq)

charToCreature :: Char -> Maybe Creature
charToCreature 'E' = Just Elf
charToCreature 'G' = Just Goblin
charToCreature _ = Nothing

creatureToChar :: Creature -> Char
creatureToChar Elf = 'E'
creatureToChar Goblin = 'G'

readingOrder :: Ord a => V2 a -> V2 a -> Ordering
readingOrder = compare `on` swap
	where
	swap (V2 x y) = V2 y x

--type ProblemMap = Grid2d Int MapElement
type ProblemMap = Arr.Array (Int, Int) MapElement

loadMap :: [String] -> ProblemMap
loadMap lns =
	Arr.array ((1,1),(nCols,nLines)) $ zip pos $ fmap (readMap . charToMapElement) $ concat lns
	where
	pos = fmap (\(V2 x y) -> (x,y)) $ DL.sortBy readingOrder $ V2 <$> [1..nCols] <*> [1..nLines]
	nCols = length $ head $ lns
	nLines = length lns
	readMap Nothing = OpenSapce
	readMap (Just e) = e

type CreatureMap = Arr.Array (Int,Int) (Maybe Creature)

loadCreatures :: [String] -> CreatureMap
loadCreatures lns = 
	Arr.array ((1,1),(nCols,nLines)) $ zip pos $ fmap charToCreature $ concat lns
	where
	pos = fmap (\(V2 x y) -> (x,y)) $ DL.sortBy readingOrder $ V2 <$> [1..nCols] <*> [1..nLines]
	nCols = length $ head $ lns
	nLines = length lns

mapChars :: ProblemMap -> Arr.Array (Int,Int) Char
mapChars pm = Arr.amap mapElementToChar pm

printMap :: ProblemMap -> IO ()
printMap pm =
	putStrLn $ unlines $
		fmap (fmap (mapElementToChar . snd)) $
		DL.groupBy ((==)  `on` (snd . fst)) $
		DL.sortBy (readingOrder `on` (uncurry V2 . fst)) $
		Arr.assocs pm

-- Print creatures and map
printCreatures :: ProblemMap -> CreatureMap -> IO ()
printCreatures pm cm =
	putStrLn $ unlines $
		fmap (fmap (snd)) $
		DL.groupBy ((==)  `on` (snd . fst)) $
		DL.sortBy (readingOrder `on` (uncurry V2 . fst)) $
		Arr.assocs finalChars
	where
	finalChars = (mapChars pm) Arr.// creatureChars
	creatureChars = fmap (second (creatureToChar . fromJust)) $ filter (isJust . snd) $ Arr.assocs cm

printOverlay :: [((Int,Int),Char)] -> ProblemMap -> CreatureMap -> IO ()
printOverlay over pm cm =
	putStrLn $ unlines $
		fmap (fmap (snd)) $
		DL.groupBy ((==)  `on` (snd . fst)) $
		DL.sortBy (readingOrder `on` (uncurry V2 . fst)) $
		Arr.assocs finalChars
	where
	finalChars = (mapChars pm) Arr.// (creatureChars ++ over)
	creatureChars = fmap (second (creatureToChar . fromJust)) $ filter (isJust . snd) $ Arr.assocs cm

adjacents :: Creature -> ProblemMap -> CreatureMap -> [(Int, Int)]
adjacents actor pm cm =
	fmap (head) $ DL.group $
	filter (not . impassable) $ concatMap (adjs . fst) $
		filter ((/= actor) . fromJust . snd) $
		filter (isJust . snd) $
		Arr.assocs cm
	where
	adjs (x,y) = [(x,y-1),(x-1,y), (x,y+1), (x+1,y)]
	impassable (x,y) = ((pm Arr.! (x,y)) == Wall) || (isJust $ cm Arr.! (x,y))

solve_1 :: IO ()
solve_1 =
	SysIO.withFile "inputs/day15" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
		let problem_map = loadMap $ file_lines
		let creatures = loadCreatures $ file_lines
		printOverlay (fmap (,'?') $ adjacents Goblin problem_map creatures) problem_map creatures
