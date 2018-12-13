{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{- See: https://en.wikipedia.org/wiki/Summed-area_table -}
module Day11 (
	solve_1
) where

import Text.Printf
import qualified System.IO as SysIO
import Data.Function (on)
import qualified Data.List as DL
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import Debug.Trace
import Data.Either (partitionEithers)
import Control.Monad (when, join, replicateM, void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (ord)
import Control.Arrow
import Control.Lens
import qualified Data.Array.IArray as Arr
import Linear hiding (trace)
import Util.Parsers (numbers, signeds)

type Pos = V2 Integer

powerLevel :: Integer -> Pos -> Integer
powerLevel serial (V2 x y) =
	if (mult < 100)
		then (-5)
		else ((mult `div` 100) `mod` 10) - 5
	where
	rack_id = x + 10
	start = rack_id * y
	add_serial = start + serial
	mult = add_serial * rack_id

-- An array of columns
type Grid = Arr.Array Integer (Arr.Array Integer Integer)

mySerial :: Integer
mySerial = 4842

grid :: Integer -> Integer -> Grid
grid serial size = --(flip trace) undefined $ show $
	Arr.array b $ zip [1..size] $ fmap mapper $
	DL.groupBy ((==) `on` ((^. _x) . fst)) $ fmap (id &&& powerLevel serial) $
	V2 <$> [1..size] <*> [1..size]
	where
	mapper :: [(V2 Integer, Integer)] -> Arr.Array Integer Integer
	mapper = Arr.array b . fmap (first (^. _y))
	b :: (Integer, Integer)
	b = (1,size)

q :: Grid -> V2 Integer -> Integer
q g (V2 x y) = (g Arr.! x) Arr.! y

totalSquarePower :: Integer -> Grid -> V2 Integer -> Integer
totalSquarePower square_size g (V2 x y) =
	DL.foldl' (+) 0 $
	fmap (q g) $
	V2 <$> [x..(x+square_size-1)] <*> [y..(y+square_size-1)]

s1 :: Integer -> Integer -> Integer -> V2 Integer
s1 serial grid_size square_size=
	DL.maximumBy (compare `on` (totalSquarePower square_size g)) $
	V2 <$> [1..(grid_size-square_size+1)] <*> [1..(grid_size-square_size+1)]
	where
	g = grid serial grid_size

s1' :: Grid -> Integer -> Integer -> (V2 Integer, Integer)
s1' g grid_size square_size=
	DL.maximumBy (compare `on` snd) $
	fmap (id &&& (totalSquarePower square_size g)) $
	V2 <$> [1..(grid_size-square_size+1)] <*> [1..(grid_size-square_size+1)]

-- Compute the sums incrementaly, so it is faster
s2 :: Integer -> Integer -> (V2 Integer, Integer)
s2 serial grid_size=
	DL.maximumBy (compare `on` fst) $ fmap mapper [1..grid_size]
	where
	g = grid serial grid_size
	mapper sz = let
		(max,val) = s1' g grid_size sz
		in trace (show (val, max, sz)) (max, sz)

solve_1 :: IO ()
solve_1 =
	putStrLn $ show $ s2 mySerial 300
	--SysIO.withFile "inputs/day11b" SysIO.ReadMode $ \input_fh ->
	--	(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
	--	print $ fmap signeds $ file_lines
