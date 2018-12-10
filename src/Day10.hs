{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Day10 (
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

type Pos = V2 Integer
type Vel = V2 Integer

data GenCoords = GenCoords
	{ _pos :: Pos
	, _vel :: Vel } deriving Show

makeLenses ''GenCoords

parseCoords :: AP.Parser GenCoords
parseCoords = do
	_ <- AP.string "position=<"
	AP.skipSpace
	px <- AP.signed AP.decimal 
	_ <- (AP.char ',' >> AP.skipSpace)
	py <- AP.signed AP.decimal
	_ <- (AP.takeWhile (/= '<') >> AP.char '<' >> AP.skipSpace) AP.<?> "Here"
	vx <- AP.signed AP.decimal
	_ <- (AP.char ',' >> AP.skipSpace)
	vy <- AP.signed AP.decimal
	_ <- AP.char '>'
	pure $ GenCoords (V2 px py) (V2 vx vy)

evolve :: GenCoords -> GenCoords
evolve (GenCoords p v) = GenCoords (p+v) v

comparePos :: Pos -> Pos -> Ordering
comparePos (V2 x y) (V2 xx yy)
	| y < yy = LT
	| y > yy = GT
	| otherwise = compare x xx

display :: Integer -> Integer -> [Pos] -> ([String], Integer, Integer)
display lx ly pos = if spanx > lx || spany > ly
	then
		([show spanx, show spany], spanx, spany)
	else (go []
		(fmap head $ DL.group $ fmap (`decrease` topLeft) pos)
		pixels, spanx, spany)
	where
	spany = (maxy ^. _y) - (miny ^. _y)
	spanx = (maxx ^. _x) - (minx ^. _x)
	pixels = DL.groupBy ((==) `on` (^._y)) $ DL.sortBy (comparePos) $
		V2 <$> [0..(botRight ^. _x)] <*> [0..(botRight ^. _y)]
	go :: [String] -> [Pos] -> [[Pos]] -> [String]
	go acc [] pls = reverse $ (:acc) $ concatMap (fmap $ const '.') pls
	go acc ps [] = trace (show acc) $ error "Out of lines"
	go acc ps (pl:pls) = let
			(inc,nps) = go' [] ps pl
		in go (inc:acc) nps pls
	
	go' :: String -> [Pos] -> [Pos] -> (String, [Pos])
	go' acc [] pps = (reverse (acc ++ fmap (const '.') pps), [])
	go' acc ps [] = (reverse acc, ps)
	go' acc (p:ps) (pp:pps) 
		| p == pp = go' ('#':acc) ps pps
		| otherwise = go' ('.':acc) (p:ps) pps

	topLeft = V2 (minx ^. _x) (miny ^. _y)
	botRight = (V2 (maxx ^. _x) (maxy ^. _y)) - topLeft
	minx = DL.minimumBy (compare `on` (^. _x)) pos
	miny = DL.minimumBy (compare `on` (^. _y)) pos
	maxx = DL.maximumBy (compare `on` (^. _x)) pos
	maxy = DL.maximumBy (compare `on` (^. _y)) pos
	decrease a b = a - b

part1 :: [GenCoords] -> IO ()
part1 = go 0
	where
	lx = 1000
	ly = 1000
	go time gc = do
		let (dsp, spx, spy) = display lx ly $ DL.sortBy comparePos $ fmap (^. pos) gc
		when ((spx < lx) && (spy < ly)) $ do
				putStrLn (unlines dsp)
				putStrLn $ show time
				void (getLine)
		putStrLn $ show (spx, spy)
		go (time+1) (fmap evolve gc)

solve_1 :: IO ()
solve_1 = 
	SysIO.withFile "inputs/day10" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
		part1 $
			head $ drop (10000) $
			iterate (fmap evolve) $
			snd $ partitionEithers $ fmap (AP.parseOnly parseCoords . T.pack) file_lines
