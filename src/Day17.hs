{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Day17 (
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

import Data.Either (fromRight, partitionEithers)
import Data.Maybe (catMaybes, isJust, fromJust)

import Control.Applicative
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

import qualified Control.Monad.State.Strict as ST
import Data.Bits

import qualified System.Random as Rnd

sum' :: (Foldable t, Num a) => t a -> a
sum' = DL.foldl' (+) 0

data MapObject = Clay | Sand deriving (Show, Eq)

mapObjectToChar :: MapObject -> Char
mapObjectToChar Clay = '#'
mapObjectToChar Sand = '.'

data MapState = MapState MapObject Bool Bool deriving Show

mapStateToChar :: MapState -> Char
mapStateToChar (MapState mo True _) = '~'
mapStateToChar (MapState mo False True) = '|'
mapStateToChar (MapState mo False False) = mapObjectToChar mo

dry :: MapObject -> MapState
dry mo = MapState mo False False

isWet :: MapState -> Bool
isWet (MapState _ b _) = b

setDry :: MapState -> MapState
setDry (MapState mo _ ww) = MapState mo False ww

setWet :: MapState -> MapState
setWet (MapState Sand _ ww) = MapState Sand True True
setWet ms@(MapState _ _ ww) = error $ show ms

setWasWet :: MapState -> MapState
setWasWet (MapState mo ws _) = MapState mo ws True

canFlow :: MapState -> Bool
canFlow (MapState Sand False _) = True
canFlow _ = False

mapObj :: MapState -> MapObject
mapObj (MapState mo _ _) = mo

data Grid = Grid
	{ _origin :: (Int, Int)
	, _gridData :: Arr.Array (Int, Int) MapState } deriving Show

makeLenses ''Grid

data CompState = CompState
	{ _rndGen :: Rnd.StdGen
	, _compGrid :: Grid } deriving Show

makeLenses ''CompState

parseRow :: AP.Parser [(Int, Int)]
parseRow = do
	_ <- AP.string "y="
	y <- AP.decimal
	_ <- AP.string ", x="
	lx <- AP.decimal
	_ <- AP.string ".."
	ux <- AP.decimal
	pure $ fmap (,y) [lx..ux]

parseColumn :: AP.Parser [(Int, Int)]
parseColumn = do
	_ <- AP.string "x="
	x <- AP.decimal
	_ <- AP.string ", y="
	ly <- AP.decimal
	_ <- AP.string ".."
	uy <- AP.decimal
	pure $ fmap (x,) [ly..uy]

parseLine :: AP.Parser [(Int,Int)]
parseLine = parseColumn <|> parseRow

grid :: [(Int,Int)] -> Grid
grid clays = Grid (xmin, ymin) $ initial Arr.// (fmap (,dry Clay) $ fmap ((`dec` xmin) *** (`dec` ymin)) clays)
	where
	xmin = DL.minimum $ fmap fst clays
	ymin = 0
	--ymin = DL.minimum $ fmap snd clays
	xmax = DL.maximum $ fmap fst clays
	ymax = DL.maximum $ fmap snd clays
	initial = Arr.array ((0, 0), (xmax-xmin, ymax-ymin)) $
		zip (DL.sort ((,) <$> xrow <*> ycol)) $ repeat (dry Sand)
	xrow = [0..(xmax - xmin)]
	ycol = [0..(ymax - ymin)]
	dec x y = x - y

printGrid :: Grid -> IO ()
printGrid grd =
	putStrLn $ unlines $
		fmap (fmap (mapStateToChar . snd)) $
		DL.groupBy ((==)  `on` (snd . fst)) $
		DL.sortBy (readingOrder `on` fst) $
		Arr.assocs (grd ^. gridData)


evolve :: (Int, Int) -> CompState -> CompState
evolve src cmpSt = --trace (show wet_spots) $
	ST.execState (do
		mapM_ mapper wet_spots
		sms <- ((Arr.! relSrc) <$> (use (compGrid.gridData))) --source map state
		when (not (isWet sms)) $
			(use rndGen >>= \gen -> pure (Rnd.randomR (0,10) gen :: (Int, Rnd.StdGen)) >>= \(d, ng) ->
				(if d `mod` 7 == 0
					then (compGrid.gridData) %= (Arr.// [(relSrc, setWet sms)])
					else pure ()) >> rndGen .= ng) --When the source is dry wet it
	) cmpSt
	where
	grd = cmpSt ^. compGrid
	relSrc = (fst src - (fst $ (grd ^. origin)), snd src)
	((xmin,ymin),(xmax,ymax)) = Arr.bounds (grd ^. gridData)
	wet_spots = DL.sortBy (flip $ (readingOrder `on` fst)) $ filter (isWet . snd) $ Arr.assocs (grd ^. gridData)
	swap (x,y) = (y,x)

	--mapper (idx, ms@(MapState _ False _)) = trace (show idx) $
	--	if (idx == (srcx-xmin, srcy))
	--		then ST.modify (Arr.// [(idx, setWet ms)]) --When the source is dry wet it
	--		else pure ()
	mapper (idx@(x, y), ms) =
		if (y == ymax)
			then (compGrid.gridData) %= (Arr.// [(idx, setDry ms)]) -- sink at the bottom
			else ((Arr.! (x, y+1)) <$> (use (compGrid.gridData))) >>= \under ->
				if canFlow under
					then do--(compGrid.gridData) %= (Arr.// [(idx, setDry ms), ((x, y+1), setWet under)]) --flow down
						unders <- ((use (compGrid.gridData)) >>= \gd -> pure $ fmap (\yy ->
							((x,yy), (gd Arr.! (x,yy)))) [y+1..ymax])
						case takeWhile (canFlow . snd) unders of
							[] -> error "This shouldn't happen"
							as -> let
									((lx,ly),lms) = last as
									fss = init as
								in do
									(compGrid.gridData) %= (\gd -> (gd Arr.//) $ fmap (second setWasWet) fss)
									(compGrid.gridData) %= (Arr.// [(idx, setDry ms), ((lx,ly), setWet lms)])
					else trace (show idx) $ do
						gd <- use (compGrid.gridData)
						let lf = fmap (canFlow . (gd Arr.!)) [(x-1, y), (x+1, y)]
						--let dbg = fmap (gd Arr.!) [(x-1, y), (x+1, y)]
						--trace (show lf) $ pure ()
						--trace (show dbg) $ pure ()
						let leftms = gd Arr.! (x-1,y)
						let rightms = gd Arr.! (x+1,y)
						case lf of
							[True, True] -> (use rndGen >>= \gen -> pure (Rnd.randomR (0,1) gen :: (Int, Rnd.StdGen)) >>= \(d, ng) -> do
								if d == 1
									then (compGrid.gridData) %= (Arr.// [(idx, setDry ms), ((x-1, y), setWet leftms)]) --flow left
									else (compGrid.gridData) %= (Arr.// [(idx, setDry ms), ((x+1, y), setWet rightms)]) --flow right
								rndGen .= ng)
							[True, False] -> (compGrid.gridData) %= (Arr.// [(idx, setDry ms), ((x-1, y), setWet leftms)]) --flow left
								--lefters <- ((use (compGrid.gridData)) >>= \gd -> pure $
								--	fmap (\xx -> ((xx,y), (gd Arr.! (xx,y)))) $
								--	takeWhile (>0) $ iterate (\xx -> xx - 1) (x-1))
								----trace (show $ takeWhile (>0) $ iterate (\xx -> xx - 1) x) $ pure ()
								----trace (show lefters) $ pure ()
								--case takeWhile (canFlow . snd) lefters of
								--	[] -> pure () --error "This shouldn't happen"
								--	as -> let
								--			((lx,ly),lms) = last as
								--			fss = init as
								--		in do
								--			(compGrid.gridData) %= (\gd -> (gd Arr.//) $ fmap (second setWasWet) fss)
								--			(compGrid.gridData) %= (Arr.// [(idx, setDry ms), ((lx,ly), setWet lms)])
							[False, True] -> (compGrid.gridData) %= (Arr.// [(idx, setDry ms), ((x+1, y), setWet rightms)]) --flow right
								--righters <- ((use (compGrid.gridData)) >>= \gd -> pure $
								--	fmap (\xx -> ((xx,y), (gd Arr.! (xx,y)))) $
								--	takeWhile (<xmax) $ iterate (+1) (x+1))
								--case takeWhile (canFlow . snd) righters of
								--	[] -> pure () --error "This shouldn't happen"
								--	as -> let
								--			((lx,ly),lms) = last as
								--			fss = init as
								--		in do
								--			(compGrid.gridData) %= (\gd -> (gd Arr.//) $ fmap (second setWasWet) fss)
								--			(compGrid.gridData) %= (Arr.// [(idx, setDry ms), ((lx,ly), setWet lms)])
							[False, False] -> pure ()

solve_1 :: IO ()
solve_1 =
	SysIO.withFile "inputs/day17b" SysIO.ReadMode $ \input_fh ->
		(lines <$> (SysIO.hGetContents input_fh)) >>= \file_lines -> do
		mapM_ (\(CompState _ grd) -> printGrid grd >> getLine) $
			iterate (evolve (500, 0)) $ CompState (Rnd.mkStdGen 12) $
			grid $ fmap (second (+10)) $ join $
			snd $ partitionEithers $ fmap (AP.parseOnly parseLine . T.pack) $ file_lines

readingOrder :: Ord a => (a,a) -> (a,a) -> Ordering
readingOrder = compare `on` swap
	where
	swap (x, y) = (y,x)
