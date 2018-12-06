{-# LANGUAGE OverloadedStrings #-}
module Day06 (
	solve_1
) where

import qualified System.IO as SysIO
import Data.Function (on)
import qualified Data.List as DL
import qualified Data.Attoparsec.Text as AP
import qualified Linear as L
import qualified Data.Text as T
import Data.Either (partitionEithers)
import Data.Maybe (fromJust, isJust)
import Control.Monad (when)
import Control.Lens
import Control.Arrow
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace
import qualified Data.Set as Set

type Point = L.V2 Int

parsePoint :: AP.Parser Point
parsePoint = do
	x <- AP.decimal
	_ <- (AP.char ',' >> AP.char ' ')
	y <- AP.decimal
	pure $ L.V2 x y

manDist :: Point -> Point -> Int
manDist (L.V2 x y) (L.V2 xx yy) =
	(max xx x - min xx x) +
	(max yy y - min yy y)

minCorner :: [Point] -> Point
minCorner = uncurry L.V2 .
	(((^. L._x) . (DL.minimumBy (compare `on` (^. L._x))))
	&&& ((^. L._y) . (DL.minimumBy (compare `on` (^. L._y)))))

maxCorner :: [Point] -> Point
maxCorner = uncurry L.V2 .
	(((^. L._x) . (DL.maximumBy (compare `on` (^. L._x))))
	&&& ((^. L._y) . (DL.maximumBy (compare `on` (^. L._y)))))

minimums :: (a -> a -> Ordering) -> [a] -> [a]
minimums cmp = foldr folder []
	where
	folder a [] = a:[]
	folder a (b:bs) = case cmp a b of
		EQ -> (a:b:bs)
		LT -> a:[]
		GT -> (b:bs)

-- There are 96142 points inside the region to consider
-- there are 50 points
-- There would be: 4 757 100 distances to compute
solve1 :: [Point] -> Integer
solve1 centers =
	DL.maximum . Map.elems $
	(`Map.withoutKeys` (Set.fromList ch)) $ --Remove the convex hull
	Map.fromListWith (+) $
	fmap (first fromJust) $ filter (isJust . fst) $
	fmap (\(a, b) -> (b, 1)) $
	fmap (\p -> (p, closestCenter p )) points
	where
	closestCenter :: Point -> Maybe Point
	closestCenter p =
		case (minimums (compare `on` snd) $ fmap (\c -> (c, manDist p c)) centers) of
			[(c,_)] -> Just c
			_ -> Nothing
	points = L.V2 <$> [mnx..mxx] <*> [mny..mxy]
	(L.V2 mnx mny) = minCorner centers
	(L.V2 mxx mxy) = maxCorner centers
	ch = convexHull centers

sum' :: [Integer] -> Integer
sum' = DL.foldl' (+) 0

solve2 :: [Point] -> Integer
solve2 centers = --(flip trace) undefined $ show $
	sum' $ fmap (const 1) $ filter (< 10000) $
	fmap (\p -> sum' $ fmap (fromIntegral . (p `manDist`)) centers) points
	where
	closestCenter :: Point -> Maybe Point
	closestCenter p =
		case (minimums (compare `on` snd) $ fmap (\c -> (c, manDist p c)) centers) of
			[(c,_)] -> Just c
			_ -> Nothing
	points = L.V2 <$> [mnx..mxx] <*> [mny..mxy]
	(L.V2 mnx mny) = minCorner centers
	(L.V2 mxx mxy) = maxCorner centers
	ch = convexHull centers

solve_1 :: IO ()
solve_1 = 
	SysIO.withFile "inputs/day06" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
		let input = partitionEithers $
			fmap (AP.parseOnly parsePoint . T.pack ) file_lines
		when (length (fst input) /= 0) (putStrLn "Parse Error")
		print $ solve2 $ snd input

compareFrom :: Point -> Point -> Point -> Ordering
compareFrom o l r =
  compare ((x l - x o) * (y r - y o)) ((y l - y o) * (x r - x o))
  where
  x = (^. L._x)
  y = (^. L._y)

distanceFrom :: Point -> Point -> Float
distanceFrom from to = ((x to - x from) ** 2 + (y to - y from) ** 2) ** (1 / 2)
  where
  x :: Point -> Float
  x = fromIntegral . (^. L._x)
  y :: Point -> Float
  y = fromIntegral . (^. L._y)

convexHull :: [Point] -> [Point]
convexHull points =
	let
		o = minimum points
		presorted = DL.sortBy (compareFrom o) (filter (/= o) points)
		collinears = DL.groupBy (((EQ ==) .) . compareFrom o) presorted
		outmost = DL.maximumBy (compare `on` (distanceFrom o)) <$> collinears
	in dropConcavities [o] outmost

dropConcavities :: [Point] -> [Point] -> [Point]
dropConcavities (left:lefter) (right:righter:rightest) =
  case compareFrom left right righter of
    LT -> dropConcavities (right : left : lefter) (righter : rightest)
    EQ -> dropConcavities (left : lefter) (righter : rightest)
    GT -> dropConcavities lefter (left : righter : rightest)
dropConcavities output lastInput = lastInput ++ output
