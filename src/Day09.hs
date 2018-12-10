{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
module Day09 (
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
import Control.Monad (when, join, replicateM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (ord)
import Control.Arrow
import Control.Lens
import qualified Data.Array.IArray as Arr

data Circle = Circle ![Integer] !Integer ![Integer] deriving Show--anti current clockwise

showCircle (Circle ccw c cw) = "Circle " ++
	(concatMap (printf " %2d") $ reverse ccw) ++ 
	(if c < 10 then " (" else "(") ++
	show c ++ ")" ++
	(concatMap (printf "%2d ") cw)

clockWise :: Circle -> Circle
clockWise !c@(Circle [] _ []) = c
clockWise !(Circle ccw c (cw:cws)) = Circle (c:ccw) cw cws
clockWise !(Circle ccw c []) =
	Circle (c:[]) (head ncw) (tail ncw)
	where
	ncw = reverse ccw

counterWise :: Circle -> Circle
counterWise !c@(Circle [] _ []) = c
counterWise !(Circle (ccw:ccws) c cw) = Circle ccws ccw (c:cw)
counterWise !(Circle [] c cw) =
	Circle (tail nccw) (head nccw) (c:[])
	where
	nccw = reverse cw

sampleCircle :: Circle
sampleCircle = Circle [1, 1] 0 [2, 2]

put :: Integer -> Circle -> Circle
put n (Circle ccw c cw) = Circle (c:ccw) n cw

remove :: Circle -> (Integer, Circle)
remove (Circle [] _ []) = error "Empty circle"
remove (Circle ccw c []) =
	(c, Circle [] (head ncw) (tail ncw))
	where
	ncw = reverse ccw
remove (Circle ccw c (cw:cws)) = (c, Circle ccw cw cws)

initialCircle :: Circle
initialCircle = Circle [] 0 []

type Marbles = [Integer]
initialMarble :: Integer -> Marbles
initialMarble mx = [1..mx]

advance :: Marbles -> Circle -> Maybe (Integer, Marbles, Circle)
advance [] _ = Nothing
advance (current_marble:next_marbles) circle = Just $ --trace (showCircle $ circle) $
		if (current_marble `mod` 23 == 0)
			then let
				(marble, next_circle) = --possible off by one s/7/8
					remove $ head $ drop 7 $ iterate counterWise circle
				in (marble + current_marble, next_marbles, next_circle)
			else
				(0, next_marbles, put current_marble $ clockWise circle)

play :: Integer -> Integer -> [(Integer, Integer)]
play max_marble n_players =
	go (initialMarble max_marble) (initialCircle) [] (zip [1..n_players] $ repeat 0)
	where
	go :: Marbles -> Circle -> [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
	go !marbles !circle !rearScores [] = --the round starts again
		go marbles circle [] $ reverse rearScores
	go !marbles !circle !rearScores !(!fs:(!fronScores)) =
		case advance marbles circle of
			Nothing -> rearScores ++ (fs:fronScores)
			Just (points, next_marbles, next_circle) ->
					go next_marbles next_circle
						((second (+points) fs):rearScores) fronScores

test :: IO ()
test = print result >>
	(print $ zipWith (-) (fmap snd result) expected_score)
	where
	result = zipWith
		(\mm np -> DL.maximumBy (compare `on` snd) $ play mm np)
		max_marble
		n_players
	n_players = [ 10, 13, 17, 21, 30]
	max_marble = [ 1618, 7999, 1104, 6111, 5807]
	expected_score = [ 8317, 146373, 2764, 54718, 37305]

solve_1 :: IO ()
solve_1 = 
	SysIO.withFile "inputs/day09" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
		--print $ fmap (fmap (AP.parseOnly AP.decimal) . T.pack . words) file_lines
		print $ fmap (
			(\(np:mm:_) -> DL.maximumBy (compare `on` snd) $ play (mm*100) np) . snd . partitionEithers . fmap (AP.parseOnly AP.decimal . T.pack) . words) file_lines

{- input:
 - 486 players; last marble is worth 70833 points
 -}

{- 368343 is too low -}
{-

Circle 16  8 17  4 18 (19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15  0
[5]    16  8 17  4 18 (19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15  0

Circle 16  8 17  4 18 19  2 (24)20 10 21  5 22 11  1 12  6 13  3 14  7 15  0
[6]    16  8 17  4 18 19  2 (24)20 10 21  5 22 11  1 12  6 13  3 14  7 15  0

Circle 16  8 17  4 18 19  2 24 20 (25)10 21  5 22 11  1 12  6 13  3 14  7 15  0
[7]    16  8 17  4 18 19  2 24 20 (25)10 21  5 22 11  1 12  6 13  3 14  7 15  0

-}
