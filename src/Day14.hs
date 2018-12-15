{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Day14 (
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
import Data.Maybe (catMaybes)

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

data CState = CState
	{ _storage :: Arr.Array Int (Maybe Int)
	, _size :: Int
	, _capacity :: Int } deriving Show

makeLenses ''CState

initState :: Int -> CState
initState capa = CState
	(Arr.array (1,capa) $ zip [1..capa] $ repeat Nothing)
	0 capa

pushBack :: Int -> CState -> CState
pushBack n (CState sto sz cap)  = if sz < cap
	then CState (sto Arr.// [(sz+1, Just n)]) (sz+1) cap
	else trace ("New capacity: " ++ show (2*cap)) $ CState
			(Arr.array (1,2*cap) $ (Arr.assocs sto) ++
				((cap+1,Just n):(zip [cap+2..] $ replicate (cap-1) Nothing)))
			(cap+1)
			(2*cap)

csTail :: CState -> Int -> [Int]
csTail (CState sto sz cap) amount
	| amount >= sz = catMaybes $ fmap (sto Arr.!) [1..sz]
	| otherwise = catMaybes $ fmap (sto Arr.!) [sz-amount..sz]

(!) :: CState -> Int -> Int
(!) st idx = case (st ^. storage) Arr.! idx of
	Nothing -> error "Bad index"
	Just r -> r

display :: (Int, Int, CState) -> String
display (e1, e2, st) =
	concat $ catMaybes $
	Arr.elems $
	Arr.accum acc2 
		(Arr.accum acc1 (Arr.amap (fmap (printf " %d ")) $ (st ^. storage)) [(e1,Nothing)])
		[(e2,Nothing)]
	where
	acc1 :: Maybe String -> Maybe String -> Maybe String
	acc1 org Nothing = (printf "(%s)" . filter (/=' ')) <$> org
	acc2 :: Maybe String -> Maybe String -> Maybe String
	acc2 org Nothing = (printf "[%s]" . filter (/=' ')) <$> org

solve1 :: Int -> Int -> Int -> (Int, Int, CState)
solve1 stSize nPractice nPred =
	head $ dropWhile (pred . thrd) $
	--thrd $ last $ take 10 $
		iterate it (1,2, pushBack 7 $ pushBack 3 $ initState stSize)
	where
	pred :: CState -> Bool
	pred = (< nPred + nPractice) . (^. size)
	thrd (_,_,x) = x
	it !(!e1,!e2,!st) = let
			re1 = st ! e1
			re2 = st ! e2
			nSt = foldr (pushBack) st $ nextRecipes re1 re2
			nre1 = (e1 + re1 + 1) `mod` (nSt ^. size)
			nre2 = (e2 + re2 + 1) `mod` (nSt ^. size)
			nnre1 = if nre1 == 0 then nSt ^. size else nre1
			nnre2 = if nre2 == 0 then nSt ^. size else nre2
		--in trace (display (nnre1, nnre2, nSt)) $ (nnre1, nnre2, nSt)
		in (nnre1, nnre2, nSt)

data MatchStatus a
	= FullMatch
	| PartialMatch [a] [a]
	deriving Show

pushMatch :: Eq a => MatchStatus a -> a -> MatchStatus a
pushMatch FullMatch _ = FullMatch
pushMatch (PartialMatch (t:ts) []) a
	| a == t = PartialMatch (t:ts) ts
	| otherwise = PartialMatch (t:ts) []

pushMatch (PartialMatch (t:ts) [p]) a
	| a == p = FullMatch
	| a == t = PartialMatch (t:ts) ts
	| otherwise = PartialMatch (t:ts) []

pushMatch (PartialMatch (t:ts) (p:ps)) a
	| a == p = PartialMatch (t:ts) ps
	| a == t = PartialMatch (t:ts) ts
	| otherwise = PartialMatch (t:ts) []

matchSeq :: Eq a => MatchStatus a -> [a] -> MatchStatus a
matchSeq = Fld.foldl' pushMatch

solve2 :: [Int] -> Int -> (MatchStatus Int,Int,Int, Int, CState)
solve2 target stSize =
	head $ dropWhile (not . pred . sec) $
	--last $ take 21 $
		iterate it
			( PartialMatch target [] --Wether it has matched
			, 2 -- The amount of digits matched so far
			, 1,2, pushBack 7 $ pushBack 3 $ initState stSize)
	where
	pred :: MatchStatus Int -> Bool
	pred FullMatch = True
	pred _ = False
	sec (x,_,_,_,_) = x
	ltarget = length target
	it :: (MatchStatus Int,Int,Int, Int, CState) -> (MatchStatus Int,Int,Int, Int, CState)
	it !(!currMatch, !nRecep, !e1,!e2,!st) = let
			re1 = st ! e1
			re2 = st ! e2
			nxtRecp = nextRecipes re1 re2
			nSt = foldr pushBack st $ nxtRecp
			nre1 = (e1 + re1 + 1) `mod` (nSt ^. size)
			nre2 = (e2 + re2 + 1) `mod` (nSt ^. size)
			nnre1 = if nre1 == 0 then nSt ^. size else nre1
			nnre2 = if nre2 == 0 then nSt ^. size else nre2
			nnRecep = nRecep + length nxtRecp
		--in trace (display (nnre1, nnre2, nSt)) $ (nnre1, nnre2, nSt)
		in case matchSeq (PartialMatch target []) (csTail nSt 20) of
			--NoMatch -> (target, NoMatch, nnRecep, nnre1, nnre2, nSt)
			FullMatch -> (FullMatch, nnRecep, nnre1, nnre2, nSt)
			--ts -> trace ((show ts) ++ " " ++ display (nnre1,nnre2, nSt)) $ (ts, nnRecep, nnre1, nnre2, nSt)
			ms -> (ms, nnRecep, nnre1, nnre2, nSt)

quinq :: (a,b,c,d,e) -> (c,d,e)
quinq (_,_,c,d,e) = (c,d,e)

test1 = solve2 [5,1,5,8,9] 20
test2 = solve2 [0,1,2,4,5] 20
test3 = solve2 [9,2,5,1,0] 20
test4 = solve2 [5,9,4,1,4] 20

--unfold
--Fri, 14 Dec 2018 02:08:54 -0300
nextRecipes :: Int -> Int -> [Int]
nextRecipes e ee
	| s < 10 = [s]
	| otherwise = u:d:( if u + d > 10 then nextRecipes u d else [])
	where
	s = e + ee
	u = s `mod` 10
	d = (s `div` 10) `mod` 10

{- Reddit's solution mstksg, interesting idea that doesn't work. The type
 - signatures have been fixed from the original -}

--chocolatePractice :: [Int]
--chocolatePractice = 3 : 7 : go 0 1 (Seq.fromList [3, 7])
--	where
--	go :: Int -> Int -> Seq.Seq Int -> [Int]
--	go !p1 !p2 !tp = newDigits ++ go p1' p2' tp'
--		where
--		sc1 = tp `Seq.index` p1
--		sc2 = tp `Seq.index` p2
--		newDigits = nextRecipes p1 p2
--		tp' = tp <> Seq.fromList newDigits
--		p1' = (p1 + sc1 + 1) `mod` length tp'
--		p2' = (p2 + sc2 + 1) `mod` length tp'
--
--
--substrLoc :: [Int] -> [Int] -> Int
--substrLoc xs = length
--             . takeWhile (not . (xs `DL.isPrefixOf`))
--             . DL.tails
--
--day14b :: [Int] -> Int
--day14b xs = xs `substrLoc` chocolatePractice

solve_1 :: IO ()
solve_1 =
	putStrLn $ display $
		--solve1 660000  652601 10
		quinq $ solve2 [6,5,2,6,0,1] 20
--	SysIO.withFile "inputs/day13" SysIO.ReadMode $ \input_fh ->
--		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
--		print $ file_lines

{- First run ~ 3h
 -  1  4  5  9  1  1  6  9  9  8  1  6  8  5  2  7  6  9  9  5  2  1  5  5  4  1
 -  7  4  1  1  1  2  2  1  1  2  2  5  9  1  1  0  6  3 
 -  3  8  7  1  8  6  6  6  1  1  4  4  8  1  3  4  1  0  5  1  0  1  5  7  1  3
 -  3  9  1  0  9  7  1  3  2  2  4  1  2  1  2  1  1  2  6  1  0  1  7  5  8  2
 -  1  0  1  1  4  8  4  1  2  2  1  2  8  3  4  9  4
 -}
{- 1221283494 is right!-}
