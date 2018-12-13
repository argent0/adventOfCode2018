{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Day13 (
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
import Control.Monad (forever, when, join, replicateM, void, foldM, foldM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (ord)
import Control.Arrow
import Control.Lens
import qualified Data.Array.IArray as Arr
import Linear hiding (trace)

import Data.Functor.Foldable --recursion-schemes
import Data.Functor.Foldable.TH --makeBaseFunctor
import Control.Comonad.Cofree

import qualified Data.Foldable as Fld

import Util.Grid

sum' :: (Foldable t, Num a) => t a -> a
sum' = DL.foldl' (+) 0

data Orientation = Up | Down | Lft | Rgt deriving Show
-- Thu, 13 Dec 2018 02:03:23 -0300

cartChrToOrient :: Char -> Orientation
cartChrToOrient '^' = Up
cartChrToOrient 'v' = Down
cartChrToOrient '>' = Rgt
cartChrToOrient '<' = Lft
cartChrToOrient _ = error "cartChrToOrient"
--Thu, 13 Dec 2018 04:09:24 -0300

orientToChar :: Orientation -> Char
orientToChar Up   = '^'
orientToChar Down = 'v'
orientToChar Lft  = '<'
orientToChar Rgt  = '>'
--Thu, 13 Dec 2018 03:56:12 -0300

data Turn = TLeft | TStraight | TRight deriving Show

--nextTurn :: Maybe Turn -> Turn
--nextTurn Nothing = TLeft
--nextTurn (Just TLeft) = TStraight
--nextTurn (Just TStraight) = TRight
--nextTurn (Just TRight) = TLeft
--Thu, 13 Dec 2018 02:05:59 -0300

nextTurn :: Turn -> Turn
nextTurn (TLeft) = TStraight
nextTurn (TStraight) = TRight
nextTurn (TRight) = TLeft
--Thu, 13 Dec 2018 02:51:33 -0300

data Track = Horiz | Vert 
	| CurvA -- /
	| CurvB -- \
	| Inter -- +
	deriving Show
--Thu, 13 Dec 2018 02:12:21 -0300
--Thu, 13 Dec 2018 02:24:56 -0300

parseTracks :: [(Int, [(Int, Maybe Track)])] -> Grid2d Int (Maybe Track)
parseTracks tr = Arr.array (0, maxy) $
		fmap (second (Arr.array (0,maxx))) tr
	where
	miny = 0
	maxy = fst $ last tr
	minx = 0
	maxx = fst $ last $ snd $ head tr
--Thu, 13 Dec 2018 02:35:08 -0300
--Thu, 13 Dec 2018 02:37:59 -0300

queryTracks :: Grid2d Int (Maybe Track) -> V2 Int -> Track
queryTracks tracks p@(V2 x y) = case (tracks Arr.! y) Arr.! x of
	Nothing -> error $ "queryTracks " ++ show p
	Just t -> t
--Thu, 13 Dec 2018 03:31:16 -0300

parseTrack :: Char -> Maybe Track
parseTrack ' ' = Nothing
parseTrack '-' = Just Horiz
parseTrack '|' = Just Vert
parseTrack '/' = Just CurvA
parseTrack '\\' = Just CurvB
parseTrack '+' = Just Inter
parseTrack '>' = Just Horiz
parseTrack '<' = Just Horiz
parseTrack 'v' = Just Vert
parseTrack '^' = Just Vert
parseTrack c = error $ show c

trackChar :: Maybe Track -> Char
trackChar Nothing      = ' '
trackChar (Just Horiz) = '-'
trackChar (Just Vert ) = '|'
trackChar (Just CurvA) = '/'
trackChar (Just CurvB) = '\\'
trackChar (Just Inter) = '+'
--Thu, 13 Dec 2018 03:47:10 -0300

data Cart = Cart
	{ _cartPos :: V2 Int
	, _cartOrt :: Orientation
	, _cartNtrn :: Turn } deriving Show
makeLenses ''Cart
--Thu, 13 Dec 2018 02:39:47 -0300

mkCart :: V2 Int -> Orientation -> Cart
mkCart p o = Cart p o TLeft
--Thu, 13 Dec 2018 02:50:26 -0300

isCart :: Char -> Bool
isCart '>' = True
isCart '<' = True
isCart 'v' = True
isCart '^' = True
isCart _ = False
--Thu, 13 Dec 2018 02:41:31 -0300

unitX :: V2 Int
unitX = V2 1 0

unitY :: V2 Int
unitY = V2 0 1
--Thu, 13 Dec 2018 03:01:02 -0300

nextCart :: Cart -> Track -> Cart
nextCart (Cart p Lft nt) Horiz = Cart (p-unitX) Lft nt
nextCart (Cart p Rgt nt) Horiz = Cart (p+unitX) Rgt nt

nextCart (Cart p Up nt) Vert = Cart (p-unitY) Up nt
nextCart (Cart p Down nt) Vert = Cart (p+unitY) Down nt

-- /
nextCart (Cart p Down nt) CurvA = Cart (p-unitX) Lft   nt --There was no choice
nextCart (Cart p Up nt)   CurvA = Cart (p+unitX) Rgt   nt --idem
nextCart (Cart p Lft nt)  CurvA = Cart (p+unitY) Down  nt --idem
nextCart (Cart p Rgt nt)  CurvA = Cart (p-unitY) Up    nt --idem

-- \
nextCart (Cart p Up nt) CurvB   = Cart (p-unitX) Lft nt --idem
nextCart (Cart p Rgt nt) CurvB  = Cart (p+unitY) Down nt --idem
nextCart (Cart p Down nt) CurvB = Cart (p+unitX) Rgt nt --idem
nextCart (Cart p Lft nt) CurvB  = Cart (p-unitY) Up nt --idem

nextCart (Cart p Up   TLeft) Inter = Cart (p-unitX) Lft  (nextTurn TLeft)
nextCart (Cart p Down TLeft) Inter = Cart (p+unitX) Rgt  (nextTurn TLeft)
nextCart (Cart p Lft  TLeft) Inter = Cart (p+unitY) Down (nextTurn TLeft)
nextCart (Cart p Rgt  TLeft) Inter = Cart (p-unitY) Up   (nextTurn TLeft)

nextCart (Cart p Up   TRight) Inter = Cart (p+unitX) Rgt  (nextTurn TRight)
nextCart (Cart p Down TRight) Inter = Cart (p-unitX) Lft  (nextTurn TRight)
nextCart (Cart p Lft  TRight) Inter = Cart (p-unitY) Up   (nextTurn TRight)
nextCart (Cart p Rgt  TRight) Inter = Cart (p+unitY) Down (nextTurn TRight)

nextCart (Cart p Up   TStraight) Inter = Cart (p-unitY) Up  (nextTurn TStraight)
nextCart (Cart p Down TStraight) Inter = Cart (p+unitY) Down  (nextTurn TStraight)
nextCart (Cart p Lft  TStraight) Inter = Cart (p-unitX) Lft   (nextTurn TStraight)
nextCart (Cart p Rgt  TStraight) Inter = Cart (p+unitX) Rgt (nextTurn TStraight)
--Thu, 13 Dec 2018 03:18:35 -0300

type Tracks = Grid2d Int (Maybe Track)
--Thu, 13 Dec 2018 03:40:01 -0300

comparePositions :: V2 Int -> V2 Int -> Ordering
comparePositions = compare `on` swap
	where
	swap (V2 x y) = V2 y x

-- stops at the first crash
evolve :: Grid2d Int (Maybe Track) -> [Cart] -> ([Cart], Maybe (V2 Int)) --a posssible crash location
evolve tracks = go ([], Nothing) . DL.sortBy (comparePositions `on` ((^. cartPos)))
	where
	go acc [] = acc
	go acc@(_, Just _) _ = acc --Thu, 13 Dec 2018 03:25:59 -0300
	go (done, Nothing) (cart:carts) =
		let
			nc = nextCart cart (tracks `queryTracks` (cart ^. cartPos))
		in if (nc ^. cartPos) `DL.elem` (fmap (^. cartPos) $ carts ++ done) --Why am I tracking the position in the Cart?
			--Thu, 13 Dec 2018 15:43:27 -0300
			then (done, Just (nc ^. cartPos))
			else go (nc:done, Nothing) carts

evolve2 :: Grid2d Int (Maybe Track) -> [Cart] -> [Cart] --a posssible crash location
evolve2 tracks = go [] . DL.sortBy (comparePositions `on` ((^. cartPos)))
	where
	go acc [] = acc
	go done (cart:carts) =
		let
			nc = nextCart cart (tracks `queryTracks` (cart ^. cartPos))
		in if (nc ^. cartPos) `DL.elem` (fmap (^. cartPos) $ carts ++ done) --Why am I tracking the position in the Cart?
			--Thu, 13 Dec 2018 15:43:27 -0300
			then go (filter (not . (==(nc ^. cartPos)) . (^. cartPos) ) done)
					(filter (not . (==(nc ^. cartPos)) . (^. cartPos) ) carts)
			else go (nc:done) carts

represent :: Tracks -> [Cart] -> IO ()
represent tracks carts = do
	print carts
	putStrLn $
		unlines $ Arr.elems $ Arr.amap (Arr.elems) $ 
		foldr updt charArr carts
	where
	(ymin, ymax) = Arr.bounds tracks
	(xmin, xmax) = Arr.bounds (head $ Arr.elems tracks)
	charArr = Arr.amap (Arr.amap trackChar) tracks
	updt (Cart (V2 x y) or _) ar = ar Arr.// [(y, (ar Arr.! y) Arr.// [(x, orientToChar or)])]

solve_1 :: IO ()
solve_1 =
	SysIO.withFile "inputs/day13" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
		let tracks =
			parseTracks $
			zip [0..] $ fmap (zip [0..]) $
			fmap (fmap parseTrack . filter (/='\n') ) file_lines
		let carts =
			fmap (\(y,(x,c)) -> mkCart (V2 x y) (cartChrToOrient c)) $
			join $ fmap (\(y, cs) -> fmap (y,) cs) $
			filter (not . null . snd) $
			fmap (second ( filter (isCart . snd) ) ) $
			zip [0..] $ fmap (zip [0..]) $
			fmap (filter (/='\n') ) file_lines
		foldM_ (\crts tick -> do
			print tick
			--represent tracks crts --Thu, 13 Dec 2018 02:44:08 -0300
			--_ <- getLine
			let ncrts = evolve2 tracks crts
			case ncrts of
				[c] -> do
					represent tracks crts
					error $ show c
				_ -> pure $ ncrts
			) carts [0..]
		where
		--Thu, 13 Dec 2018 02:20:21 -0300
		--Thu, 13 Dec 2018 02:35:32 -0300
		--
--Thu, 13 Dec 2018 04:31:26 -0300: Got the simple example working
--Thu, 13 Dec 2018 04:35:40 -0300 112,57 is not the right answer
--Thu, 13 Dec 2018 05:18:20 -0300 57,104 is right


{- Sample recursion schemes -}

--data Expr
--	= Index Expr Expr
--	| Call Expr [Expr]
--	| Lit Int
--
--makeBaseFunctor ''Expr
--
--lenAlg :: Base Expr Int -> Int
--lenAlg (IndexF a b) = a + b
--lenAlg (CallF a bs) = a + sum' bs
--lenAlg (LitF _) = 1

-- cata lenAlg ...

{- Sample fibonacci series -}
--data Nat
--	= Z
--	| S Nat
--
--makeBaseFunctor ''Nat
--
--intToNat :: Int -> Nat
--intToNat 0 = Z
--intToNat n = S (intToNat (n-1))
--
--fib :: Int -> Int
--fib = histo go . intToNat
--	where
--	go ZF = 0
--	go (SF (a Control.Comonad.Cofree.:< ZF)) = 1
--	go (SF (a Control.Comonad.Cofree.:< SF (b Control.Comonad.Cofree.:< c))) =
--		a + b

