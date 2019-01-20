{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Day03 (
	solve_1
) where

import Linear (V2)
import qualified Linear as L
import Control.Lens hiding (contains, Empty, (:<))
import qualified System.IO as SysIO
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import qualified Data.List as DL
import Debug.Trace

import qualified Util.QuadTree as QT

import Data.List.NonEmpty

{-
data QuadTree a
	= Leaf a
	| Branch a (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
	deriving (Show, Functor, Foldable)

-- | Rectangle constructor
rect :: Int -> Int -> Int -> Int -> QT.Rect
rect x y w h = QT.Rect (L.V2 x y) (QT.Size w h)

-- | p `relPos` q = relative position of p w.r.t. q
--
-- >>> relPos (L.V2 1 1) (L.V2 0 0)
-- NE
-- >>> relPos (L.V2 (-1) 1) (L.V2 0 0)
-- NW
-- >>> relPos (L.V2 (-1) (-1)) (L.V2 0 0)
-- SW
-- >>> relPos (L.V2 1 (-1)) (L.V2 0 0)
-- SE
relPos :: QT.Pos -> QT.Pos -> QT.RelPos
relPos (L.V2 x y) (L.V2 xx yy)
	| (x >= xx) && (y > yy) = QT.NE
	| (x < xx) && (y >= yy) = QT.NW
	| (x <= xx) && (y < yy) = QT.SW
	| (x > xx) && (y <= yy) = QT.SE
	| otherwise = QT.CO

-- | Relative position between a square and a point
--
-- >>> rectPointRelPos (rect (-1) 1 2 2) (L.V2 0 0)
-- CO
-- >>> rectPointRelPos (rect (-2) (-2) 1 1) (L.V2 0 0)
-- SW
-- >>> rectPointRelPos (rect (-1) (-1) 2 1) (L.V2 0 0)
-- CO
rectPointRelPos :: QT.Rect -> QT.Pos -> QT.RelPos
rectPointRelPos (QT.Rect pos@(L.V2 x y) (QT.Size w h)) ref@(L.V2 xx yy) =
	case relPos pos ref of
		QT.NW -> if (x + w < xx) && (y + h <= yy) then QT.NW else QT.CO
		QT.SW -> if (x + w <= xx) then QT.SW else QT.CO
		QT.NE -> if (y + h < y) then QT.NE else QT.CO
		o -> o

newtype Id = Id Int deriving Show

data Claim = Claim Id QT.Rect deriving Show

instance QT.HasRect Claim where
	rect (Claim _ r) = r

parseClaim :: Parser Claim
parseClaim = do
	_ <- AP.char '#'
	id <- AP.many1 AP.digit --One or more digits
	_ <- AP.string " @ "

	x <- AP.many1 AP.digit
	_ <- AP.char ','
	y <- AP.many1 AP.digit

	_ <- AP.string ": "
	w <- AP.many1 AP.digit
	_ <- AP.char 'x'
	h <- AP.many1 AP.digit
	return $ Claim
		(Id $ read id)
		(QT.Rect (L.V2 (read x) (read y)) (QT.Size (read w) (read h)))

-- This works ok
solveWithQt :: QT.QuadTree Claim -> Integer
solveWithQt quadTree =
	DL.foldl' (+) 0 $
	fmap (const 1) $
	DL.filter (>1) $
	fmap (DL.foldl' (+) 0 . fmap (const 1) . (`QT.pinch` quadTree)) $
	(L.V2 <$> [1..1000] <*> [1..1000])

-- ~1.4*10^9 operations (worst case) ~ 15 min at 1.5 GHz
solveWithClaims :: [Claim] -> Integer
solveWithClaims claims =
	DL.foldl' (+) 0 $
	fmap (const 1) $
	DL.filter pred $
	(L.V2 <$> [1..1000] <*> [1..1000])
	where
	pred :: QT.Pos -> Bool
	pred p = go 0 claims p
	go :: Integer -> [Claim] -> QT.Pos -> Bool
	go acc [] _ = (acc > 1)
	go acc (c:cs) p
		| (c `QT.contains` p) =
			if (acc > 0) then True else go (acc+1) cs p
		| otherwise = go (acc) cs p

-- collide all claims with the quadtree
solve2WithQt :: QT.QuadTree Claim -> [Claim]
solve2WithQt quadTree =
	fmap fst $
	DL.filter ((==1).snd) $
	fmap (\c@(Claim _ r) -> (c,
		DL.foldl' (+) 0 $ fmap (const 1) $ r `QT.collide` qtr)) $
	QT.qtToList quadTree
	where
	qtr :: QT.QuadTree QT.Rect
	qtr = fmap (\(Claim _ r) -> r) quadTree

solve_1 :: IO ()
solve_1= do
	SysIO.withFile "inputs/day03" SysIO.ReadMode $ \input_fh ->
		((T.lines . T.pack) <$> SysIO.hGetContents input_fh) >>= \file_lines ->
		print $
			-- mapM_ (\(L.V2 x y) -> putStrLn $ (show x) ++ " " ++ (show y)) $
			solve2WithQt $
			DL.foldl' (flip QT.insert) QT.Empty $ uncurry (++) $
			DL.partition (`QT.contains` (L.V2 500 500)) $
			--solveWithClaims $
			--filter (`contains` (L.V2 1 159)) $
			--filter (`contains` (L.V2 1 160)) $
			fmap (\(AP.Done _ c) -> c) $
			fmap (\(AP.Partial p) -> p "") (fmap (AP.parse parseClaim) file_lines)
		
-}
solve_1 :: IO ()
solve_1 = undefined

{- 90105 is too low -}
{- 118223 is right -}
{- 137163 is too high -}
