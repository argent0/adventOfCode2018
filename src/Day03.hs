{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Day03 (
) where

import Linear (V2)
import qualified Linear as L
import Control.Lens hiding (contains, Empty)
import qualified System.IO as SysIO
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import qualified Data.List as DL

newtype Id = Id Int deriving Show

data Size = Size
	{ _width :: Int
	, _height :: Int } deriving Show

makeLenses ''Size

type Pos = V2 Int

data Rect = Rect Pos Size deriving Show

class IsFig s where
	contains :: s -> Pos -> Bool
	crosses :: s -> Pos -> Bool
	pos :: s -> Pos

instance IsFig Rect where
	contains :: Rect -> Pos -> Bool
	contains (Rect (L.V2 xx yy) (Size w h)) (L.V2 x y) =
		and [x >= xx, x <= xx+w, y >= yy, y <= yy+h]
	crosses :: Rect -> Pos -> Bool
	crosses (Rect (L.V2 xx yy) (Size w h)) (L.V2 x y) =
		(xx < x && xx+w > x) || (yy < y && yy+h > yy+h)
	pos :: Rect -> Pos
	pos (Rect p _) = p

data Claim = Claim Id Rect deriving Show

instance IsFig Claim where
	contains :: Claim -> Pos -> Bool
	contains (Claim _ r) p = contains r p
	crosses :: Claim -> Pos -> Bool
	crosses (Claim _ r) p = crosses r p
	pos :: Claim -> Pos
	pos (Claim _ r) = pos r

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
		(Rect (L.V2 (read x) (read y)) (Size (read w) (read h)))

data QuadTree a
	= Empty
	| Leaf Pos [a]
	| Branch Pos [a] (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
	deriving Show

insert :: IsFig a => a -> QuadTree a -> QuadTree a
insert a Empty = Leaf (pos a) [a]
insert a (Leaf c as) =
	if a `crosses` c
		then Leaf c (a:as)
		else case relPos (pos a) c of
			NE -> Branch c as (Leaf (pos a) [a]) Empty Empty Empty
			NW -> Branch c as Empty (Leaf (pos a) [a]) Empty Empty
			SW -> Branch c as Empty Empty (Leaf (pos a) [a]) Empty
			SE -> Branch c as Empty Empty Empty (Leaf (pos a) [a])
			CO -> Leaf c (a:as)
insert a (Branch c as ne nw sw se) =
	if a `crosses` c
		then Branch c (a:as) ne nw sw se
		else case relPos (pos a) c of
			NE -> Branch c as (insert a ne) nw sw se
			NW -> Branch c as ne (insert a nw) sw se
			SW -> Branch c as ne nw (insert a sw) se
			SE -> Branch c as ne nw sw (insert a se)
			CO -> Branch c (a:as) ne nw se se

collide :: IsFig a => Pos -> QuadTree a -> Integer
collide p (Empty) = 0
collide p (Leaf c as) =
	DL.foldl' (+) 0 $ (const 1) <$> filter (`contains` p) as
collide p (Branch c as ne nw sw se) =
	(DL.foldl' (+) 0 $ (const 1) <$> filter (`contains` p) as) +
	(case relPos p c of
		NE -> (collide p ne)
		NW -> (collide p nw)
		SW -> (collide p sw)
		SE -> (collide p se)
		CO -> 0)

data RelPos = NE | NW | SW | SE | CO deriving Show

relPos :: Pos -> Pos -> RelPos
relPos (L.V2 x y) (L.V2 xx yy)
	| (x >= xx) && (y > yy) = NE
	| (x < xx) && (y >= yy) = NW
	| (x <= xx) && (y < yy) = SW
	| (x > xx) && (y <= yy) = SE
	| otherwise = CO

rect1 :: Rect
rect1 = Rect (L.V2 (-1) (-1)) (Size 2 2)
rect2 = Rect (L.V2 (-100) (-100)) (Size 101 101)
rect3 = Rect (L.V2 100 100) (Size 1 1)

qt :: QuadTree Rect
qt = DL.foldl' (flip insert) Empty [rect1, rect2, rect3]

solveWithQt :: QuadTree Claim -> Integer
solveWithQt quadTree =
	DL.foldl' (+) 0 $
	fmap (const 1) $
	filter (>1) $
	fmap (`collide` quadTree) $
	(L.V2 <$> [1..1000] <*> [1..1000])

solve_1 :: IO ()
solve_1= do
	SysIO.withFile "inputs/day03" SysIO.ReadMode $ \input_fh ->
		((T.lines . T.pack) <$> SysIO.hGetContents input_fh) >>= \file_lines ->
		print $
			solveWithQt $
			DL.foldl' (flip insert) Empty $ uncurry (++) $
			DL.partition (`contains` (L.V2 500 500)) $
			fmap (\(AP.Done _ c) -> c) $
			fmap (\(AP.Partial p) -> p "") (fmap (AP.parse parseClaim) file_lines)

{- 90105 is not the answer -}
