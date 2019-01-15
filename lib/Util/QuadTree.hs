{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{- DOESN'T WORK YET -}
module Util.QuadTree where

import qualified Linear as L
import Control.Lens hiding (Empty, contains)
import Data.Function (on)

type Pos = L.V2 Int

data Size = Size
	{ _width :: Int
	, _height :: Int } deriving Show

makeLenses ''Size

{------------------------------------------------------------------------------}

-- | Position of a corner. The opposite corner is at
-- | Pos.x + _width, Pos.y + _height
data Rect = Rect
	{ _rectPos :: Pos
	, _rectSize :: Size } deriving Show

class HasRect a where
	rect :: a -> Rect
	-- a `contains` the position Pos
	contains :: a -> Pos -> Bool
	contains a (L.V2 x y) =
		and [x >= xx, x <= xx+w-1, y >= yy, y <= yy+h-1]
		where
		(Rect (L.V2 xx yy) (Size w h)) = rect a

	-- a crosses the cross formed by the position Pos
	crosses :: a -> Pos -> Bool
	crosses a (L.V2 x y) =
		(xx <= x && xx+w-1 >= x) || (yy <= y && yy+h-1 >= y)
		where
		(Rect (L.V2 xx yy) (Size w h)) = rect a

	-- If two figures overlap
	overlap :: HasRect b => a -> b -> Bool
	overlap a b
		| (x > xx+ww || xx > x+w) = False
		| (y > yy+hh || yy > y+h) = False
		| otherwise = True
		where
		(Rect (L.V2 x y) (Size w h)) = rect a
		(Rect (L.V2 xx yy) (Size ww hh)) = rect b
	
	pos :: a -> Pos
	pos = _rectPos . rect

instance HasRect Rect where
	rect :: Rect -> Rect
	rect = id

	overlap :: HasRect b => Rect -> b -> Bool
	overlap (Rect (L.V2 x y) (Size w h)) b
			| (x > xx+ww || xx > x+w) = False
			| (y > yy+hh || yy > y+h) = False
			| otherwise = True
		where
		(Rect (L.V2 xx yy) (Size ww hh)) = rect b

{------------------------------------------------------------------------------}
-- Relative position of two points in 2-d
data RelPos = NE | NW | SW | SE | CO deriving Show

relPos :: Pos -> Pos -> RelPos
relPos (L.V2 x y) (L.V2 xx yy)
	| (x >= xx) && (y > yy) = NE
	| (x < xx) && (y >= yy) = NW
	| (x <= xx) && (y < yy) = SW
	| (x > xx) && (y <= yy) = SE
	| otherwise = CO
{------------------------------------------------------------------------------}

data QuadTree a
	= Empty
	| Leaf Pos [a]
	| Branch Pos [a] (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
	deriving Show

instance Functor QuadTree where
	fmap _ Empty = Empty
	fmap f (Leaf p as) = Leaf p $ fmap f as
	fmap f (Branch p as ne nw sw se) = Branch p (fmap f as)
		(fmap f ne) (fmap f nw) (fmap f sw) (fmap f se)

qtToList :: QuadTree a -> [a]
qtToList Empty = []
qtToList (Leaf _ as) = as
qtToList (Branch _ as ne nw sw se) = as ++ concatMap (qtToList) [ne, nw, sw, se]

insert :: HasRect a => a -> QuadTree a -> QuadTree a
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

-- Return all the elements in the quadtree that inserct with the point
-- Should be more general, I'd like to count for example
pinch :: HasRect a => Pos -> QuadTree a -> [a]
pinch = pinchHelp []
	where
	pinchHelp :: HasRect a => [a] -> Pos -> QuadTree a -> [a]
	pinchHelp acc _ Empty = acc
	pinchHelp acc p (Leaf c as) = acc ++ filter (`contains` p) as
	pinchHelp acc p (Branch c as ne nw sw se) = let
		nacc = filter (`contains` p) as
		in (case relPos p c of
			NE -> (pinchHelp (acc++nacc) p ne)
			NW -> (pinchHelp (acc++nacc) p nw)
			SW -> (pinchHelp (acc++nacc) p sw)
			SE -> (pinchHelp (acc++nacc) p se)
			CO -> nacc)

collide :: (HasRect a, HasRect b) => a -> QuadTree b -> [b]
collide = collideH []
	where
	collideH acc _ (Empty) = acc
	collideH acc r (Leaf c as) = acc ++ filter (`overlap` r) as
	collideH acc r (Branch c as ne nw sw se) =
		let
			(Rect _ (Size w h)) = rect r
			nacc = acc ++ filter (`overlap` r) as
		in
		(case (relPos (pos r) c, relPos (pos r + (L.V2 w h)) c) of
			(NE, NE) -> (collideH nacc r ne)
			(NW, NW) -> (collideH nacc r nw)
			(SW, SW) -> (collideH nacc r sw)
			(SE, SE) -> (collideH nacc r se)
	
			(NE, CO) -> (collideH nacc r ne)
			(NW, CO) -> (collideH nacc r nw)
			(SW, CO) -> (collideH nacc r sw)
			(SE, CO) -> (collideH nacc r se)
	
			(CO, NE) -> (collideH nacc r ne)
			(CO, NW) -> (collideH nacc r nw)
			(CO, SW) -> (collideH nacc r sw)
			(CO, SE) -> (collideH nacc r se)
	
			(NW, NE) -> collideH nacc r nw ++ collideH nacc r ne
			(NE, NW) -> collideH nacc r nw ++ collideH nacc r ne --This will never happen
	
			(NW, SW) -> collideH nacc r nw ++ collideH nacc r sw
			(SW, NW) -> collideH nacc r nw ++ collideH nacc r sw --This will never happen
	
			(SW, SE) -> collideH nacc r sw ++ collideH nacc r se
			(SE, SW) -> collideH nacc r sw ++ collideH nacc r se --This will never happen
	
			(NE, SE) -> collideH nacc r ne ++ collideH nacc r se
			(SE, NE) -> collideH nacc r ne ++ collideH nacc r se --This will never happen
			_ -> collideH nacc r ne ++ collideH nacc r se ++ collideH nacc r nw ++ collideH nacc r sw)
