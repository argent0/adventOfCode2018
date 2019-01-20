{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{- DOESN'T WORK YET -}
module Util.QuadTree where

import qualified Linear as L
import Control.Lens hiding (Empty, contains, (:<))

import Control.Comonad.Cofree
import Data.Functor.Foldable
import Data.Functor.Classes
import Data.Function
import Data.Bool
import Text.Show

-- Partition a collection by separating in two parts that might have different
-- size.
--
-- LeftTurn bt a
--   a
--  /
-- bt
--
-- RightTurn a bt
--   a
--    \
--    bt
--
-- Bifurcation lt a rt
--   a
--  / \
-- lt rt

-- -- Explicit type
--data BinaryPartitioning a
--	-- Only one part
--	= Leaf a
--	-- One part and more to the left
--	| LeftTurn (BinaryPartitioning a) a
--	-- One part and more to the right
--	| RightTurn a (BinaryPartitioning a)
--	-- One part and more to the left and right
--	| Bifurcation (BinaryPartitioning a) a (BinaryPartitioning a)
--	deriving Show

-- -- Or declaring it by annotating the following recursion using `Cofree`
--
-- data BPFuct f
-- 	= LeafF
-- 	| LeftTurnF f
-- 	| RightTurnF f
-- 	| BifurcationF f f
-- 	deriving Show
--
-- type BinaryPartition a = Cofree BPFuct a
--
-- -- The Leaf constructor being
-- leaf :: a -> BinaryPartition a
-- leaf a = a :< LeafF
--
-- -- Etc,
--
-- leftTurn :: a -> BinaryPartition a -> BinaryPartition a
-- leftTurn a lt = a :< LeftTurnF lt
--
-- -- etc...

-- So I want to partition an N x N sheet using four options
--
-- NW|NE
-- --+--
-- SW|SE

data Quadrant = Q1 | Q2 | Q3 | Q4 deriving (Eq,Show)
data Wind = N | S | E | W | NWSE | SWNE deriving (Eq, Show)

data PartF f
	= LeafF
	| VertexF Quadrant f
	| SideF Wind f f
	| VertexCompF Quadrant f f f
	| QuadrantF f f f f
	deriving (Eq, Show, Functor)

instance Eq1 PartF where
	liftEq _ LeafF LeafF = True
	liftEq p (VertexF q c) (VertexF qq cc) = (q == qq) && p c cc
	liftEq p (SideF w f s) (SideF ww ff ss) = w == ww && p f ff && p s ss
	liftEq p (VertexCompF w f s t) (VertexCompF ww ff ss tt) = w == ww && p f ff && p s ss && p t tt
	liftEq p (QuadrantF f s t q) (QuadrantF ff ss tt qq) = p f ff && p s ss && p t tt && p q qq
	liftEq _ _ _ = False

instance Show1 PartF where
	liftShowsPrec _ _ _ LeafF = ("LeafF" <>)
	liftShowsPrec prec _ _ (VertexF q f) = (\b -> "VertexF " <> show q <> " " <> b) . prec 10 f
	liftShowsPrec prec _ _ (SideF w ff ss) = (\b -> "SideF " <> show w <> " " <> b) . prec 10 ff . prec 10 ss
	liftShowsPrec prec _ _ (VertexCompF q f s t) = (\b -> "VertexCompF " <> show q <> " " <> b) . prec 10 f . prec 10 s . prec 10 t
	liftShowsPrec prec _ _ (QuadrantF f s t q) = (\b -> "QuadrantF" <> " " <> b) . prec 10 f . prec 10 s . prec 10 t . prec 10 q

-- A 2d non uniform grid
newtype Grid a = Grid { unGrid :: Cofree PartF a } deriving (Eq, Show)

leaf :: a -> Grid a
leaf = Grid . (:< LeafF)

vertex :: a -> Quadrant -> Grid a -> Grid a
vertex a q c = Grid $ a :< VertexF q (unGrid c)

side :: a -> Wind -> Grid a -> Grid a -> Grid a
side a w f s = Grid $ a :< SideF w (unGrid f) (unGrid s)

vertexComp :: a -> Quadrant -> Grid a -> Grid a -> Grid a -> Grid a
vertexComp a q f s t = Grid $ a :< VertexCompF q (unGrid f) (unGrid s) (unGrid t)

quadrant :: a -> Grid a -> Grid a -> Grid a -> Grid a -> Grid a
quadrant a f s t q = Grid $ a :< QuadrantF (unGrid f) (unGrid s) (unGrid t) (unGrid t)

instance Semigroup a => Semigroup (Grid a) where
	(Grid (a :< LeafF)) <> (Grid (aa :< c)) = Grid $ (a <> aa) :< c
	a@(Grid (_ :< _)) <> aa@(Grid (_ :< LeafF)) = aa <> a

	(Grid (a :< (VertexF q c))) <> (Grid (aa :< (VertexF qq cc))) =
		Grid $ (a <> aa) :< if q == qq
			then (VertexF q (unGrid $ (Grid c) <> (Grid cc)))
			else (case (q,qq) of
				(Q1, Q2) -> SideF N c cc
				(Q1, Q3) -> SideF SWNE c cc
				(Q1, Q4) -> SideF E cc c

				(Q2, Q1) -> SideF N cc c
				(Q2, Q3) -> SideF W c cc
				(Q2, Q4) -> SideF NWSE c cc

				(Q3, Q1) -> SideF SWNE cc c
				(Q3, Q2) -> SideF W cc c
				(Q3, Q4) -> SideF S c cc

				(Q4, Q1) -> SideF E c cc
				(Q4, Q2) -> SideF NWSE cc c
				(Q4, Q3) -> SideF S cc c
				)

	(Grid (a :< (VertexF q c))) <> (Grid (aa :< (SideF w ff ss))) =
		Grid $ (a <> aa) :< case (q,w) of
			(Q1, N) -> SideF N (unGrid $ (Grid c) <> (Grid ff)) ss
			(Q2, N) -> SideF N ff (unGrid $ (Grid c) <> (Grid ss))
			(Q3, N) -> VertexCompF Q4 ff ss c
			(Q4, N) -> VertexCompF Q3 c ff ss

			(Q1, W) -> VertexCompF Q4 c ff ss
			(Q2, W) -> SideF W (unGrid $ (Grid c) <> (Grid ff)) ss
			(Q3, W) -> SideF W ff (unGrid $ (Grid c) <> (Grid ss))
			(Q4, W) -> VertexCompF Q1 ff ss c

			(Q1, S) -> VertexCompF Q2 ff ss c
			(Q2, S) -> VertexCompF Q1 c ff ss
			(Q3, S) -> SideF S (unGrid $ (Grid c) <> (Grid ff)) ss
			(Q4, S) -> SideF S ff (unGrid $ (Grid c) <> (Grid ss))

			(Q1, E) -> SideF E ff (unGrid $ (Grid c) <> (Grid ss))
			(Q2, E) -> VertexCompF Q3 ff ss c
			(Q3, E) -> VertexCompF Q4 c ff ss
			(Q4, E) -> SideF E (unGrid $ (Grid c) <> (Grid ff)) ss

			(Q1, NWSE) -> VertexCompF Q3 ss c ff
			(Q2, NWSE) -> SideF NWSE (unGrid $ (Grid c) <> (Grid ff)) ss
			(Q3, NWSE) -> VertexCompF Q1 ff c ss
			(Q4, NWSE) -> SideF NWSE ff (unGrid $ (Grid c) <> (Grid ss))

	a@(Grid (_ :< (SideF _ _ _))) <> aa@(Grid (_ :< (VertexF _ _))) =
		aa <> a
	
	(Grid (a :< (VertexF q c))) <> (Grid (aa :< (VertexCompF qq ff ss tt))) =
		Grid $ (a <> aa) :< case (q, qq) of
			(Q1, Q1) -> QuadrantF c ff ss tt
			(Q1, Q2) -> VertexCompF Q1 ff ss (unGrid $ (Grid c) <> (Grid tt))
			(Q1, Q3) -> VertexCompF Q2 ff (unGrid $ (Grid c) <> (Grid ss)) tt
			(Q1, Q4) -> VertexCompF Q2 (unGrid $ (Grid c) <> (Grid ff)) ss  tt

			(Q2, Q2) -> QuadrantF tt c ff ss
			(Q2, Q3) -> VertexCompF Q1 ff ss (unGrid $ (Grid c) <> (Grid tt))
			(Q2, Q4) -> VertexCompF Q2 ff (unGrid $ (Grid c) <> (Grid ss)) tt
			(Q2, Q1) -> VertexCompF Q2 (unGrid $ (Grid c) <> (Grid ff)) ss  tt

-- type Pos = L.V2 Int
-- 
-- data Size = Size
-- 	{ _width :: Int
-- 	, _height :: Int } deriving Show
-- 
-- makeLenses ''Size
-- 
-- {------------------------------------------------------------------------------}
-- 
-- -- | Position of a corner. The opposite corner is at
-- -- | Pos.x + _width, Pos.y + _height
-- data Rect = Rect
-- 	{ _rectPos :: Pos
-- 	, _rectSize :: Size } deriving Show
-- 
-- class HasRect a where
-- 	rect :: a -> Rect
-- 	-- a `contains` the position Pos
-- 	contains :: a -> Pos -> Bool
-- 	contains a (L.V2 x y) =
-- 		and [x >= xx, x <= xx+w-1, y >= yy, y <= yy+h-1]
-- 		where
-- 		(Rect (L.V2 xx yy) (Size w h)) = rect a
-- 
-- 	-- a crosses the cross formed by the position Pos
-- 	crosses :: a -> Pos -> Bool
-- 	crosses a (L.V2 x y) =
-- 		(xx <= x && xx+w-1 >= x) || (yy <= y && yy+h-1 >= y)
-- 		where
-- 		(Rect (L.V2 xx yy) (Size w h)) = rect a
-- 
-- 	-- If two figures overlap
-- 	overlap :: HasRect b => a -> b -> Bool
-- 	overlap a b
-- 		| (x > xx+ww || xx > x+w) = False
-- 		| (y > yy+hh || yy > y+h) = False
-- 		| otherwise = True
-- 		where
-- 		(Rect (L.V2 x y) (Size w h)) = rect a
-- 		(Rect (L.V2 xx yy) (Size ww hh)) = rect b
-- 	
-- 	pos :: a -> Pos
-- 	pos = _rectPos . rect
-- 
-- instance HasRect Rect where
-- 	rect :: Rect -> Rect
-- 	rect = id
-- 
-- 	overlap :: HasRect b => Rect -> b -> Bool
-- 	overlap (Rect (L.V2 x y) (Size w h)) b
-- 			| (x > xx+ww || xx > x+w) = False
-- 			| (y > yy+hh || yy > y+h) = False
-- 			| otherwise = True
-- 		where
-- 		(Rect (L.V2 xx yy) (Size ww hh)) = rect b
-- 
-- {------------------------------------------------------------------------------}
-- -- Relative position of two points in 2-d
-- data RelPos = NE | NW | SW | SE | CO deriving Show
-- 
-- relPos :: Pos -> Pos -> RelPos
-- relPos (L.V2 x y) (L.V2 xx yy)
-- 	| (x >= xx) && (y > yy) = NE
-- 	| (x < xx) && (y >= yy) = NW
-- 	| (x <= xx) && (y < yy) = SW
-- 	| (x > xx) && (y <= yy) = SE
-- 	| otherwise = CO
-- {------------------------------------------------------------------------------}
-- 
-- data QuadTree a
-- 	= Empty
-- 	| Leaf Pos [a]
-- 	| Branch Pos [a] (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
-- 	deriving Show
-- 
-- instance Functor QuadTree where
-- 	fmap _ Empty = Empty
-- 	fmap f (Leaf p as) = Leaf p $ fmap f as
-- 	fmap f (Branch p as ne nw sw se) = Branch p (fmap f as)
-- 		(fmap f ne) (fmap f nw) (fmap f sw) (fmap f se)
-- 
-- qtToList :: QuadTree a -> [a]
-- qtToList Empty = []
-- qtToList (Leaf _ as) = as
-- qtToList (Branch _ as ne nw sw se) = as ++ concatMap (qtToList) [ne, nw, sw, se]
-- 
-- insert :: HasRect a => a -> QuadTree a -> QuadTree a
-- insert a Empty = Leaf (pos a) [a]
-- insert a (Leaf c as) =
-- 	if a `crosses` c
-- 		then Leaf c (a:as)
-- 		else case relPos (pos a) c of
-- 			NE -> Branch c as (Leaf (pos a) [a]) Empty Empty Empty
-- 			NW -> Branch c as Empty (Leaf (pos a) [a]) Empty Empty
-- 			SW -> Branch c as Empty Empty (Leaf (pos a) [a]) Empty
-- 			SE -> Branch c as Empty Empty Empty (Leaf (pos a) [a])
-- 			CO -> Leaf c (a:as)
-- insert a (Branch c as ne nw sw se) =
-- 	if a `crosses` c
-- 		then Branch c (a:as) ne nw sw se
-- 		else case relPos (pos a) c of
-- 			NE -> Branch c as (insert a ne) nw sw se
-- 			NW -> Branch c as ne (insert a nw) sw se
-- 			SW -> Branch c as ne nw (insert a sw) se
-- 			SE -> Branch c as ne nw sw (insert a se)
-- 			CO -> Branch c (a:as) ne nw se se
-- 
-- -- Return all the elements in the quadtree that inserct with the point
-- -- Should be more general, I'd like to count for example
-- pinch :: HasRect a => Pos -> QuadTree a -> [a]
-- pinch = pinchHelp []
-- 	where
-- 	pinchHelp :: HasRect a => [a] -> Pos -> QuadTree a -> [a]
-- 	pinchHelp acc _ Empty = acc
-- 	pinchHelp acc p (Leaf c as) = acc ++ filter (`contains` p) as
-- 	pinchHelp acc p (Branch c as ne nw sw se) = let
-- 		nacc = filter (`contains` p) as
-- 		in (case relPos p c of
-- 			NE -> (pinchHelp (acc++nacc) p ne)
-- 			NW -> (pinchHelp (acc++nacc) p nw)
-- 			SW -> (pinchHelp (acc++nacc) p sw)
-- 			SE -> (pinchHelp (acc++nacc) p se)
-- 			CO -> nacc)
-- 
-- collide :: (HasRect a, HasRect b) => a -> QuadTree b -> [b]
-- collide = collideH []
-- 	where
-- 	collideH acc _ (Empty) = acc
-- 	collideH acc r (Leaf c as) = acc ++ filter (`overlap` r) as
-- 	collideH acc r (Branch c as ne nw sw se) =
-- 		let
-- 			(Rect _ (Size w h)) = rect r
-- 			nacc = acc ++ filter (`overlap` r) as
-- 		in
-- 		(case (relPos (pos r) c, relPos (pos r + (L.V2 w h)) c) of
-- 			(NE, NE) -> (collideH nacc r ne)
-- 			(NW, NW) -> (collideH nacc r nw)
-- 			(SW, SW) -> (collideH nacc r sw)
-- 			(SE, SE) -> (collideH nacc r se)
-- 	
-- 			(NE, CO) -> (collideH nacc r ne)
-- 			(NW, CO) -> (collideH nacc r nw)
-- 			(SW, CO) -> (collideH nacc r sw)
-- 			(SE, CO) -> (collideH nacc r se)
-- 	
-- 			(CO, NE) -> (collideH nacc r ne)
-- 			(CO, NW) -> (collideH nacc r nw)
-- 			(CO, SW) -> (collideH nacc r sw)
-- 			(CO, SE) -> (collideH nacc r se)
-- 	
-- 			(NW, NE) -> collideH nacc r nw ++ collideH nacc r ne
-- 			(NE, NW) -> collideH nacc r nw ++ collideH nacc r ne --This will never happen
-- 	
-- 			(NW, SW) -> collideH nacc r nw ++ collideH nacc r sw
-- 			(SW, NW) -> collideH nacc r nw ++ collideH nacc r sw --This will never happen
-- 	
-- 			(SW, SE) -> collideH nacc r sw ++ collideH nacc r se
-- 			(SE, SW) -> collideH nacc r sw ++ collideH nacc r se --This will never happen
-- 	
-- 			(NE, SE) -> collideH nacc r ne ++ collideH nacc r se
-- 			(SE, NE) -> collideH nacc r ne ++ collideH nacc r se --This will never happen
-- 			_ -> collideH nacc r ne ++ collideH nacc r se ++ collideH nacc r nw ++ collideH nacc r sw)
