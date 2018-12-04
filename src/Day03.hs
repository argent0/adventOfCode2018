{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Strict #-}
module Day03 (
	solve_1
) where

import Linear (V2)
import qualified Linear as L
import Control.Lens hiding (contains, Empty)
import qualified System.IO as SysIO
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import qualified Data.List as DL
import Debug.Trace

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
	overlap :: s -> s -> Bool

instance IsFig Rect where
	contains :: Rect -> Pos -> Bool
	contains (Rect (L.V2 xx yy) (Size w h)) (L.V2 x y) =
		and [x >= xx, x <= xx+w-1, y >= yy, y <= yy+h-1]
	crosses :: Rect -> Pos -> Bool
	crosses (Rect (L.V2 xx yy) (Size w h)) (L.V2 x y) =
		(xx <= x && xx+w-1 >= x) || (yy <= y && yy+h-1 >= y)
	pos :: Rect -> Pos
	pos (Rect p _) = p
	overlap :: Rect -> Rect -> Bool
	overlap
		(Rect (L.V2 x y) (Size w h)) 
		(Rect (L.V2 xx yy) (Size ww hh))
			| (x > xx+ww || xx > x+w) = False
			| (y > yy+hh || yy > y+h) = False
			| otherwise = True

data Claim = Claim Id Rect deriving Show

instance IsFig Claim where
	contains :: Claim -> Pos -> Bool
	contains (Claim _ r) p = contains r p
	crosses :: Claim -> Pos -> Bool
	crosses (Claim _ r) p = crosses r p
	pos :: Claim -> Pos
	pos (Claim _ r) = pos r
	overlap :: Claim -> Claim -> Bool
	overlap (Claim _ r) (Claim _ rr) = r `overlap` rr

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

instance Functor QuadTree where
	fmap _ Empty = Empty
	fmap f (Leaf p as) = Leaf p $ fmap f as
	fmap f (Branch p as ne nw sw se) = Branch p (fmap f as)
		(fmap f ne) (fmap f nw) (fmap f sw) (fmap f se)

qtToList :: QuadTree a -> [a]
qtToList Empty = []
qtToList (Leaf _ as) = as
qtToList (Branch _ as ne nw sw se) = as ++ concatMap (qtToList) [ne, nw, sw, se]

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

-- I'd like to generalize collide and collideR
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

collideR :: Rect -> QuadTree Rect -> Integer
collideR r (Empty) = 0
collideR r (Leaf c as) =
	DL.foldl' (+) 0 $ (const 1) <$> filter (`overlap` r) as
collideR r@(Rect _ (Size w h)) (Branch c as ne nw sw se) =
	(DL.foldl' (+) 0 $ (const 1) <$> filter (`overlap` r) as) +
	(case (relPos (pos r) c, relPos (pos r + (L.V2 w h)) c) of
		(NE, NE) -> (collideR r ne)
		(NW, NW) -> (collideR r nw)
		(SW, SW) -> (collideR r sw)
		(SE, SE) -> (collideR r se)

		(NE, CO) -> (collideR r ne)
		(NW, CO) -> (collideR r nw)
		(SW, CO) -> (collideR r sw)
		(SE, CO) -> (collideR r se)

		(CO, NE) -> (collideR r ne)
		(CO, NW) -> (collideR r nw)
		(CO, SW) -> (collideR r sw)
		(CO, SE) -> (collideR r se)

		(NW, NE) -> collideR r nw + collideR r ne
		(NE, NW) -> collideR r nw + collideR r ne --This will never happen

		(NW, SW) -> collideR r nw + collideR r sw
		(SW, NW) -> collideR r nw + collideR r sw --This will never happen

		(SW, SE) -> collideR r sw + collideR r se
		(SE, SW) -> collideR r sw + collideR r se --This will never happen

		(NE, SE) -> collideR r ne + collideR r se
		(SE, NE) -> collideR r ne + collideR r se --This will never happen
		_ -> collideR r ne + collideR r se + collideR r nw + collideR r sw)


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

-- This works ok
solveWithQt :: QuadTree Claim -> Integer
solveWithQt quadTree =
	DL.foldl' (+) 0 $
	fmap (const 1) $
	filter (>1) $
	fmap (`collide` quadTree) $
	(L.V2 <$> [1..1000] <*> [1..1000])

-- ~1.4*10^9 operations (worst case) ~ 15 min at 1.5 GHz
solveWithClaims :: [Claim] -> Integer
solveWithClaims claims =
	DL.foldl' (+) 0 $
	fmap (const 1) $
	filter pred $
	(L.V2 <$> [1..1000] <*> [1..1000])
	where
	pred :: Pos -> Bool
	pred p = go 0 claims p
	go :: Integer -> [Claim] -> Pos -> Bool
	go acc [] _ = (acc > 1)
	go acc (c:cs) p
		| (c `contains` p) =
			if (acc > 0) then True else go (acc+1) cs p
		| otherwise = go (acc) cs p

-- collide all claims with the quadtree
solve2WithQt :: QuadTree Claim -> [Claim]
solve2WithQt quadTree =
	map fst $
	filter ((==1).snd) $
	fmap (\c@(Claim _ r) -> (c, r `collideR` qtr)) $
	qtToList quadTree
	where
	qtr :: QuadTree Rect
	qtr = fmap (\(Claim _ r) -> r) quadTree

solve_1 :: IO ()
solve_1= do
	SysIO.withFile "inputs/day03" SysIO.ReadMode $ \input_fh ->
		((T.lines . T.pack) <$> SysIO.hGetContents input_fh) >>= \file_lines ->
		print $
			-- mapM_ (\(L.V2 x y) -> putStrLn $ (show x) ++ " " ++ (show y)) $
			solve2WithQt $
			DL.foldl' (flip insert) Empty $ uncurry (++) $
			DL.partition (`contains` (L.V2 500 500)) $
			--solveWithClaims $
			--filter (`contains` (L.V2 1 159)) $
			--filter (`contains` (L.V2 1 160)) $
			fmap (\(AP.Done _ c) -> c) $
			fmap (\(AP.Partial p) -> p "") (fmap (AP.parse parseClaim) file_lines)

{- 90105 is too low -}
{- 118223 is right -}
{- 137163 is too high -}
