{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day02
	( solve_1
	, solve_2
) where

import qualified System.IO as SysIO
import qualified Data.List as DL
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Data.Foldable as DF
import Debug.Trace
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- Count the number of repetitions each element has
-- O(n*log n)
count :: Ord a => [a] -> Map a Integer
count = Map.fromListWith (+) . (flip zip) (repeat 1)

-- O(N^2) using only an `Eq` constraint
-- Alternative for unfolder:
-- 	unfolder (a:as) =
-- 		let (al,nas) = DL.partition (==a) as in
-- 			Just ((a, 1 + DL.genericLength al), nas)
count' :: Eq a => [a] -> [(a, Integer)]
count' = DL.unfoldr unfolder
	where
	unfolder [] = Nothing
	unfolder (a:as) = Just $ DL.foldl' folder ((a, 1), []) as
	-- Computes the length and partitions in one pass
	folder ((a, n), nas) e
		| e == a = ((a, n + 1), nas)
		| otherwise = ((a, n), e:nas)

data Elem2 = Elem2None | Elem2OnlyFirst | Elem2OnlySecond | Elem2Both deriving (Show, Eq)

instance Semigroup Elem2 where
	Elem2Both <> _ = Elem2Both
	_ <> Elem2Both = Elem2Both
	Elem2None <> b = b
	a <> Elem2None = a
	Elem2OnlyFirst <> Elem2OnlyFirst = Elem2OnlyFirst
	Elem2OnlySecond <> Elem2OnlySecond = Elem2OnlySecond
	Elem2OnlyFirst <> Elem2OnlySecond = Elem2Both
	Elem2OnlySecond <> Elem2OnlyFirst = Elem2Both

instance Monoid Elem2 where
	mempty = Elem2None

elem2 :: (Show a, Foldable t, Eq a) => a -> a -> t a -> Elem2
elem2 a b = DF.foldMap (folder' a b)
	where
	folder' a b e
		| e == a = Elem2OnlyFirst
		| e == b = Elem2OnlySecond
		| otherwise = Elem2None

-- Lazy version
elem2' :: Eq a => a -> a -> [a] -> Elem2
elem2' a b = go Elem2None
	where
	go acc [] = acc
	go Elem2Both _ = Elem2Both  --Early stop
	go acc (e:es)
		| e == a = go (acc <> Elem2OnlyFirst) es
		| e == b = go (acc <> Elem2OnlySecond) es
		| otherwise = go (acc <> Elem2None) es

data FindTwo = StillLooking FindTwo | FoundFirst Bool | FoundSecond Bool | FoundNone deriving Show

makeBaseFunctor ''FindTwo

elem2'' :: forall a. Eq a => a -> a -> [a] -> FindTwo
elem2'' a b = ana coAlg
	where
	coAlg :: [a] -> FindTwoF [a]
	coAlg [] = FoundNoneF
	coAlg (e:es)
		| e == a = FoundFirstF $ b `elem` es
		| e == b = FoundSecondF $ a `elem` es
		| otherwise = StillLookingF es

data Find2 = Find2None | Find2First Bool | Find2Second Bool deriving Show

elem2'v :: forall a. Eq a => a -> a -> [a] -> [Find2]
elem2'v a b = DL.unfoldr coAlg
	where
	coAlg :: [a] -> Maybe (Find2, [a])
	coAlg [] = Nothing
	coAlg (e:es)
		| e == a = Just (Find2First $ b `elem` es, [])
		| e == b = Just (Find2Second $ a `elem` es, [])
		| otherwise = Just (Find2None, es)

-- O(N*log n)
-- where N is the length of the input list
-- where n is the length of the strings inside the input list
checksum :: [String] -> Integer
checksum = uncurry (*) . DL.foldl' folder (0,0) . fmap count
	where
	folder :: (Integer, Integer) -> Map Char Integer -> (Integer, Integer)
	folder (p,t) counts = case elem2' 2 3 (Map.elems counts) of
		Elem2None -> (p, t)
		Elem2OnlyFirst -> (p + 1, t)
		Elem2OnlySecond -> (p, t + 1)
		Elem2Both -> (p + 1, t + 1)

solve_1 :: IO ()
solve_1= do
	SysIO.withFile "inputs/day02" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines ->
		print $ checksum file_lines

data RoseTree a = RoseTree a [Maybe (RoseTree a)] deriving (Show, Eq, Ord)

data ReducedRoseTree a =
	ReducedRoseTree [a] (RoseTree a) deriving (Show, Eq, Ord)

build :: (Show a, Eq a, Ord a) => [[a]] -> [RoseTree a]
build l = --trace ("\n>>rec>>" ++ show l) $
	(concatMap mapper . DL.groupBy ((==) `on` head)) l
	where
	-- all these have the same prefix
	mapper :: (Show a, Eq a, Ord a) =>
		[[a]] -> [RoseTree a]
	mapper (a:[]) = pure $ prefixer a []
	mapper ((a:as):ass) = pure . RoseTree a $
		(fmap (const Nothing) $ filter null $ as:(fmap tail ass))
		 ++ (fmap pure $ build $ filter (not . null) $ as:(fmap tail ass))

diffOneLetter :: String -> String -> Bool
diffOneLetter [] [] = False
diffOneLetter (a:as) (b:bs)
	| a /= b = as == bs
    | a == b = diffOneLetter as bs

part :: [String] -> [(String, [String])]
part as = go [] as
	where
	go :: [(String, [String])] -> [String] -> [(String, [String])]
	go acc [] = acc
	go acc (b:bs) =
		let (g,ng) = DL.partition (diffOneLetter b) as in
			go ((b,g):acc) bs

sing :: [a] -> Bool
sing [a] = True
sing _ = False

prefixer :: [a] -> [Maybe (RoseTree a)] -> RoseTree a
prefixer [a] = RoseTree a
prefixer (a:as) =  RoseTree a . pure . pure . prefixer as

solve_2 :: IO ()
solve_2= do
	SysIO.withFile "inputs/day02" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines ->
		print $
			filter (not . null. snd) $ part $ file_lines

-- test_input :: [String]
-- test_input =
-- 	["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]

test_input :: [String]
test_input = [ "abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "vxyz"]

{-
[RoseTree 'a'
	[Just (RoseTree 'a'
		[Just (RoseTree 'b'
			[Just (RoseTree 'c'
				[Just (RoseTree 'd'
					[Just (RoseTree 'd' [])])])])])
	,Just (RoseTree 'b'
		[Just (RoseTree 'a'
			[Just (RoseTree 'b'
				[Just (RoseTree 'a'
					[Just (RoseTree 'b' [])])])])
		,Just (RoseTree 'b'
			[Just (RoseTree 'c'
				[Just (RoseTree 'd'
					[Just (RoseTree 'e' [])])])])
		,Just (RoseTree 'c'
			[Just (RoseTree 'c'
				[Just (RoseTree 'c'
					[Just (RoseTree ' d' [])])])
			,Just (RoseTree 'd'
				[Just (RoseTree 'e'
					[Just (RoseTree 'e' [])
					,Just (RoseTree 'f' [])])])])])]
,RoseTree 'b'
	[Just (RoseTree 'a'
		[Just (RoseTree 'b'
			[ Just (RoseTree 'a'
				[Just (RoseTree 'b' [Just (RoseTree 'c' [])])])])])]
]
-}

{-
"zihwtxagwifpbsnwleydukjmqv"
"zihwtxagsifpbsnwleydukjmqv"

"zihwtxagifpbsnwleydukjmqv"
"zihwtxagifpbsnwleydukjmqv"
-}
