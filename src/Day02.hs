{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Function (on)
import qualified Data.Foldable as DF
import Data.Functor.Foldable (ana)

import Control.Monad.Free (retract)
import Data.Functor.Identity (runIdentity, Identity(..))
import qualified Control.Monad.Trans.Free as CMTF
import Control.Arrow ((***))
import Data.Bool (bool)

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

-- data Rec a = Rec (Rec a) | Done a deriving Show
--
-- 00:00 < argent0> hi, is this isomorphic to something more popular? data Rec a
-- = Rec (Rec a) | Done a deriving Show
-- 00:01 < Solonarv> It's isomorphic to Free Identity
-- 00:02 < argent0> Solonarv: thanks I'll check that out
-- 00:02 < Solonarv> argent0: Free is in free:Control.Monad.Free, Identity is in
-- base:Data.Functor.Identity
-- 00:03 < c_wraith> it's also Fix Maybe
-- 00:04 < c_wraith> err, no. it's Fix (Either a)
-- 00:04 < c_wraith> or if you want to be particularly smart-ass about it, (Nat,
-- a)
-- 00:05 < Solonarv> more generally, Free f a ~ Fix (Either a `Compose` f)
-- 00:05 < c_wraith> hmm. good point.
-- 00:06 < Solonarv> ( similarly, Cofree f a ~ Fix ((,) a `Compose` f) )
-- 00:07 < c_wraith> do backticks work in types? I can't say as I've ever tried.
-- 00:07 < Solonarv> They do, you might need -XTypeOperators though – I don't
-- remember
-- 00:08 < Solonarv> % :k Int `Either` Char
-- 00:08 < yahb> Solonarv: Int `Either` Char :: *
-- 00:09 < puffnfresh> Free Identity is also known as the "Partiality Monad"
-- 00:09 < puffnfresh> argent0: you're recreating Partiality exactly :)
-- 00:09 < Solonarv> which one is that?
-- 00:10 < argent0> c_wraith: puffnfresh: I'll check those too
-- 00:10 < c_wraith> I have a result! | I need to think more.
-- 00:11 < c_wraith> it's a very direct way to encode partiality into a total
-- language that supports codata
-- 00:11 < Solonarv> Oh, I see!
-- 00:12 < c_wraith> sometimes you even want it in Haskell. it's a pure way to
-- produce progress reports from a computation, for instance.
-- 00:14 < c_wraith> so long as you don't mind the computation only running when
-- you ask for the next progress report.
-- 00:14 < Solonarv> Seems like it'd be quite useful as a monad transformer
-- 00:14 < c_wraith> (deepseq it with par to get it to run in the background!)

-- | Predicate that checks if two elements are in t.
--
-- retract :: Free f a -> f a
-- data Free f a = Pure a | Free (f (Free f a))
-- data Rec a = Done a | Rec (Rec a)
--
-- Rec is isomorphic to Free Identity
--
-- >>> elem2 1 2 [1,2,undefined]
-- (True,True)
-- >>> elem2 1 2 [2,3]
-- (False,True)
-- >>> elem2 1 2 [1,3]
-- (True,False)
elem2 :: forall a t. (Eq a, Foldable t) => a -> a -> t a -> (Bool, Bool)
elem2 a b = runIdentity . retract . ana coAlg . DF.toList
	where
	coAlg [] = CMTF.Pure (False, False)
	coAlg (e:es)
		| e == a = CMTF.Pure (True, b `elem` es)
		| e == b = CMTF.Pure (a `elem` es, True)
		| otherwise = CMTF.Free (Identity es)

-- O(N*log n)
-- where N is the length of the input list
-- where n is the length of the strings inside the input list
checksum :: [String] -> Integer
checksum = uncurry (*) . DL.foldl' folder (0,0) . fmap count
	where
	folder :: (Integer, Integer) -> Map Char Integer -> (Integer, Integer)
	folder (p, t) counts = (+p) *** (+t) $ (bool 0 1) *** (bool 0 1) $ elem2 2 3 counts
	--(False, False) -> (p, t)
	--(True, False) -> (p + 1, t)
	--(False, True) -> (p, t + 1)
	--(True, True) -> (p + 1, t + 1)

solve_1 :: IO ()
solve_1= do
	SysIO.withFile "inputs/day02" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines ->
		print $ checksum file_lines

-- Part 2

-- | True if argument strings differ by one letter
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

-- | Solve part 2 by comparing all strings
solve_2bf :: IO ()
solve_2bf = do
	SysIO.withFile "inputs/day02" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines ->
		print $
			filter (not . null. snd) $ part $ file_lines

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
