{-# LANGUAGE ScopedTypeVariables #-}
module Util.Recursion
	( quadTime
	, semiQuadTime
	, count
	, count' )
	where

import qualified Data.List as DL
import Control.Monad (join)
import Control.Arrow ((&&&))
import qualified Data.Map as Map
import Data.Map (Map)

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

-- | All vs all application of f
quadTime :: forall a b c . (a -> b -> c) -> [a] -> [b] -> [c]
quadTime f as bs = f <$> as <*> bs

-- | If f is commutative in its arguments, half of the pairs can be ignored.
--
-- >>> semiQuadTime (,) "abc"
-- [('a','a'),('a','b'),('a','c'),('b','b'),('b','c'),('c','c')]
semiQuadTime :: forall a b . (a -> a -> b) -> [a] -> [b]
semiQuadTime f = join . (uncurry (zipWith (fmap . f))) . (id &&& DL.tails)
