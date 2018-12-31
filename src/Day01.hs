{-
The key insight would be that

Let `sums = [0, ps_1, ps_2, ..., ps_N]` be the partial sums of the input list.
`ps_N` is the total sum. There is either:

1. a repeated number in `sums`
2. `ps_N == 0`. In which case 0 is the repeated value.
3. There exists `i,j,n_i,n_j` such that `ps_i + n_i*ps_N = ps_j + n_j*ps_N`.
4. None of the above and there is no repeating value

If `3` is true then:

`(n_i - n_j)*ps_N = ps_j - ps_i`. (1)

This tell us that:

`(ps_j - ps_i) % ps_N == 0`. (2)

From all the pairs `(ps_j, ps_i)` we only need to consider those that satisfy
(2). For those:

`(n_i - n_j) = (ps_j - ps_i) div ps_N`. (3)

As it can be seen there is only information to compute `(n_i - n_j)`. This means
that the repeated value is one element in `sums`. So we take `n_i = 0`.

`n_j = -(ps_j - ps_i) div ps_N`. (4)

Of all those `n_j` we want the smallest and the one with least `j`. Then the
answer is `sums !! i`.
-}

module Day01
	( solve_1
	, solve_2
	, input
) where

import qualified System.IO as SysIO

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow
import qualified Data.List as DL
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as ST
import Data.Monoid
import Data.Bool (bool)

parseInteger :: String -> Integer
parseInteger [] = error "parseInteger: Empty string"
parseInteger ('+':s) = read s
parseInteger ('-':s) = negate $ read s
parseInteger s = error $ "Can't parse" ++ s

solve_1 :: IO ()
solve_1= SysIO.withFile "inputs/day01" SysIO.ReadMode $ \input_fh ->
	SysIO.hGetContents input_fh >>= \contents ->
	(pure $ (DL.foldl' (+) 0) $ fmap parseInteger $ lines contents)
	>>= print

solve_2 :: IO ()
solve_2 = SysIO.withFile "inputs/day01" SysIO.ReadMode $ \input_fh ->
	SysIO.hGetContents input_fh >>= \contents ->
	print $ sums $ fmap parseInteger $ lines contents

-- The problem input as an IO effect
input :: IO [Integer]
input = do
	input_fh <- SysIO.openFile "inputs/day01" SysIO.ReadMode
	contents <- SysIO.hGetContents input_fh
	pure $ fmap parseInteger $ lines $ contents


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

-- O(N^2)
sums :: [Integer] -> Maybe Integer
sums [] = Nothing
sums l = getFirst $
		-- There is a duplicated value in the list of partial sums
		(First $ firstDuplicate ps) <>
		--The total sum is 0 making this also the first repeated value
		(First $ if total_sum == 0 then Just 0 else Nothing) <>
		-- There is a repetition in the infinite list of partial sums or there
		-- isn't.
		(First $ (((ps !!) . fst . fst) <$> ) $
			safeHead $
			DL.sortBy comp $
			filter ((>0) . snd) $ --Keep the ones that lay forward
			fmap (second (negate . (`div` total_sum))) $ --Compute the `n_j`
			fmap (\((i,psi),(j,psj)) -> ((i,j), psj - psi)) $ candidate_pairs
		)
	where
	-- Only the pairs with difference divisible by `total_sum` are candidates
	candidate_pairs = filter (\((_,psi),(_,psj)) -> (psj - psi) `mod` total_sum == 0) $
		filter (\((i,_),(j,_)) -> i /= j) $
		(,) <$> (init eps) <*> (init eps)
	eps = zip [0..] ps
	ps = partialSums l
	total_sum = last ps
	-- Compare favoring the least n and then the min index
	comp ((i,j),n) (((ii,jj),nn)) = case compare n nn of
		EQ -> compare (max i j) (max ii jj)
		c -> c

-- Find the first duplicate in a list
-- It works for infinite lists with duplicates. E.g. 10:[1..]
-- It works for finite lists without duplicates
-- Requires an Ord constrain
-- O(N*logN)

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate l = ST.evalState ((getFirst . foldMap First) <$> traverse tr l) Set.empty
	where
	tr :: Ord a => a -> State (Set a) (Maybe a)
	tr n = ((n `Set.member`) <$> ST.get) >>= bool
			(ST.modify (n `Set.insert`) >> pure Nothing)
			(pure $ Just n)

-- The same but only an Eq constrain O(N^2)
-- firstDuplicate' :: Eq a => [a] -> Maybe a
-- firstDuplicate' l = ST.evalState ((getFirst . foldMap First) <$> traverse tr l) []
-- 	where
-- 	tr :: Eq a => a -> State [a] (Maybe a)
-- 	tr n = ((n `elem`) <$> ST.get) >>= bool
-- 			(ST.modify (n:) >> pure Nothing)
-- 			(pure $ Just n)

-- Ad hoc version
-- firstDuplicate :: [Integer] -> Maybe Integer
-- firstDuplicate = go Set.empty
-- 	where
-- 	go _ [] = Nothing
-- 	go set (n:ns)
-- 		| n `Set.member` set = Just n
-- 		| otherwise = go (n `Set.insert` set) ns

-- Original version
--sums :: [Integer] -> Maybe Integer
--sums = go Set.empty . partialSums . cycle
--	where
--	go :: Set Integer -> [Integer] -> Maybe Integer
--	go _ [] = Nothing
--	go set (n:ns)
--		| n `Set.member` set = Just n
--		| otherwise = go (n `Set.insert` set) ns

-- Calculate the partial sums of a list of Integers starting with 0
partialSums :: [Integer] -> [Integer]
partialSums = scanl (+) 0

-- test1, test2, test3, test4 :: [Integer]
-- test1 = [-1, 1]
-- test2 = [3,3,4,-2,-4]
-- test3 = [-6,3,8,5,-6]
-- test4 = [7,7,-2,-7,-4]
