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
solve_1= do
	SysIO.withFile "inputs/day01" SysIO.ReadMode $ \input_fh ->
		SysIO.hGetContents input_fh >>= \contents ->
		(pure $ (DL.foldl' (+) 0) $ fmap parseInteger $ lines contents)
		>>= print

solve_2 :: IO ()
solve_2 = do
	SysIO.withFile "inputs/day01" SysIO.ReadMode $ \input_fh ->
		SysIO.hGetContents input_fh >>= \contents ->
		print $ sums $ fmap parseInteger $ lines contents

-- The problem input as an IO effect
input :: IO [Integer]
input = do
	input_fh <- SysIO.openFile "inputs/day01" SysIO.ReadMode
	contents <- SysIO.hGetContents input_fh
	pure $ fmap parseInteger $ lines $ contents

-- Original version
--sums :: [Integer] -> Maybe Integer
--sums = go Set.empty . partialSums . cycle
--	where
--	go :: Set Integer -> [Integer] -> Maybe Integer
--	go _ [] = Nothing
--	go set (n:ns)
--		| n `Set.member` set = Just n
--		| otherwise = go (n `Set.insert` set) ns

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
		-- There is a repetition in the infine list of partial sums or there
		-- isn't.
		(First $ (((ps !!) . fst . fst) <$> ) $
			safeHead $
			DL.sortBy comp $
			filter ((>0) . snd) $
			fmap (second (negate . (`div` total_sum))) $
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
-- Requieres an Ord constrain
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

-- Calculate the partial sums of a list of Integers starting with 0
partialSums :: [Integer] -> [Integer]
partialSums = scanl (+) 0

-- test1, test2, test3, test4 :: [Integer]
-- test1 = [-1, 1]
-- test2 = [3,3,4,-2,-4]
-- test3 = [-6,3,8,5,-6]
-- test4 = [7,7,-2,-7,-4]

{-
 - [1,-2,3] sum = 2, sums = [0,1,-1,2]
 - 1+n1*s = -1+n2*s (equal if n2=0 and n2=1)
 - (n1-n2)*s = -1 - 1
 - (n1-n2)*s = -2
 - -1 * 2 = -2 - 1
 - `s` is a factor of the difference of the two first round sums, there are O(N^2)
 - such differences. N*(N-1)
 -
 - 0+n0*s = 1+n1*s
 -
 - Some Ideas
 -
 - let sums = [0, ps_1, ps_2, ..., ps_N] be the partial sums of the list
 - There is either:
 -
 - * a repeated numner in sums
 - * ps_i+n_1*ps_N = ps_j+n_2*ps_N
 -
 - (n1_1 - n_2)*ps_N = ps_j - ps_i
 -
 - This tell us that (ps_j - ps_i) % ps_N == 0
 -
 -
-}
