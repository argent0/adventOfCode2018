module Day01
	( solve_1
	, solve_2
) where

import qualified System.IO as SysIO
import qualified Data.List as DL
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

import Data.Functor.Foldable
import Control.Comonad.Cofree

parseInteger :: String -> Integer
parseInteger [] = error "parseInteger: Empty string"
parseInteger ('+':s) = read s
parseInteger ('-':s) = negate $ read s

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

sums :: [Integer] -> Maybe Integer
sums = go 0 (Set.singleton 0) . cycle
	where
	go :: Integer -> Set Integer -> [Integer] -> Maybe Integer
	go p set [] = Nothing
	go p set (n:ns) = let next = n + p
		in if next `Set.member` set
			then (Just next)
			else go next (next `Set.insert` set) ns

-- What I want
-- * Lazy
-- * Non Custom recursion

-- Is lazy
-- Custom recursion
partialSums1 :: [Integer] -> [Integer]
partialSums1 = (0:) . go 0
	where
	go :: Integer -> [Integer] -> [Integer]
	go acc [] = []
	go acc (n:ns) = (acc+n):go (acc+n) ns

-- The right function is scanl
partialSums :: [Integer] -> [Integer]
partialSums = scanl (+) 0

l :: [Integer]
l = [7,7,-2,-7,-4]
