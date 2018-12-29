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
sums = go Set.empty . partialSums . cycle
	where
	go :: Set Integer -> [Integer] -> Maybe Integer
	go _ [] = Nothing
	go set (n:ns)
		| n `Set.member` set = Just n
		| otherwise = go (n `Set.insert` set) ns

-- The right function is scanl
partialSums :: [Integer] -> [Integer]
partialSums = scanl (+) 0

l :: [Integer]
l = [7,7,-2,-7,-4]
