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
--import Control.Arrow

-- O(n*log n)
count :: Ord a => [a] -> Map a Integer
count = Map.fromListWith (+) . (flip zip) (repeat 1)

-- O(N*log n)
-- where N is the length of the input list
-- where n is the length of the strings inside the input list
checksum :: [String] -> Integer
checksum = uncurry (*) . DL.foldl' folder (0,0) . fmap count
	where
	folder :: (Integer, Integer) -> Map Char Integer -> (Integer, Integer)
	folder (p,t) counts =
		(if 2 `elem` counts then p + 1 else p,
		if 3 `elem` counts then t + 1 else t)

solve_1 :: IO ()
solve_1= do
	SysIO.withFile "inputs/day02" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines ->
		print $ checksum file_lines

solve_2 :: IO ()
solve_2 = undefined

test_input :: [String]
test_input =
	["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
