module Day01
	( solve_1
	, solve_2
) where

import qualified System.IO as SysIO
import qualified Data.List as DL
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

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
		(pure $ sums $ fmap parseInteger $ lines contents)
		>>= print

sums :: [Integer] -> Maybe Integer
sums = go 0 (Set.singleton 0) . cycle
	where
	go :: Integer -> Set Integer -> [Integer] -> Maybe Integer
	go p set [] = Nothing
	go p set (n:ns) = let next = n + p
		in if next `Set.member` set
			then (Just next)
			else go next (next `Set.insert` set) ns

l :: [Integer]
l = [7,7,-2,-7,-4]
