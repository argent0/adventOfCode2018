{-# Language ScopedTypeVariables #-}
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

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

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
