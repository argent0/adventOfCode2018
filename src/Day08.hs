{-# LANGUAGE OverloadedStrings #-}
module Day08 (
	solve_1
) where

import qualified System.IO as SysIO
import Data.Function (on)
import qualified Data.List as DL
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import Debug.Trace
import Data.Either (partitionEithers)
import Control.Monad (when, join, replicateM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (ord)
import Control.Arrow
import Control.Lens

type Metadata = Int
data Header = Header
	{ _nChildren :: Int
	, _nMeta :: Int } deriving Show

data Tree = Node Header [Tree] [Metadata] deriving Show

parseTree :: AP.Parser Tree
parseTree = do
	nc <- AP.decimal
	_ <- AP.char ' '
	nm <- AP.decimal
	children <- replicateM nc (AP.char ' ' >> parseTree)
	metaData <- replicateM nm (AP.char ' ' >> AP.decimal)
	pure $ Node (Header nc nm) children metaData

solve1 :: Tree -> Int
solve1 (Node _ children md) = go (sum md) children
	where
	go acc [] = acc
	go acc (c:cs) = go (acc + solve1 c) cs

solve2 :: Tree -> Int
solve2 (Node _ [] md) = sum md
solve2 (Node _ cs md) = DL.foldl' folder 0 $ fmap (head &&& length) $ DL.group $ DL.sort md
	where
	folder :: Int -> (Int, Int) -> Int
	folder acc (idx,mlt)
		| idx == 0 = acc
		| (idx - 1) >= lcs = acc
		| otherwise = acc + mlt*(cv !! (idx - 1))
	cv = fmap solve2 cs
	lcs = length cs

solve_1 :: IO ()
solve_1 = 
	SysIO.withFile "inputs/day08" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
		let input = partitionEithers $
			fmap (AP.parseOnly parseTree . T.pack ) file_lines
		when (length (fst input) /= 0) (print $ fst input)
		print $ fmap solve2 $ snd input

