{-# LANGUAGE OverloadedStrings #-}
{- Abusing maps -}
module Day05 (
	solve_1
) where

import qualified System.IO as SysIO
import Data.Char (toUpper, isUpper, isLower)
import Data.Function (on)

sameType :: Char -> Char -> Bool
sameType = (==) `on` toUpper

opppositePolarity :: Char -> Char -> Bool
opppositePolarity a b 
	| isLower a = isUpper b
	| isUpper a = isLower b
	| otherwise = False

reduce :: String -> String
reduce = go []
	where
	go :: String -> String -> String
	go prev [] = reverse prev
	go [] (a:as) = go (a:[]) as
	go (a:as) (b:bs)
		| (sameType a b) && (opppositePolarity a b) = go as bs
		| otherwise = go (b:a:as) bs

solve_1 :: IO ()
solve_1 = 
	SysIO.withFile "inputs/day05" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
		print $ fmap (length . reduce) file_lines
