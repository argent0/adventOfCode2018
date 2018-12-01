module Day01 (
	solve
) where

import qualified System.IO as SysIO
import qualified Data.List as DL

parseInteger :: String -> Integer
parseInteger [] = error "parseInteger: Empty string"
parseInteger ('+':s) = read s
parseInteger ('-':s) = negate $ read s

solve :: IO ()
solve = do
	SysIO.withFile "inputs/day01" SysIO.ReadMode $ \input_fh ->
		SysIO.hGetContents input_fh >>= \contents ->
		(pure $ (DL.foldl' (+) 0) $ fmap parseInteger $ lines contents)
		>>= print
