module Util.Grid (
) where

import Control.Arrow
import Control.Lens
import Linear
import qualified Data.Array.IArray as Arr

-- A 2-d grid
--type Grid2d a = Arr.Array Integer (Arr.Array Integer a)
--
--gridWith :: Integer -> (V2 Integer -> a) -> Grid a
--gridWith size f =
--	Arr.array b $ zip [1..size] $ fmap mapper $
--	DL.groupBy ((==) `on` ((^. _x) . fst)) $ fmap (id &&& f) $
--	V2 <$> [1..size] <*> [1..size]
--	where
--	mapper :: [(V2 Integer, Integer)] -> Arr.Array Integer Integer
--	mapper = Arr.array b . fmap (first (^. _y))
--	b :: (Integer, Integer)
--	b = (1,size)
