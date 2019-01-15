{-# LANGUAGE FlexibleContexts #-}
module Util.Memo (
	memo
) where
{- THIS IS MOSTLY USELESS -}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as ST

memo :: Ord a => (a -> b) -> (a -> State (Map a b) b)
memo f = f'
	where
	f' a = do
		cache <- ST.get
		case Map.lookup a cache of
			Just b -> pure b
			Nothing -> do
				let b = f a
				ST.modify' (Map.insert a b)
				pure b
