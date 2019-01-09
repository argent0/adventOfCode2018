{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Day02
	( solve_1
	, solve_2
) where

import qualified System.IO as SysIO
import qualified Data.List as DL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as DF
import Data.Functor.Foldable
import Control.Monad.Free --(retract)
import Data.Functor.Identity --(runIdentity, Identity(..))

import qualified Control.Monad.Trans.Free as CMTF
import qualified Control.Comonad.Trans.Cofree as CCTC

import Control.Arrow ((***))
import Data.Bool (bool)
import Control.Comonad.Cofree

import Data.Functor.Apply

-- Count the number of repetitions each element has
-- O(n*log n)
count :: Ord a => [a] -> Map a Integer
count = Map.fromListWith (+) . (flip zip) (repeat 1)

-- O(N^2) using only an `Eq` constraint
-- Alternative for unfolder:
-- 	unfolder (a:as) =
-- 		let (al,nas) = DL.partition (==a) as in
-- 			Just ((a, 1 + DL.genericLength al), nas)
count' :: Eq a => [a] -> [(a, Integer)]
count' = DL.unfoldr unfolder
	where
	unfolder [] = Nothing
	unfolder (a:as) = Just $ DL.foldl' folder ((a, 1), []) as
	-- Computes the length and partitions in one pass
	folder ((a, n), nas) e
		| e == a = ((a, n + 1), nas)
		| otherwise = ((a, n), e:nas)

-- data Rec a = Rec (Rec a) | Done a deriving Show
--
-- 00:00 < argent0> hi, is this isomorphic to something more popular? data Rec a
-- = Rec (Rec a) | Done a deriving Show
-- 00:01 < Solonarv> It's isomorphic to Free Identity
-- 00:02 < argent0> Solonarv: thanks I'll check that out
-- 00:02 < Solonarv> argent0: Free is in free:Control.Monad.Free, Identity is in
-- base:Data.Functor.Identity
-- 00:03 < c_wraith> it's also Fix Maybe
-- 00:04 < c_wraith> err, no. it's Fix (Either a)
-- 00:04 < c_wraith> or if you want to be particularly smart-ass about it, (Nat,
-- a)
-- 00:05 < Solonarv> more generally, Free f a ~ Fix (Either a `Compose` f)
-- 00:05 < c_wraith> hmm. good point.
-- 00:06 < Solonarv> ( similarly, Cofree f a ~ Fix ((,) a `Compose` f) )
-- 00:07 < c_wraith> do backticks work in types? I can't say as I've ever tried.
-- 00:07 < Solonarv> They do, you might need -XTypeOperators though – I don't
-- remember
-- 00:08 < Solonarv> % :k Int `Either` Char
-- 00:08 < yahb> Solonarv: Int `Either` Char :: *
-- 00:09 < puffnfresh> Free Identity is also known as the "Partiality Monad"
-- 00:09 < puffnfresh> argent0: you're recreating Partiality exactly :)
-- 00:09 < Solonarv> which one is that?
-- 00:10 < argent0> c_wraith: puffnfresh: I'll check those too
-- 00:10 < c_wraith> I have a result! | I need to think more.
-- 00:11 < c_wraith> it's a very direct way to encode partiality into a total
-- language that supports codata
-- 00:11 < Solonarv> Oh, I see!
-- 00:12 < c_wraith> sometimes you even want it in Haskell. it's a pure way to
-- produce progress reports from a computation, for instance.
-- 00:14 < c_wraith> so long as you don't mind the computation only running when
-- you ask for the next progress report.
-- 00:14 < Solonarv> Seems like it'd be quite useful as a monad transformer
-- 00:14 < c_wraith> (deepseq it with par to get it to run in the background!)

-- | Predicate that checks if two elements are in t.
--
-- retract :: Free f a -> f a
-- data Free f a = Pure a | Free (f (Free f a))
-- data Rec a = Done a | Rec (Rec a)
--
-- Rec is isomorphic to Free Identity
--
-- >>> elem2 1 2 [1,2,undefined]
-- (True,True)
-- >>> elem2 1 2 [2,3]
-- (False,True)
-- >>> elem2 1 2 [1,3]
-- (True,False)
elem2 :: forall a t. (Eq a, Foldable t) => a -> a -> t a -> (Bool, Bool)
elem2 a b = runIdentity . retract . ana coAlg . DF.toList
	where
	coAlg [] = CMTF.Pure (False, False)
	coAlg (e:es)
		| e == a = CMTF.Pure (True, b `elem` es)
		| e == b = CMTF.Pure (a `elem` es, True)
		| otherwise = CMTF.Free (Identity es)

-- O(N*log n)
-- where N is the length of the input list
-- where n is the length of the strings inside the input list
checksum :: [String] -> Integer
checksum = uncurry (*) . DL.foldl' folder (0,0) . fmap count
	where
	folder :: (Integer, Integer) -> Map Char Integer -> (Integer, Integer)
	folder (p, t) counts = (+p) *** (+t) $ (bool 0 1) *** (bool 0 1) $ elem2 2 3 counts
	--(False, False) -> (p, t)
	--(True, False) -> (p + 1, t)
	--(False, True) -> (p, t + 1)
	--(True, True) -> (p + 1, t + 1)

solve_1 :: IO ()
solve_1= do
	SysIO.withFile "inputs/day02" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines ->
		print $ checksum file_lines

-- Part 2

-- | Unfolds `t a` into a stream of (Maybe a).
--
-- It starts with Justs as long as `t a` has elements. Then, it continues with
-- `Nothing`.
--
-- We can define a stream as:
--
-- @data Rec a = Rec a (Rec a)@
--
-- This is isomorphic to
--
-- @Cofree Identity a@
--
-- where
--
-- @Cofree f a = a :< Cofree f a@
--
-- E.g:
--
-- >>> take 5 $ DF.toList $ extend [True, False]
-- [Just True,Just False,Nothing,Nothing,Nothing]
--
-- >>> take 5 $ DF.toList $ extend []
-- [Nothing,Nothing,Nothing,Nothing,Nothing]

extend :: forall a t . Traversable t => t a -> Cofree Identity (Maybe a)
extend = ana coAlg . DF.toList
	where
	coAlg [] = Nothing CCTC.:< (Identity [])
	coAlg (a:as) = (Just a) CCTC.:< (Identity as)

-- | True if argument strings differ by one letter.
--
-- It is lazy on both strings
-- >>> diffOne ['a','b',undefined] ['x','y',undefined]
-- False
-- >>> diffOne "fghij" "fguij"
-- True
-- >>> diffOne "abc" "acb"
-- False
--
-- Different lengths don't have diffOne
-- >>> diffOne "abc" "abdx"
-- False

diffOne :: forall a t t'. (Foldable t, Foldable t', Eq a) => t a -> t' a -> Bool
diffOne a b = runIdentity . retract . ana coAlg $ liftF2 (,) (extend $ DF.toList a) (extend $ DF.toList b)
	where
	-- Unfold a `Free Identity Bool` value from a stream of `(Maybe a, Maybe a)`
	coAlg :: Cofree Identity (Maybe a, Maybe a) -> Base (Free Identity Bool) (Cofree Identity (Maybe a, Maybe a))
	-- It can be concluded that the result is False if one of the lists ends
	coAlg ( (Nothing, _) :< _ )  = CMTF.Pure False
	coAlg ( (_, Nothing) :< _ )  = CMTF.Pure False
	coAlg ( (Just e, Just ee) :< ps )
		-- The result can be computed from the remaining values, if a difference is found
		| e /= ee = CMTF.Pure (rest ps)
		-- otherwise, it is necessary to continue
		| otherwise = CMTF.Free ps

	-- Convert a stream to a Bool value.
	-- `hylo` is for hylomorphism: builds up and tears down a virtual structure
	rest :: Identity (Cofree Identity (Maybe a, Maybe a)) -> Bool
	rest ps = hylo alg coAlg' $ runIdentity $ fmap (fmap (uncurry meq)) $ ps

	-- Unfold a `Free Identity Bool` value from a stream of `Maybe Bool`
	coAlg' :: Cofree Identity (Maybe Bool) -> Base (Free Identity Bool) (Cofree Identity (Maybe Bool))
	coAlg' (Nothing :< _) = CMTF.Pure True	-- Conclude if we reach the Nothing's
	coAlg' ((Just False) :< _) = CMTF.Pure False -- Or if we reach a False value
	coAlg' ((Just True) :< js) = CMTF.Free js -- Continue if we get a True

	-- Fold or destruct a `Free Identity Bool` value into a Bool
	alg :: Base (Free Identity Bool) Bool -> Bool
	alg (CMTF.Pure True) = True
	alg (CMTF.Pure False) = False
	alg (CMTF.Free js) = runIdentity js

	-- Compare as long as there is something to compare
	meq :: Maybe a -> Maybe a -> Maybe Bool
	meq Nothing Nothing = Nothing
	meq Nothing (Just _) = Just False
	meq (Just _) Nothing = Just False
	meq (Just x) (Just y) = Just (x==y)

-- | Return the common elements when there is only one difference
--
-- >>> commons "fghij" "fguij"
-- Just "fgij"
-- >>> commons ['a','b',undefined] ['x','y',undefined]
-- Nothing
-- >>> commons "abc" "abc"
-- Nothing

commons :: Eq a => [a] -> [a] -> Maybe [a]
commons [] [] = Nothing
commons (_:_) [] = Nothing
commons [] (_:_) = Nothing
commons (a:as) (b:bs)
	| a == b = (a:) <$> commons as bs
	| otherwise = bool Nothing (Just as) (as == bs)

-- | Given a list of strings pair each one with the sub-list of strings that
-- differ by one letter.
--
-- >>> part ["abc","abd","xyz"]
-- [("abc",["abd"]),("abd",["abc"]),("xyz",[])]
part :: forall a . Eq a => [[a]] -> [([a], [[a]])]
part as = DL.unfoldr coAlg as
	where
	coAlg :: [[a]] -> Maybe ( ([a], [[a]]), [[a]] )
	coAlg [] = Nothing
	coAlg (b:bs) =
		let g = filter (diffOne b) as in
		Just $ ( (b,g) , bs )

-- | Solve part 2 by comparing all strings
solve_2 :: IO ()
solve_2 = do
	SysIO.withFile "inputs/day02" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines ->
		print $
			fmap (\(h, l) -> fmap (commons h) l) $
			filter (not . null. snd) $ part $ file_lines

{-
"zihwtxagwifpbsnwleydukjmqv"
"zihwtxagsifpbsnwleydukjmqv"

"zihwtxagifpbsnwleydukjmqv"
"zihwtxagifpbsnwleydukjmqv"
-}
