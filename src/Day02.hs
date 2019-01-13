{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
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
import Data.Functor.Foldable.TH
import Control.Monad.Free --(retract)
import Data.Functor.Identity --(runIdentity, Identity(..))
import Data.Functor.Const

import qualified Control.Monad.Trans.Free as CMTF
import qualified Control.Comonad.Trans.Cofree as CCTC

import Control.Arrow ((***))
import Data.Bool (bool)
import Control.Comonad.Cofree

import Data.Functor.Apply

import Data.Maybe (catMaybes)
import Data.Functor.Compose

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
elem2 :: forall a . Eq a => a -> a -> [a] -> (Bool, Bool)
elem2 a b = runIdentity . retract . ana coAlg
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
	folder (p, t) counts = (+p) *** (+t) $ (bool 0 1) *** (bool 0 1) $ elem2 2 3 $ DF.toList counts
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
--
-- See: Data.Align

extend :: forall a . [a] -> Cofree Identity (Maybe a)
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

diffOne :: forall a . Eq a => [a] -> [a] -> Bool
diffOne as bs = runIdentity . retract . ana coAlg $ liftF2 (,) (extend as) (extend bs)
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

-- For the recursion in commons
--
-- Rec a (Rec a) : Keep adding elements to the result
-- No : Abort when there are more than two different values or the lists are
-- equal
-- Yes [a] : There is only one difference and the rest of the elements are [a]
data Rec a = Rec a (Rec a) | No | Yes [a] deriving Show
makeBaseFunctor ''Rec

-- | Return the common elements when there is only one difference
--
-- >>> commons "fghij" "fguij"
-- Just "fgij"
-- >>> commons ['a','b',undefined] ['x','y',undefined]
-- Nothing
-- >>> commons "abc" "abc"
-- Nothing
commons :: forall a . Eq a => [a] -> [a] -> Maybe [a]
commons xx yy = hylo alg coAlg (xx, yy)
	where

	alg :: Base (Rec a) (Maybe [a]) -> Maybe [a]
	alg NoF = Nothing
	alg (YesF acc) = Just acc
	alg (RecF a acc) = (a:) <$> acc

	coAlg :: ([a], [a]) -> Base (Rec a) ([a], [a])
	coAlg ([], _) = NoF
	coAlg (_, []) = NoF
	coAlg ( (a:as), (b:bs) )
		| a == b = RecF a (as, bs)
		| otherwise = bool NoF (YesF as) (as == bs)

--data Free (f :: * -> *) a = Pure a | Free (f (Free f a))

--type List' a = Free (Maybe `Compose` (,) a) a -- ~ [a]
--build :: forall a . [a] -> List' a
--build xx = ana coAlg xx
--	where
--	coAlg :: [a] -> Base (List' a) [a]
--	coAlg [] = CMTF.Free (Compose Nothing)
--	coAlg [a] = CMTF.Pure a
--	coAlg (a:as) = CMTF.Free (Compose $ Just (a, as))

type Cmp a = Free (Maybe `Compose` (Maybe `Compose` (,) a)) [a]
build :: forall a . Eq a => [a] -> [a] -> Cmp a
build xx yy = ana coAlg (xx, yy)
	where
	coAlg :: ([a], [a]) -> Base (Cmp a) ([a], [a])
	coAlg ([], _) = CMTF.Free (Compose Nothing)
	coAlg (_, []) = CMTF.Free (Compose Nothing)
	coAlg ( (a:as), (b:bs) )
		| a == b = CMTF.Free $ Compose $ Just $ Compose $ Just (a, (as, bs))
		| as == bs = CMTF.Pure as
		| otherwise = CMTF.Free (Compose Nothing)
-- bld :: forall a . Eq a => [a] -> [a] -> Cmp a
-- bld xx yy = apo coAlg (xx, yy)
-- 	where
-- 	coAlg :: ([a], [a]) -> Base (Cmp a) (Either (Cmp a) ([a], [a]))
-- 	coAlg ([], _) = CMTF.Pure Nothing
-- 	coAlg (_, []) = CMTF.Pure Nothing
-- 	coAlg ( (a:as), (b:bs) )
-- 		| a == b = CMTF.Free _


-- On abusing constrains.
--
-- 16:35:23 <argent0> is there a class for things that can be built in sequence? I'm thinking in defining something like: quadTime :: forall a b c f . Foldable f => (a -> b -> c) -> f a -> f b -> [c]
-- 16:35:50 <xsperry> hi. is there a way to access non-exported methods? I want to create an instance of some class. library exports the class, but not method required to create an instance of that class
-- 16:35:58 <argent0> that applies f to all pairs a b
-- 16:36:16 <geekosaur> xsperry, no. some packages provide an internals module for such things
-- 16:36:49 <geekosaur> otherwise, if it wasn't exported, it's not identifiably there (it's there as machine code but no way to find it)
-- 16:37:24 <Welkin> the purpose of a module is to expose an API
-- 16:37:39 <Welkin> anything that is exported is part of that API, and anything that is not exported it meant to be internal only
-- 16:37:40 <c_wraith> xsperry, that's something that's usually done explicitly to prevent creating instances
-- 16:38:49 <xsperry> I'm trying to create PrintfType instance for MonadIO m => m. there doesn't seem to be an internals module
-- 16:38:56 <geekosaur> and yes, if it's hding constructors it's usually for a reason
-- 16:40:12 <argent0> Is there a `fromList` type class?
-- 16:40:35 <dmwit> argent0: Yes, IsList
-- 16:41:21 <dmwit> argent0: For your first question you might like https://hackage.haskell.org/package/these-0.7.5/docs/Data-Align.html#t:Align
-- 16:42:06 <argent0> dmwit: Thanks, I'll check those out
-- 16:45:48 <ski> argent0 : did you mean to combine every `a' in the `f a' with every `b' in the `f b' ?
-- 16:46:23 <ski> or proceed in lockstep/parallel, combining corresponding pairs at corresponding "positions" (in some sense) ?
-- 16:46:55 <xsperry> c_wraith, I realize that. was just wondering if there's an escape hatch, and I'm a bit surprised that there isn't one
-- 16:46:57 <ski> ("applies f to all pairs a b" would seem to suggest the former to me, but perhaps you intended the latter. or maybe something else)
-- 16:48:54 <argent0> ski: the first option
-- 16:49:22 <argent0> thus, "quadraticTime"
-- 16:49:44 <ski> ok, then i think you don't want `Data.Align'
-- 16:49:58 <ski> @type foldMap (:[])
-- 16:49:59 <lambdabot> Foldable t => t a -> [a]
-- 16:50:28 <ski> you can convert your `f a' and `g a' to `[a]' and `[b]', and then combine those as usual, using `a -> b -> c', to `[c]' ?
-- 16:50:52 <ski> (using list comprehension, or `concat'/`concatMap', or `liftA2')
-- 16:51:20 <argent0> I have an implementation: quadTime f as bs = f <$> (DF.toList as) <*> (DF.toList bs)
-- 16:51:21 <ski> (er, `f b', not `g a')
-- 16:51:37 <ski> yea, that is the same thing
-- 16:51:44 <ski> (as the `liftA2' version)
-- 16:51:50 <argent0> yes
-- 16:52:04 <ski> (the brackets are redundant, btw)
-- 16:52:47 * ski would probably not bother with calling `toList' there, though
-- 16:53:12 <argent0> ski, why not?
-- 16:53:47 <ski> (if `quadTime' immediately throws away the extra `f' structure, only using the list of elements, then why does it need to deal with `f' at all, why not just pass it the lists ?)
-- 16:55:23 <xsperry> is this syntax correct? I'm not seeing any effect. {-# OPTIONS_GHC -Worphans #-}
-- 16:55:31 <ski> anyway, perhaps you think it's nicer to put the `toList' calls there, rather than in the caller (which might duplicate those calls) .. which would be fair enough
-- 16:55:44 <xsperry> (I want to disable those warnings)
-- 16:55:52 <ski> it's just that it looks suspicious to me
-- 16:56:33 <argent0> ski, yes I'd like a "generic" quadratic time algorithm, that I can apply anywhere where it makes sense.
-- 16:56:46 <argent0> e.g: map
-- 16:56:54 <argent0> s/map/Map/
-- 16:57:16 <dmj`> Does disabling optimizations (i.e. --disable-optimization) affect symbol resolution for the C FFI? Am running into an issue where it works w/ optimizations, but not w/o (get an undefined reference)
-- 16:57:25 <ski> if your operation returned a `f c', instead of `[c]', it would be another matter
-- 16:57:28 <Welkin> xsperry: use {-# LANGUAGE StandaloneDeriving #-}
-- 16:58:34 <Welkin> that is if you are deriving the instance
-- 16:58:38 <nshepperd> xsperry: note that you can't create an instance 'for' MonadIO m => m a, anyway
-- 16:58:51 <ski> argent0 : fwiw, you could even have `forall a b c f g. (Foldable f,Foldable g) => (a -> b -> c) -> f a -> g b -> [c]', with your current implementation. no need to require `f' to be the same as `g'
-- 16:59:14 <Welkin> if you want to turn off warnings you must use `no-orphans`, not `orphans`
-- 17:00:12 <nshepperd> xsperry: if you could create it, it would be an instance for 'm a' and would overlap with all the other instances (and it would happen to induce a MonadIO constraint, once chosen)
-- 17:00:13 <ski> argent0 : .. and this smells possibly similar to wanting to have e.g. a `(+)' operation which can accept arguments of different numeric types, in the same call
-- 17:00:17 <argent0> ski, Right.
-- 17:00:49 <ski> (which i'm not convinced is a good idea .. in any case in the way people usually seem to want that)
-- 17:01:15 <ski> (e.g. adding a scalar to a matrix)
-- 17:01:26 <argent0> ski: That's why I used f
-- 17:01:32 <ski> ok
-- 17:01:53 <xsperry> nsshepperd are you sure? I just did :)
-- 17:01:56 <nshepperd> i imagine that overlapping instances would make type errors with usage of printf even more confusing than usual
-- 17:02:15 <ski> argent0 : `exists f. Foldable f *> f a' is more or less the same thing as `[a]'
-- 17:02:19 <aranea> Adding a scalar to a matrix? What would that even do? Add x to every entry, or add x*unit matrix?
-- 17:02:35 <xsperry> > printf "test\n" :: MonadIO m => m ()   => test
-- 17:02:38 <lambdabot>  error:
-- 17:02:38 <lambdabot>      â€¢ Expected a constraint, but â€˜m ()â€™ has kind â€˜*â€™
-- 17:02:38 <lambdabot>      â€¢ In an expression type signature: MonadIO m => m () => test
-- 17:03:27 <argent0> ski: where does the *> comes from?
-- 17:03:34 <argent0> s/comes/come/
-- 17:03:50 <xsperry> nshepperd, I don't see any issues, String and IO instances still work
-- 17:03:52 <jackdk> > printf "test\n" :: MonadIO m => m ()
-- 17:03:54 <lambdabot>  error:
-- 17:03:55 <lambdabot>      â€¢ Could not deduce (PrintfType (m1 ()))
-- 17:03:55 <lambdabot>          arising from a use of â€˜printfâ€™
-- 17:04:08 <dmwit> ski: (You know the standard objection to that claim?)
-- 17:04:12 <ski> argent0 : so `quadTime :: forall a b c f g. (Foldable f,Foldable g) => (a -> b -> c) -> f a -> g b -> [c]' is basically `quadTime :: forall a b c. (a -> b -> c) -> (exists f. Foldable f *> f a) -> (exists g. Foldable g *> g b) -> [c]', which is then more or less the same thing as `forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]'
-- 17:04:33 <ski> dmwit : yes, there's a blog post about it
-- 17:04:37 * dmwit nods
-- 17:04:49 <nshepperd> the standard object is the 'or less' part :p
-- 17:04:53 <nshepperd> *objection
-- 17:05:13 <dmwit> argent0: *> comes from ski's brain =)
-- 17:05:18 <xsperry> jackdk, I downloaded printf's source and modified it. it can't be done from the user's space, as Text.Printf does not export PrintfType.spr
-- 17:05:43 <jackdk> ah
-- 17:05:45 <dmwit> argent0: Where you can think of => as being a "function" which takes a typeclass dictionary, you can think of *> as being a "pair" that includes a typeclass dictionary.
-- 17:06:15 <ski> argent0 : anyway, so according to this argument, which is admittedly a bit shaky (as dmwit indicated), you don't buy more expressivity by this use of `Foldable', only some convenience (which may be enough to warrant your use)
-- 17:06:17 <Welkin> you can transform => into ->
-- 17:06:25 <dmwit> argent0: (It doesn't actually exist in GHC. But it's convenient shorthand for things that do exist.)
-- 17:06:35 <Welkin> and all typeclass constraints into parameters separated by arrow functions ->
-- 17:08:31 <ski> "Free Monoids in Haskell" by dolio in 2015-02-21 <http://comonad.com/reader/2015/free-monoids-in-haskell/> is the blag in question
-- 17:09:16 <ski> argent0 : `forall a. (F a -> T)' is logically equivalent to `(exists a. F a) -> T', agree ?
-- 17:09:36 <argent0> ski: yes
-- 17:09:40 <ski> e.g. `length :: forall a. ([a] -> Int)' means the same thing as `length :: (exists a. [a]) -> Int'
-- 17:10:10 <ski> the former says that, for every type `a', we can apply `length' to a value of type `[a]', and get a result of type `Int' back
-- 17:10:51 <ski> the latter says that we get a result of type `Int' back, if we apply `length', as long as there exists some type `a' such that the argument we apply `length' to has type `[a]'
-- 17:11:10 <argent0> ok
-- 17:11:42 <ski> argent0 : now, i claim that `Constr => (T -> U)' is logically equivalent to `(Constr *> T) -> U'
-- 17:12:29 <argent0> thats the typeclass dictionaries?
-- 17:12:47 <ski> you can *use* (/consume/"call") a value of type `Constr => U', if you (the caller/user) provide evidence for `Constr', and in that case, you can use the value as if it had type `U'
-- 17:13:14 <argent0> ok
-- 17:13:38 <ski> (to produce/construct/define such a value, you (the callee/producer) just *assume* that the caller will provide evidence of `Constr'. you can just rely on it being there)
-- 17:15:23 <ski> when *using* (/consuming/"calling") a value of type `Constr *> T', you (the caller/user) is provided with evidence for (/ gets to assume) `Constr', in addition to a value of type `T'
-- 17:16:01 <ski> (while to produce/construct/define such a value, you (the callee/producer) need to both provide evidence for `Constr', as well as produce a value of type `T')
-- 17:17:13 <ski> so, just as the roles of producer/consumer, or callee/caller, &c. are swapped, when using `forall' vs. when using `exists' (who gets to pick the type to use in place of the type variable, and who has to be content with any choice made)
-- 17:17:58 <argent0> I don't follow
-- 17:19:00 <ski> `Nothing' has type `forall a. Maybe a'. this means that the user of `Nothing' gets to pick any type to use in place of `a', say `Bool', so that the user may use `Nothing' as having type `Maybe Bool'
-- 17:19:37 <ski> or, `reverse' has type `forall a. [a] -> [a]'. the caller/user may pick `a' as `Bool', so that they may use `reverse' as having type `[Bool] -> [Bool]'
-- 17:20:21 <argent0> yes
-- 17:20:26 <ski> otoh, the producer of `reverse', the callee, the implementation, does not get to pick `a', or assume anything about it. to the implementation, `a' behaves as an abstract data type, an opaque type
-- 17:20:49 <ski> if we imagine
-- 17:21:03 <ski>   silly :: Bool -> (exists a. (a,a -> a,a -> String))
-- 17:21:23 <ski>   silly False = (False,not,show)
-- 17:21:32 <ski>   silly True  = ("True",reverse,id)
-- 17:22:12 <ski> then here the *implementation* gets to pick the particular type to use in place of `a', since `exists' was used, not `forall'
-- 17:22:25 <ski> (in fact, the particular choice made can depend on run-time values, as you can see)
-- 17:22:34 <argent0> yes
-- 17:23:18 <ski> while the user, the caller, of `silly', now has to treat the type `a' in the return value as an abstract/opaque/hidden/forgotten type (sometimes called a "skolem type")
-- 17:23:37 <xsperry> another useful printf instance.. for Text
-- 17:23:52 <argent0> yes, he only know that there is a value a, a->a and a->String
-- 17:24:06 <ski> so changing a `forall' into an `exists', or vice versa, flips around who gets to pick/choose the type, and who must treat it as abstract
-- 17:24:12 <ski> argent0 : makes sense, now ?
-- 17:24:21 <argent0> yes
-- 17:24:54 <argent0> how does this apply to quadTime?
-- 17:24:57 <ski> now the roles of who gets to *assume* (/require) evidence of the constraint `Constr', and who is required to *provide* said evidence, is swapped, when we use `Constr => U' vs. `Constr *> T'
-- 17:25:01 <argent0> :p
-- 17:25:24 <ski> so, just as
-- 17:25:29 <ski> <ski> argent0 : `forall a. (F a -> T)' is logically equivalent to `(exists a. F a) -> T', agree ?
-- 17:25:43 <ski> (because the left side of `->' is logically inverted)
-- 17:25:47 <ski> we also have
-- 17:25:53 <ski> <ski> argent0 : now, i claim that `Constr => (T -> U)' is logically equivalent to `(Constr *> T) -> U'
-- 17:27:08 <ski> if you think in terms of the dictionary passing translation/implementation of type class evidence, you can think of `Constr => (T -> U)' as being the curried version, and `(Constr *> T) -> U' as being the uncurried version
-- 17:27:25 <ski> conceptually, we're passed a separate argument encoding evidence for `Constr', with the former
-- 17:27:57 <ski> while with the latter, conceptually, that evidence is bundled together (in a pair, so to speak) with the argument of type `T'
-- 17:28:27 <ski> so one couls say that `*>' is to `=>' as `(,)' is to `(->)' (and also compare with how `exists' is to `forall')
-- 17:28:36 <ski> argent0, ok ?
-- 17:29:24 * ski assumes argent0 has heard of the dictionary passing translation for type classes, before
-- 17:29:32 <argent0> nope
-- 17:29:35 <ski> ok
-- 17:29:42 <ski> consider e.g.
-- 17:30:05 <ski>   elem :: Eq a => a -> [a] -> Bool
-- 17:30:21 <ski>   elem _  [    ] = False
-- 17:30:37 <ski>   elem x0 (x:xs) = x0 == x || elem x0 xs
-- 17:30:49 <argent0> yes
-- 17:30:51 <ski> and
-- 17:30:52 <ski> @src Eq
-- 17:30:52 <lambdabot> class Eq a where
-- 17:30:52 <lambdabot>     (==), (/=) :: a -> a -> Bool
-- 17:31:16 <ski> one way to implement this is to translate that type class definition into a data type, like
-- 17:31:36 <ski>   data EqDict a = MkEqDict {eq,neq :: a -> a -> Bool}
-- 17:31:53 <ski> and an instance like
-- 17:31:59 <ski>   instance Eq Ordering
-- 17:32:01 <ski>     where
-- 17:32:09 <ski>     LT == LT = True
-- 17:32:12 <ski>     ...
-- 17:32:19 <ski> is translated into a value
-- 17:32:30 <argent0> why are you instancing Eq Ordering?
-- 17:32:42 <ski> just as an example
-- 17:32:47 <ski>   instEqOrdering :: EqDict Ordering
-- 17:33:08 <ski>   instEqOrdering = MkEqDict {eq = ...,neq = ...}
-- 17:33:24 <ski> and then the definition of `elem' above is translated into
-- 17:33:38 <ski>   elem :: EqDict a -> a -> [a] -> Bool
-- 17:33:56 <ski>   elem _    _  [    ] = False
-- 17:34:09 <ski>   elem dict x0 (x:xs) = eq dict x0 x || elem dict x0 xs
-- 17:34:32 <ski> where `eq dict' just selects the function of type `a -> a -> Bool' in the `eq' field of the dictionary argument
-- 17:34:58 <ski> so, now we explicitly pass around records of implementation of "type class methods", for particular types (here `a')
-- 17:34:59 <argent0> what is eq type signature
-- 17:35:01 <argent0> ?
-- 17:35:17 <ski>   eq,neq :: EqDict a -> (a -> a -> Bool)
-- 17:35:26 <ski> as field selector functions
-- 17:36:03 <ski> the point is, `=>' in types gets translated to `->', with this translation
-- 17:36:17 <argent0> yes
-- 17:36:23 <ski> and explicit arguments are passed around, instead of implicit propagation of evidence for type class constraints
-- 17:36:44 <ski> if there was any `*>', then it would get translated into `(,)'
-- 17:37:11 <ski> so that e.g. `exists a. Eq a *> [a]' would get translated into `exists a. (EqDict a,[a])'
-- 17:37:42 <ski> while `[exists a. Eq a *> a]' would get translated into `[exists a. (EqDict a,a)]'
-- 17:38:23 <ski> in the former case, there is a single, unknown, type `a', about which is only known that it's an instance of `Eq'. and we have a list of values of that same common unknown type `a'
-- 17:39:02 <ski> in the latter case, each element of the list could possibly be of a different type `a' (all instances of `Eq', however). and therefore a user must assume the worst, and can't compare different elements to each other
-- 17:39:17 <ski> in the former case, we could compare different elements to each other, however
-- 17:40:15 <ski> anyway, i forgot to say before that, combining the two logical equivalences, `forall a. (C a => (F a -> T))' is logically equivalent to `(exists a. (C a *> F a)) -> T'
-- 17:40:25 <ski> which is exactly the step i used to get from
-- 17:40:35 <ski>   quadTime :: forall a b c f g. (Foldable f,Foldable g) => (a -> b -> c) -> f a -> g b -> [c]
-- 17:40:38 <ski> to
-- 17:40:45 <ski>   quadTime :: forall a b c. (a -> b -> c) -> (exists f. Foldable f *> f a) -> (exists g. Foldable g *> g b) -> [c]
-- 17:41:04 <ski> (with some additional rearrangment/reordering of arguments and constraints)
-- 17:41:28 <ski> argent0 : making any sense, now ?
-- 17:41:55 <xsperry> you managed to confused me (and I learned what Eq was two years ago)
-- 17:41:56 <argent0> nope
-- 17:42:24 <argent0>  while `[exists a. Eq a *> a]' would get translated into `[exists a. (EqDict a,a)]'
-- 17:42:31 <argent0> this part ^
-- 17:42:53 <argent0> and how all a, would be diffrent types
-- 17:43:21 <ski> a value of type `[exists a. Eq a *> a]' is a list whose elements have type `exists a. Eq a *> a'. so, for every element of the list, there exists some type `a', known to be an instance of `Eq', such that the element has type `a'
-- 17:44:26 <ski> e.g. `False' has type `Bool'. and we know `Eq False' holds. therefore we can (conceptually) bundle this value with this piece of constraint evidence, to claim that `False' also has type `Eq Bool *> Bool'
-- 17:44:30 <argent0> yes, then, as you said, you can't compare
-- 17:44:40 <argent0> elements
-- 17:44:58 <ski> then, we can forget / abstract away the `Bool', into a type variable, claiming that `False' also has type `exists a. Eq a *> a'
-- 17:46:18 <ski> (it also has type `exists a. Eq a *> Bool', and also has type `exists a. Eq Bool *> a', and also has type `exists a. Eq Bool *> Bool'. but neither of those are what i wanted in this case. we can choose *which* occurences of `Bool' we want to abstract away, and which we want to keep "transparent")
-- 17:47:00 <ski> similarly, `"True"' has type `String', which is an instance of `Eq', hence `"True"' can also be given type `exists a. Eq a *> a'
-- 17:47:36 <ski> so both `False' and `"True"' can be given the same type `exists a. Eq a *> a', so we can put them in a common list, `[False,"True"]', having type `[exists a. Eq a *> a]'
-- 17:47:40 <argent0> So, your point is about whether I should use the Foldable contrain or not
-- 17:47:59 <argent0> or be honest about it being a list
-- 17:48:20 <argent0> is that it?
-- 17:48:56 <Solonarv_> I believe so
-- 17:49:17 <ski> i'm arguing that, upto `Foldable' essentially being `ToList' (which isn't quite the case, see the blag above), your formulation of `quadTime', involving `Foldable', would be equivalent in power to one only involving lists. so there would then be no expressive gain in using `Foldable'
-- 17:50:10 <ski> but, there could still be a convenience gain. presumably you're going to put those calls to `DL.toList' *somewhere*, and in that case it may be that the best place to put them in your program is inside this operation `quadTime'
-- 17:50:52 <ski> the decision/judgement is of course up to you
-- 17:51:16 <ski> argent0 : i was just attempting to explain why it *looked* potentially fishy, to me
-- 17:51:40 <ski> @where existential-antipattern
-- 17:51:40 <lambdabot> "Haskell Antipattern: Existential Typeclass" by Luke Palmer at <http://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/>
-- 17:51:56 <ski> is another blag, doing a somewhat similar argument
-- 17:52:10 <ski> in some cases, using existentials can be indispensible
-- 17:52:50 <ski> but still, some uses of them could be considered "frivolous", unnecessary, overkill, however you want to say it
-- 17:53:33 * ski . o O ( <https://en.wikipedia.org/wiki/Rule_of_least_power> )
-- 17:55:02 <argent0> that is what you mean by power in: "would be equivalent in power to one only involving lists"
-- 17:55:05 <argent0> ?
-- 17:55:37 <glguy> People love calling things anti-patterns
-- 17:55:42 <ski> more or less, yes
-- 17:56:09 <Welkin> anti-anti-patterns
-- 17:56:15 <xsperry> glguy you don't think it is justified in above case?
-- 17:57:27 <ski> argent0 : here's another example
-- 17:57:35 <ski> let's say you write
-- 17:57:51 <ski>   frob :: IO Foo -> IO Blah
-- 17:57:58 <glguy> xsperry: I think people learned from that post that when someone says ExistentialQuantification that they get to write "anti-pattern"
-- 17:58:19 <ski>   frob act = do foo <- act
-- 17:58:21 <ski>                 ..foo..
-- 17:58:26 <Welkin> frobisher?
-- 17:58:32 <Welkin> from Torchwood?
-- 17:58:44 <ski> not using `act' anymore in the body of `frob', apart from that first line
-- 17:59:28 <argent0> Welkin: frob.com
-- 17:59:36 <argent0> ski: ok
-- 17:59:38 <ski> one could argue that the type signature of `frob' is misleading, since it introduces the additional power of passing an I/O-action computing a `Foo', as an argument, instead of simply passing a plain `Foo'
-- 17:59:57 * hackage gi-cairo-render 0.0.1 - GI friendly Binding to the Cairo library.  http://hackage.haskell.org/package/gi-cairo-render-0.0.1 (eyevanmalicesun)
-- 18:00:34 <coldpress> if I have a monad m, and a bunch of Kleisli functions of the form a -> m b. How do I use arrow combinators on those Kleisli functions?
-- 18:00:41 <argent0> I'm not sure I agree on the miseleading, as you said you have to put the toLIst somewhere
-- 18:00:47 <ski> if one sees such a type signature, one would usually assume that the operation wanted to use the callback action in some "serious" way, like executing it more than once, or perhaps sometimes not executing it at all. or perhaps registering it to be (possibly) executed later
-- 18:01:13 <argent0> ski, yes
-- 18:01:15 <Solonarv> you wrap them in 'Kleisli', coldpress. Yes, it's cumbersome.
-- 18:01:21 <argent0> that makes sense
-- 18:01:27 <ski> or at least executing something else beforehand, or executing `act' in some dynamic context that `frob' introduces (and presumably tears down at the end)
-- 18:01:52 <ski> e.g. `forkIO', or `bracket', or other operations of this sort
-- 18:01:57 * hackage gi-cairo-connector 0.0.1 - GI friendly Binding to the Cairo library.  http://hackage.haskell.org/package/gi-cairo-connector-0.0.1 (eyevanmalicesun)
-- 18:02:16 <Solonarv> :t runKleisli (Kleisli ?f &&& Kleisli ?g) -- coldpress
-- 18:02:17 <lambdabot> (?g::a -> m c', ?f::a -> m c, Monad m) => a -> m (c, c')
-- 18:02:26 <argent0> yes, for the current implementation if equivalent to just Foo -> IO Blah
-- 18:02:44 <ski> seeing this implementation of `frob', the kneejerk reaction should be to refactor the invokation of `act' to occur not inside `frob', but just before the call to `frob', changing the signature of `frob' to just `Foo -> IO Blah'
-- 18:02:51 <ski> right, exactly
-- 18:04:29 <ski> `frob' doesn't (at least currently ?) need the extra power of being passed a callback action, and so, unless the particular interface (or "impedance mismatch" between different non-matching interfaces) is forced on us for some reason, we'd be inclined to do said refactoring
-- 18:05:15 <ski> argent0 : still, if you have an `IO Foo', you have to put the invokation of that *somewhere*, getting a `Foo' out of it
-- 18:05:31 <ski> (which is parallel to putting your `toList' *somewhere*)
-- 18:06:44 <ski> (fwiw, i'd say that `frob act' was referentially transparent in `act', with the original type signature and definition of `frob')
-- 18:06:59 <argent0> I think I see your point now
-- 18:07:44 <ski> i'm sorry if i'm not able to explain it more shortly/clearly
-- 18:08:38 <argent0> ski: the frob example made it clear to me
-- 18:08:52 <ski> ok, good

-- | All vs all application of f
quadTime :: forall a b c . (a -> b -> c) -> [a] -> [b] -> [c]
quadTime f as bs = f <$> as <*> bs

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

-- The problem input as an IO effect. For repl-convinience.
input :: IO [String]
input = do
	input_fh <- SysIO.openFile "inputs/day02" SysIO.ReadMode
	contents <- SysIO.hGetContents input_fh
	pure $ lines $ contents

-- | Solve part 2 by comparing all strings
solve_2 :: IO ()
solve_2 = do
	i <- input
	putStrLn . unlines . catMaybes $ quadTime commons i i
