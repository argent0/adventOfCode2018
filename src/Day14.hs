{-
 - You finally have a chance to look at all of the produce moving around.
 - Chocolate, cinnamon, mint, chili peppers, nutmeg, vanilla... the Elves must
 - be growing these plants to make hot chocolate! As you realize this, you
 - hear a conversation in the distance. When you go to investigate, you
 - discover two Elves in what appears to be a makeshift underground kitchen
 - laboratory.
 -
 - The Elves are trying to come up with the ultimate hot chocolate recipe; the
 - 're even maintaining a scoreboard which tracks the quality score (0-9) of
 - each recipe.
 -
 - Only two recipes are on the board: the first recipe got a score of 3, the
 - second, 7. Each of the two Elves has a current recipe: the first Elf starts
 - with the first recipe, and the second Elf starts with the second recipe.
 -
 - To create new recipes, the two Elves combine their current recipes. This
 - creates new recipes from the digits of the sum of the current recipes'
 - scores. With the current recipes' scores of 3 and 7, their sum is 10, and
 - so two new recipes would be created: the first with score 1 and the second
 - with score 0. If the current recipes' scores were 2 and 3, the sum, 5,
 - would only create one recipe (with a score of 5) with its single digit.
 -
 - The new recipes are added to the end of the scoreboard in the order they
 - are created. So, after the first round, the scoreboard is 3, 7, 1, 0.
 -
 - After all new recipes are added to the scoreboard, each Elf picks a new
 - current recipe. To do this, the Elf steps forward through the scoreboard a
 - number of recipes equal to 1 plus the score of their current recipe. So,
 - after the first round, the first Elf moves forward 1 + 3 = 4 times, while
 - the second Elf moves forward 1 + 7 = 8 times. If they run out of recipes,
 - they loop back around to the beginning. After the first round, both Elves
 - happen to loop around until they land on the same recipe that they had in
 - the beginning; in general, they will move to different recipes.
 -
 - Drawing the first Elf as parentheses and the second Elf as square brackets,
 - they continue this process:
 -
 - (3)[7]
 - (3)[7] 1  0
 -  3  7  1 [0](1) 0
 -  3  7  1  0 [1] 0 (1)
 - (3) 7  1  0  1  0 [1] 2
 -  3  7  1  0 (1) 0  1  2 [4]
 -  3  7  1 [0] 1  0 (1) 2  4  5
 -  3  7  1  0 [1] 0  1  2 (4) 5  1
 -  3 (7) 1  0  1  0 [1] 2  4  5  1  5
 -  3  7  1  0  1  0  1  2 [4](5) 1  5  8
 -  3 (7) 1  0  1  0  1  2  4  5  1  5  8 [9]
 -  3  7  1  0  1  0  1 [2] 4 (5) 1  5  8  9  1  6
 -  3  7  1  0  1  0  1  2  4  5 [1] 5  8  9  1 (6) 7
 -  3  7  1  0 (1) 0  1  2  4  5  1  5 [8] 9  1  6  7  7
 -  3  7 [1] 0  1  0 (1) 2  4  5  1  5  8  9  1  6  7  7  9
 -  3  7  1  0 [1] 0  1  2 (4) 5  1  5  8  9  1  6  7  7  9  2
 -
 - The Elves think their skill will improve after making a few recipes (your
 - puzzle input). However, that could take ages; you can speed this up
 - considerably by identifying the scores of the ten recipes after that. For
 - example:
 -
 -     If the Elves think their skill will improve after making 9 recipes, the
 -     scores of the ten recipes after the first nine on the scoreboard would
 -     be 5158916779 (highlighted in the last line of the diagram).
 -
 -     After 5 recipes, the scores of the next ten would be 0124515891.
 -     After 18 recipes, the scores of the next ten would be 9251071085.
 -     After 2018 recipes, the scores of the next ten would be 5941429882.
 -
 - What are the scores of the ten recipes immediately after the number of
 - recipes in your puzzle input?
 -
 - Your puzzle answer was 1221283494.
 -
 - The first half of this puzzle is complete! It provides one gold star: *
 - --- Part Two ---
 -
 - As it turns out, you got the Elves' plan backwards. They actually want to
 - know how many recipes appear on the scoreboard to the left of the first
 - recipes whose scores are the digits from your puzzle input.
 -
 -     51589 first appears after 9 recipes.
 -     01245 first appears after 5 recipes.
 -     92510 first appears after 18 recipes.
 -     59414 first appears after 2018 recipes.
 -
 - How many recipes appear on the scoreboard to the left of the score sequence
 - in your puzzle input?
 -
 - Your puzzle input is still 652601.
 -
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Day14 (
	solve_1
) where

import Data.Ratio
import Text.Printf
import qualified System.IO as SysIO
import Data.Function (on)
import qualified Data.List as DL
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import Debug.Trace

import qualified Data.Either as DE
import Data.Maybe (catMaybes)

import Control.Monad (forever, when, join, replicateM, void, foldM, foldM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (ord)
import Control.Arrow
import Control.Lens

import qualified Data.Array.Diff as Arr

import Linear hiding (trace)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Functor.Foldable --recursion-schemes
import Data.Functor.Foldable.TH --makeBaseFunctor
import qualified Data.Functor.Base as DFB
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Bool (bool)

import qualified Data.Foldable as DF

import Util.Grid

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import qualified Control.Monad.State.Strict as ST


sum' :: (Foldable t, Num a) => t a -> a
sum' = DL.foldl' (+) 0

data CState = CState
	{ _storage :: Arr.Array Int (Maybe Int)
	, _size :: Int
	, _capacity :: Int } deriving Show

makeLenses ''CState

initState :: Int -> CState
initState capa = CState
	(Arr.array (1,capa) $ zip [1..capa] $ repeat Nothing)
	0 capa

pushBack :: Int -> CState -> CState
pushBack n (CState sto sz cap)  = if sz < cap
	then CState (sto Arr.// [(sz+1, Just n)]) (sz+1) cap
	else trace ("New capacity: " ++ show (2*cap)) $ CState
			(Arr.array (1,2*cap) $ (Arr.assocs sto) ++
				((cap+1,Just n):(zip [cap+2..] $ replicate (cap-1) Nothing)))
			(cap+1)
			(2*cap)

csTail :: CState -> Int -> [Int]
csTail (CState sto sz cap) amount
	| amount >= sz = catMaybes $ fmap (sto Arr.!) [1..sz]
	| otherwise = catMaybes $ fmap (sto Arr.!) [sz-amount..sz]

(!) :: CState -> Int -> Int
(!) st idx = case (st ^. storage) Arr.! idx of
	Nothing -> error "Bad index"
	Just r -> r

display :: (Int, Int, CState) -> String
display (e1, e2, st) =
	concat $ catMaybes $
	Arr.elems $
	Arr.accum acc2
		(Arr.accum acc1 (Arr.amap (fmap (printf " %d ")) $ (st ^. storage)) [(e1,Nothing)])
		[(e2,Nothing)]
	where
	acc1 :: Maybe String -> Maybe String -> Maybe String
	acc1 org Nothing = (printf "(%s)" . filter (/=' ')) <$> org
	acc2 :: Maybe String -> Maybe String -> Maybe String
	acc2 org Nothing = (printf "[%s]" . filter (/=' ')) <$> org

solve1 :: Int -> Int -> Int -> (Int, Int, CState)
solve1 stSize nPractice nPred =
	head $ dropWhile (pred . thrd) $
	--thrd $ last $ take 10 $
		iterate it (1,2, pushBack 7 $ pushBack 3 $ initState stSize)
	where
	pred :: CState -> Bool
	pred = (< nPred + nPractice) . (^. size)
	thrd (_,_,x) = x
	it !(!e1,!e2,!st) = let
			re1 = st ! e1
			re2 = st ! e2
			nSt = foldr (pushBack) st $ nextRecipes re1 re2
			nre1 = (e1 + re1 + 1) `mod` (nSt ^. size)
			nre2 = (e2 + re2 + 1) `mod` (nSt ^. size)
			nnre1 = if nre1 == 0 then nSt ^. size else nre1
			nnre2 = if nre2 == 0 then nSt ^. size else nre2
		--in trace (display (nnre1, nnre2, nSt)) $ (nnre1, nnre2, nSt)
		in (nnre1, nnre2, nSt)

data MatchStatus a
	= FullMatch
	| PartialMatch [a] [a]
	deriving Show

pushMatch :: Eq a => MatchStatus a -> a -> MatchStatus a
pushMatch FullMatch _ = FullMatch
pushMatch (PartialMatch (t:ts) []) a
	| a == t = PartialMatch (t:ts) ts
	| otherwise = PartialMatch (t:ts) []

pushMatch (PartialMatch (t:ts) [p]) a
	| a == p = FullMatch
	| a == t = PartialMatch (t:ts) ts
	| otherwise = PartialMatch (t:ts) []

pushMatch (PartialMatch (t:ts) (p:ps)) a
	| a == p = PartialMatch (t:ts) ps
	| a == t = PartialMatch (t:ts) ts
	| otherwise = PartialMatch (t:ts) []

matchSeq :: Eq a => MatchStatus a -> [a] -> MatchStatus a
matchSeq = DF.foldl' pushMatch

solve2 :: [Int] -> Int -> (MatchStatus Int,Int,Int, Int, CState)
solve2 target stSize =
	head $ dropWhile (not . pred . sec) $
	--last $ take 21 $
		iterate it
			( PartialMatch target [] --Wether it has matched
			, 2 -- The amount of digits matched so far
			, 1,2, pushBack 7 $ pushBack 3 $ initState stSize)
	where
	pred :: MatchStatus Int -> Bool
	pred FullMatch = True
	pred _ = False
	sec (x,_,_,_,_) = x
	ltarget = length target
	it :: (MatchStatus Int,Int,Int, Int, CState) -> (MatchStatus Int,Int,Int, Int, CState)
	it !(!currMatch, !nRecep, !e1,!e2,!st) = let
			re1 = st ! e1
			re2 = st ! e2
			nxtRecp = nextRecipes re1 re2
			nSt = foldr pushBack st $ nxtRecp
			nre1 = (e1 + re1 + 1) `mod` (nSt ^. size)
			nre2 = (e2 + re2 + 1) `mod` (nSt ^. size)
			nnre1 = if nre1 == 0 then nSt ^. size else nre1
			nnre2 = if nre2 == 0 then nSt ^. size else nre2
			nnRecep = nRecep + length nxtRecp
		--in trace (display (nnre1, nnre2, nSt)) $ (nnre1, nnre2, nSt)
		in case matchSeq (PartialMatch target []) (csTail nSt 20) of
			--NoMatch -> (target, NoMatch, nnRecep, nnre1, nnre2, nSt)
			FullMatch -> (FullMatch, nnRecep, nnre1, nnre2, nSt)
			--ts -> trace ((show ts) ++ " " ++ display (nnre1,nnre2, nSt)) $ (ts, nnRecep, nnre1, nnre2, nSt)
			ms -> (ms, nnRecep, nnre1, nnre2, nSt)

quinq :: (a,b,c,d,e) -> (c,d,e)
quinq (_,_,c,d,e) = (c,d,e)

test1 = solve2 [5,1,5,8,9] 20
test2 = solve2 [0,1,2,4,5] 20
test3 = solve2 [9,2,5,1,0] 20
test4 = solve2 [5,9,4,1,4] 20

--unfold
--Fri, 14 Dec 2018 02:08:54 -0300
nextRecipes :: Int -> Int -> [Int]
nextRecipes e ee
	| s < 10 = [s]
	| otherwise = u:d:( if u + d > 10 then nextRecipes u d else [])
	where
	s = e + ee
	u = s `mod` 10
	d = (s `div` 10) `mod` 10

-- | Combine two recipes
--
-- >>> combineRecipes 3 7
-- 1 :| [0]
-- >>> combineRecipes 2 3
-- 5 :| []
--
combineRecipes :: Int -> Int -> NonEmpty Int
combineRecipes a b = bool (q :| [r]) (s :| []) (s < 10)
	where
	s = a + b
	(q, r) = s `divMod` 10

-- Queue xs x (y:ys), x is the head, y is the second followed by `ys` and then
-- `reverse xs`.
data Queue a =
	Queue [a] a [a]
	deriving (Show, Functor)

front :: Queue a -> a
front (Queue _ a _) = a

enqueue :: Queue a -> a -> Queue a
enqueue (Queue xs x ys) n = Queue (n:xs) x ys

-- Can I get rid of the maybe using a different type.
-- dequeue :: Queue a -> (a, Maybe (Queue a))
-- dequeue (Queue [] a []) = (a, Nothing)
-- dequeue (Queue xs a []) = let (x:xxs) = reverse xs in (a, Just $ Queue [] x xxs)
-- dequeue (Queue xs a (y:ys)) = (a, Just $ Queue xs y ys)

-- | Enqueues a list
--
-- >>> enqueueList (Queue "" 'a' "") "bc"
-- Queue "cb" 'a' ""
enqueueList :: Queue a -> [a] -> Queue a
enqueueList = DL.foldl' enqueue

-- | Rotate a queue by  n >= 0 elements.
--rotateQueue :: Int -> Queue a -> Queue a
--rotateQueue 0 q = q
--rotateQueue _ q@(Queue [] a []) = q
--rotateQueue n (Queue xs a []) =
--	let (x:xxs) = reverse xs in rotateQueue (n-1) $ Queue [a] x xxs
--rotateQueue n (Queue xs a (y:ys)) = rotateQueue (n-1) $ Queue (a:xs) y ys

advanceQueue :: Int -> NonEmpty a -> Queue a -> Queue a
advanceQueue 0 _ q = q
advanceQueue n rep@(a :| as) (Queue [] _ []) = --trace ("reverse") $
	advanceQueue (n - 1) rep (Queue [] a as)
advanceQueue n rep (Queue xs _ []) =
	let (x:xxs) = reverse xs in advanceQueue (n-1) rep (Queue [] x xxs)
advanceQueue n rep (Queue xs _ (y:ys)) =
	advanceQueue (n-1) rep (Queue xs y ys)

data RecipeGenerator = RecipeGenerator
	{ _firstElf :: Either (Seq Int) (Queue Int) --Al recipes "in front" of the first elf.
	, _secondElf :: Either (Seq Int) (Queue Int) --Al recipes "in front" of the second elf.
	, _total :: NonEmpty Int
	, _nRecipes :: Integer
	, _firstPos :: NonEmpty Integer
	, _secondPos :: NonEmpty Integer
	, _memory :: Map Integer [Integer]
	, _firstInitialPos :: Integer
	} deriving Show

makeLenses ''RecipeGenerator

recipe :: Getter (Either (Seq Int) (Queue Int)) Int
recipe = to $ g
	where
	g :: Either (Seq Int) (Queue Int) -> Int
	g (Left s) = case Seq.viewl s of
		(a Seq.:< _) -> a
	g (Right (Queue _ r _)) = r

initialState :: RecipeGenerator
initialState = RecipeGenerator
	{ _firstElf = Right $ Queue [] 3 [7]
	, _secondElf = Right $ Queue [] 7 []
	, _total = 7 :| [3]
	, _nRecipes = 2
	, _firstPos = 0 :| []
	, _secondPos = 1 :| []
	, _memory = Map.fromList [(8,[]), (9,[])]
	, _firstInitialPos = 0
	}

-- | Evolve the generator of recipes
--
-- >>> putStr $ unlines $ map (unwords . map show . reverse . DF.toList . (^. total)) $ take 16 $ iterate nextState initialState
-- 3 7
-- 3 7 1 0
-- 3 7 1 0 1 0
-- 3 7 1 0 1 0 1
-- 3 7 1 0 1 0 1 2
-- 3 7 1 0 1 0 1 2 4
-- 3 7 1 0 1 0 1 2 4 5
-- 3 7 1 0 1 0 1 2 4 5 1
-- 3 7 1 0 1 0 1 2 4 5 1 5
-- 3 7 1 0 1 0 1 2 4 5 1 5 8
-- 3 7 1 0 1 0 1 2 4 5 1 5 8 9 
-- 3 7 1 0 1 0 1 2 4 5 1 5 8 9 1 6
-- 3 7 1 0 1 0 1 2 4 5 1 5 8 9 1 6 7
-- 3 7 1 0 1 0 1 2 4 5 1 5 8 9 1 6 7 7
-- 3 7 1 0 1 0 1 2 4 5 1 5 8 9 1 6 7 7 9
-- 3 7 1 0 1 0 1 2 4 5 1 5 8 9 1 6 7 7 9 2
--
nextState :: RecipeGenerator -> RecipeGenerator
nextState rg = ST.execState ( do
		firstElf %= \fe -> case fe of
			Right q -> Right (advanceQueue
				(e1Recipe + 1)
				(NE.reverse newTotal)
				(enqueueList q $ DF.toList newRecipes)
				)
			Left s -> undefined

		secondElf .= Right (advanceQueue
				(e2Recipe + 1)
				(NE.reverse newTotal)
				(enqueueList (DE.fromRight undefined (rg ^. secondElf)) $ DF.toList newRecipes)
			)

		total .= newTotal

		nRecipes .= nextNRecipes

		firstPos .= if e1NextPos < 10
			then e1NextPos :| []
			else NE.cons e1NextPos (rg ^. firstPos)

		secondPos .= NE.cons e2NextPos (rg ^. secondPos)

		when (e1NextPos > 10 && (rg ^. firstInitialPos) `elem` [8,9]) $
			memory %= Map.adjust (e1NextPos:) (rg ^. firstInitialPos)

		firstInitialPos .= if e1NextPos < 10
			then e1NextPos
			else (rg ^. firstInitialPos)
	) rg
	where
	e1NextPos = (NE.head (rg ^. firstPos) + fromIntegral e1Recipe + 1) `mod` nextNRecipes
	e2NextPos = (NE.head (rg ^. secondPos) + fromIntegral e2Recipe + 1) `mod` nextNRecipes

	e1Recipe, e2Recipe :: Int
	e1Recipe = (rg ^. firstElf . recipe)
	e2Recipe = (rg ^. secondElf . recipe)
	newRecipes = combineRecipes e1Recipe e2Recipe
	newTotal = foldr NE.cons (rg ^. total) $ NE.reverse newRecipes
	nextNRecipes = (rg ^. nRecipes) + DL.genericLength (DF.toList newRecipes)

do1 :: Integer -> [Int]
do1 nPractice = answer
	where
	answer = take 10 $ DL.genericDrop nPractice $ DL.reverse $ DF.toList $ (finalState ^. total)
	finalState = head $ dropWhile ((< nPractice + 10) . (^. nRecipes)) $
		iterate nextState initialState

-- trace (">" ++ show (enqueueList (recipeGenerator ^. firstElf) $ DF.toList newRecipes)) $
-- trace (">" ++ show (enqueueList (recipeGenerator ^. secondElf) $ DF.toList newRecipes)) $


{- Reddit's solution mstksg, interesting idea that doesn't work. The type
 - signatures have been fixed from the original -}

--chocolatePractice :: [Int]
--chocolatePractice = 3 : 7 : go 0 1 (Seq.fromList [3, 7])
--	where
--	go :: Int -> Int -> Seq.Seq Int -> [Int]
--	go !p1 !p2 !tp = newDigits ++ go p1' p2' tp'
--		where
--		sc1 = tp `Seq.index` p1
--		sc2 = tp `Seq.index` p2
--		newDigits = nextRecipes p1 p2
--		tp' = tp <> Seq.fromList newDigits
--		p1' = (p1 + sc1 + 1) `mod` length tp'
--		p2' = (p2 + sc2 + 1) `mod` length tp'
--
--
--substrLoc :: [Int] -> [Int] -> Int
--substrLoc xs = length
--             . takeWhile (not . (xs `DL.isPrefixOf`))
--             . DL.tails
--
--day14b :: [Int] -> Int
--day14b xs = xs `substrLoc` chocolatePractice

solve_1 :: IO ()
solve_1 =
	print $ do1 652601

{- First run ~ 3h
 -  1  4  5  9  1  1  6  9  9  8  1  6  8  5  2  7  6  9  9  5  2  1  5  5  4  1
 -  7  4  1  1  1  2  2  1  1  2  2  5  9  1  1  0  6  3
 -  3  8  7  1  8  6  6  6  1  1  4  4  8  1  3  4  1  0  5  1  0  1  5  7  1  3
 -  3  9  1  0  9  7  1  3  2  2  4  1  2  1  2  1  1  2  6  1  0  1  7  5  8  2
 -  1  0  1  1  4  8  4  1  2  2  1  2  8  3  4  9  4
 -}
{- 1221283494 is right!-}
