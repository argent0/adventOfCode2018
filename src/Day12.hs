{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Day12 (
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
import Data.Either (partitionEithers)
import Control.Monad (when, join, replicateM, void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (ord)
import Control.Arrow
import Control.Lens
import qualified Data.Array.IArray as Arr
import Linear hiding (trace)
import Util.Parsers (numbers, signeds)

data Rule = Rule [Bool] Bool deriving Show --True means there is a plant

type ProblemState = Arr.Array  Integer  Bool

replicate' :: Integer -> a -> [a]
replicate' n a
	| n <= 0 = []
	| otherwise = a:(replicate' (n-1) a)

--initialState ::  Integer  -> [Bool] -> ProblemState
--initialState padding input = Arr.array (-padding,il+padding-1) $
--	zip (irange (-padding) (il+padding)) $ (replicate' padding False) ++
--		input ++ (replicate' padding False)
--	where
--	il :: Integer
--	il = fromIntegral $ length input

initialState ::  Integer  -> [Bool] -> ProblemState
initialState padding input = 
	Arr.array (-10,il+padding-1) $
		zip (irange (-10) (il+padding)) $ (replicate' 10 False) ++ input ++ (replicate' padding False)
	where
	il :: Integer
	il = fromIntegral $ length input

irange :: Integer -> Integer -> [Integer]
irange mn mx
	| mx < mn = []
	| otherwise = mn:(irange (mn+1) mx)


getSurr ::  Integer  -> ProblemState -> [Bool]
getSurr idx arr
	| idx < mn + 2 = (replicate' padleft False) ++ (foldMap (pure . (arr Arr.!)) (irange mn (idx+(4-padleft-absidx))))
	| idx > mx - 2 = (foldMap (pure . (arr Arr.!)) (irange (idx-2) mx)) ++ (replicate' padrignt False)
	| otherwise = foldMap (pure . (arr Arr.!)) $ irange (idx-2) (idx+2)
	where
	absidx = idx - mn
	padleft = mn + 2 - idx
	padrignt = 4 - (mx - idx + 2)
	(mn, mx) = Arr.bounds arr

data CRule = CRule Int Bool
cRule :: Rule -> CRule
cRule (Rule rls b) = CRule (bin2int rls) b

bin2int :: [Bool] -> Int
bin2int = DL.foldl' (+) 0 . zipWith (*) [16,8,4,2,1] . fmap mapper
	where
	mapper True = 1
	mapper False = 0

-- New O(1)
type RuleSet = Arr.Array Int Bool
mkRuleSet :: [Rule] -> RuleSet
mkRuleSet rules = Arr.array
		(cRuleIdx (head c_rules), cRuleIdx (last c_rules)) $
		fmap (\(CRule i b) -> (i, b)) $ c_rules
	where
	c_rules :: [CRule]
	c_rules = DL.sortBy (compare `on` cRuleIdx)  $ fmap cRule rules
	cRuleIdx (CRule i _) = i

applyRules :: RuleSet -> ProblemState -> ProblemState
applyRules !rules !ps = Arr.array (mn, mx) $ zip [mn..mx] $
	fmap ((`fwd` rules) . bin2int) $
	fmap (`getSurr` ps) [mn..mx]
	where
	fwd = flip (Arr.!)
	(mn, mx) = Arr.bounds ps

sumElemWithPlant :: ProblemState -> Integer
sumElemWithPlant = DL.foldl' (+) 0 . fmap fst . filter (snd) . Arr.assocs

-- Length 100
pinput :: [Bool]
pinput = [True,True,False,True,False,False,False,False,True,False,False,True,False,False,False,False,False,False,True,False,False,True,True,True,True,True,True,False,False,True,False,True,True,True,True,False,False,False,False,False,True,False,False,False,False,False,False,True,True,False,True,True,False,True,True,False,False,False,True,False,False,True,False,False,False,False,True,False,True,False,True,True,False,False,True,True,False,True,True,False,True,False,True,False,False,True,False,True,False,False,False,False,True,False,True,False,False,True,False,True]

-- sed 's/#/True /g;s/\./False /g;s/^/Rule [/;s/=>/]/' day12 | xclip
rules :: [Rule]
rules =
	[ Rule [True,False,True,False,False] False
	, Rule [False,False,True,True,False] False
	, Rule [False,False,False,True,False] False
	, Rule [False,False,True,False,False] False
	, Rule [True,True,True,True,True] True
	, Rule [False,True,False,True,False] False
	, Rule [True,True,True,True,False] False
	, Rule [True,True,True,False,False] False
	, Rule [False,True,False,False,True] True
	, Rule [True,False,False,True,False] True
	, Rule [True,False,True,False,True] False
	, Rule [True,False,False,False,True] True
	, Rule [False,False,True,True,True] False
	, Rule [False,False,False,True,True] True
	, Rule [True,True,False,False,True] True
	, Rule [True,False,False,False,False] False
	, Rule [False,True,False,True,True] True
	, Rule [True,False,True,True,True] True
	, Rule [False,True,True,False,True] True
	, Rule [True,False,False,True,True] False
	, Rule [False,True,False,False,False] True
	, Rule [False,True,True,True,False] False
	, Rule [True,True,False,False,False] True
	, Rule [True,True,False,True,True] True
	, Rule [True,True,False,True,False] True
	, Rule [True,False,True,True,False] True
	, Rule [False,True,True,False,False] False
	, Rule [False,False,True,False,True] False
	, Rule [False,False,False,False,True] False
	, Rule [True,True,True,False,True] False
	, Rule [False,False,False,False,False] False
	, Rule [False,True,True,True,True] False ]

ln :: Integer -> Integer
--ln x = 6596 + (dy % dx)*((x-667) % 1)
--ln x = 27356 + 55 * (x - 499)
ln x = 187021 + 55 * (x - 3402)


itake :: Integer -> [a] -> [a]
itake _ [] = []
itake n (a:as)
	| n<=0 = []
	| otherwise = a:(itake (n-1) as)

n :: Integer
--n = 1 + 50000000000
--n = 1 + 20
n = 50000000000

sps :: ProblemState -> String
sps = fmap mapper . Arr.elems
	where
	mapper True = '#'
	mapper False = '.'

evolve :: Integer -> IO ()
evolve steps =
	putStrLn $ unlines $ fmap (\(sn,ps) -> (show sn) ++ ": " ++ (sps ps)) $ itake steps $ zip (irange 0 steps) $ iterate (applyRules (mkRuleSet rules)) $ initialState steps pinput

compute :: Integer -> IO ()
compute steps =
	putStrLn $ unlines $ fmap (\(sn,ps) -> (show sn) ++ " " ++ (show $ sumElemWithPlant ps)) $ itake steps $ zip (irange 0 steps) $ iterate (applyRules (mkRuleSet rules)) $ initialState steps pinput


solve_1 :: IO ()
solve_1 =
--	SysIO.withFile "inputs/day12b" SysIO.ReadMode $ \input_fh ->
--		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
	compute 10000
