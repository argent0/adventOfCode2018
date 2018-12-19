{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Day16 (
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

import Data.Either (fromRight, partitionEithers)
import Data.Maybe (catMaybes, isJust, fromJust)

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
import Control.Comonad.Cofree

import qualified Data.Foldable as Fld

import Util.Grid

import qualified Data.Sequence as Seq

import qualified Control.Monad.State.Strict as ST
import Data.Bits

sum' :: (Foldable t, Num a) => t a -> a
sum' = DL.foldl' (+) 0

data Instruction = Instruction Int Int Int Int deriving (Eq, Show)
opcode (Instruction oc _ _ _) = oc

data Registers = Registers {
	_rega :: Int,
	_regb :: Int,
	_regc :: Int,
	_regd :: Int } deriving (Eq, Show)


makeLenses ''Registers

-- Get registers by index
regn:: Int -> Lens' Registers Int
regn 0 = rega
regn 1 = regb
regn 2 = regc
regn 3 = regd
regn n = error $ "No register: " ++ show n

type Step = Int -> Int -> Int -> ST.State Registers ()

opr :: (Int -> Int -> Int) -> Step
opr op regA regB regC =
	(op <$> (use $ regn regA) <*> (use $ regn regB)) >>= ((regn regC) .=)

addr  = opr (+)
multr = opr (*)
banr  = opr (.&.)
borr  = opr (.|.)
gtrr  = opr $ \x y -> if x > y then 1 else 0
eqrr  = opr $ \x y -> if x == y then 1 else 0

opi :: (Int -> Int -> Int) -> Step
opi op regA valB regC =
	((`op` valB) <$> (use $ regn regA)) >>= ((regn regC) .=)

addi  = opi (+)
multi = opi (*)
bani  = opi (.&.)
bori  = opi (.|.)
gtri  = opi $ \x y -> if x > y then 1 else 0
eqri  = opi $ \x y -> if x == y then 1 else 0
eqir  = flip eqri
gtir  = flip $ opr $ \x y -> if x <= y then 1 else 0

setr regA _ regC = use (regn regA) >>= ((regn regC) .=)
seti valA _ regC = (regn regC) .= valA

data NamedInst = NamedInst
	{ _name :: String
	, _inst :: Step }

makeLenses ''NamedInst

instance Show NamedInst where
	show (NamedInst n _) = n

instructionSet :: [NamedInst]
instructionSet =
	[ NamedInst "addr" addr
	, NamedInst "multr" multr
	, NamedInst "banr" banr
	, NamedInst "borr" borr
	, NamedInst "gtrr" gtrr
	, NamedInst "eqrr" eqrr
	, NamedInst "addi" addi
	, NamedInst "multi" multi
	, NamedInst "bani" bani
	, NamedInst "bori" bori
	, NamedInst "gtri" gtri
	, NamedInst "eqri" eqri
	, NamedInst "gtir" gtir
	, NamedInst "eqir" eqir
	, NamedInst "setr" setr
	, NamedInst "seti" seti]

initReg = Registers 0 1 2 3

data Clue = Clue
	{ _before :: Registers
	, _clueInst :: Instruction
	, _after :: Registers } deriving Show

makeLenses ''Clue

sampleClue = Clue (Registers 3 2 1 1) (Instruction 9 2 1 2) (Registers 3 2 2 1)

parseRegisters :: AP.Parser Registers
parseRegisters = do
	_ <- AP.char '['
	[a,b,c,d] <- AP.decimal `AP.sepBy` (AP.string ", ")
	_ <- AP.char ']'
	pure $ Registers a b c d

parseInstruction :: AP.Parser Instruction
parseInstruction = do
	[a,b,c,d] <- AP.decimal `AP.sepBy` (AP.char ' ')
	pure $ Instruction a b c d

parseClue :: AP.Parser Clue
parseClue = do
	bf <- parseRegisters
	_ <- AP.char '\n'
	ins <- parseInstruction
	_ <- AP.char '\n'
	at <- parseRegisters
	_ <- AP.char '\n'
	pure $ Clue bf ins at

opCodeMatchClue :: Step -> Clue -> Bool
opCodeMatchClue step (Clue bf (Instruction _ op1 op2 dst) af) =
	af == ST.execState (step op1 op2 dst) bf

part1 :: [Clue] -> Int
part1 =
	--(flip trace) undefined . show . filter ((>=3) . length . snd) . fmap
	--	(\c -> (c, filter (\i -> (i ^. inst) `opCodeMatchClue` c) instructionSet))
	length . filter (>= 3) . fmap (\c -> length $ filter (\i -> (i ^. inst) `opCodeMatchClue` c) instructionSet)

part2 :: [NamedInst] -> [Clue] -> [(Int, NamedInst)]
part2 insSet =
	fmap (second head) .
	fmap head . DL.groupBy ((==) `on` fst) . DL.sortBy (compare `on` fst) . filter ((==1) . length . snd) . fmap
		(\c -> ((opcode (c ^. clueInst)), filter (\i -> (i ^. inst) `opCodeMatchClue` c) insSet))

iterator :: ([Clue], [(Int, NamedInst)], [NamedInst]) -> ([Clue], [(Int, NamedInst)], [NamedInst])
iterator (clues, dec, insSet) = (
		filter (not . (`DL.elem` (fmap fst decoded)) . opcode . (^. clueInst)) clues
		, (dec++decoded)
		, filter (\i -> not $ (i ^. name) `DL.elem` (fmap ((^. name) . snd) decoded)) insSet)
	where
	decoded :: [(Int, NamedInst)]
	decoded = part2 insSet clues

solve_1 :: IO ()
solve_1 =
	SysIO.withFile "inputs/day16p1" SysIO.ReadMode $ \input_fh ->
		SysIO.hGetContents input_fh >>= \file_contents -> do
		print $ last $ take 30 $ (\clues -> iterate iterator (clues, [], instructionSet)) $ fromRight undefined $
			(AP.parseOnly (parseClue `AP.sepBy1` AP.string "--\n")) $
			T.pack $ file_contents
		--print $ part2 instructionSet $ fromRight undefined $
		--	(AP.parseOnly (parseClue `AP.sepBy1` AP.string "--\n")) $
		--	T.pack $ file_contents

{- I have 761 samples -}
{- 527 is too high -}
