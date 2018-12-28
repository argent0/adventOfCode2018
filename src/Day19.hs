{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Day19 (
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

import Control.Applicative
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

import qualified System.Random as Rnd

sum' :: (Foldable t, Num a) => t a -> a
sum' = DL.foldl' (+) 0

data Registers = Registers
	{ _rega :: Int
	, _regb :: Int
	, _regc :: Int
	, _regd :: Int
	, _rege :: Int
	, _regf :: Int } deriving (Eq, Show)

makeLenses ''Registers

-- Get registers by index
regn:: Int -> Lens' Registers Int
regn 0 = rega
regn 1 = regb
regn 2 = regc
regn 3 = regd
regn 4 = rege
regn 5 = regf
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

data CompState = CompState
	{ _ipVal :: Int --The value
	, _ipReg :: Int
	, _registers :: Registers } deriving Show

makeLenses ''CompState

eval :: Step -> ST.State CompState ()
eval step = do
	old_ip_register_value <- ((regn <$> use ipReg) >>= \l -> use (registers . l))
	use ipVal >>= \ip_val -> use ipReg >>= \ip_reg -> (registers.(regn ip_reg)) .= ip_val
	undefined

solve_1 :: IO ()
solve_1 =
	SysIO.withFile "inputs/day17b" SysIO.ReadMode $ \input_fh ->
		(lines <$> (SysIO.hGetContents input_fh)) >>= \file_lines -> do
		print $ file_lines
