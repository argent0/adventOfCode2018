{-# LANGUAGE OverloadedStrings #-}
{- check regex-applicative package-}
module Util.Parsers (
	numbers
	, signeds
) where

import qualified Data.List as DL
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import qualified Data.Attoparsec.Text as AP
import Data.Function (on)
import qualified Data.Text as T
import Control.Monad (void)

-- Extract a list of unsigned integer-like numbers from a string
numbers :: Integral a => String -> [a]
numbers = snd . partitionEithers . fmap (AP.parseOnly AP.decimal . T.pack) . DL.groupBy ((==) `on` isDigit)

signeds :: Integral a => String -> [a]
signeds = snd .
	partitionEithers .
	fmap (AP.parseOnly (AP.signed AP.decimal) . T.pack) .
	DL.groupBy ((==) `on` isInteresting)
	where
	isInteresting a = (a=='+') || (a=='-') || isDigit a

