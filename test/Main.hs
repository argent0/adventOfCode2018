module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["src/Day14.hs"]
