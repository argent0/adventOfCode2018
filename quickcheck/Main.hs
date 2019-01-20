{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.QuickCheck
import Util.QuadTree
import Control.Comonad.Cofree
import Data.Set

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

instance Arbitrary Quadrant where
	arbitrary = oneof $ fmap pure $ [Q1, Q2, Q3, Q4]

instance Arbitrary Wind where
	arbitrary = oneof $ fmap pure $ [N, S, E, W, NWSE, SWNE]

instance Arbitrary a => Arbitrary (Grid a) where
	arbitrary = oneof [leafGen, vertexCompGen] --, vertexGen, sideGen, vertexCompGen]
		where
		leafGen :: Gen (Grid a)
		leafGen = leaf <$> arbitrary
		vertexGen :: Gen (Grid a)
		vertexGen = vertex <$> arbitrary <*> arbitrary <*> arbitrary
		sideGen :: Gen (Grid a)
		sideGen = side <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
		vertexCompGen :: Gen (Grid a)
		vertexCompGen = vertexComp <$> arbitrary <*>
			arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
		quadrantGen :: Gen (Grid a)
		quadrantGen = quadrant <$> arbitrary <*>
			arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
	shrink (Grid (a :< LeafF)) = [ Grid $ a :< LeafF ]
	shrink (Grid (a :< VertexF _ c)) = [ leaf a, Grid c ]
	shrink (Grid (a :< SideF _ f s)) = [ leaf a, Grid f, Grid s ]
	shrink (Grid (a :< VertexCompF _ f s t)) = [ leaf a, Grid f, Grid s, Grid t ]
	shrink (Grid (a :< QuadrantF f s t q)) = [leaf a, Grid f, Grid s, Grid t, Grid q]

prop_eq_reflexive :: Grid () -> Bool
prop_eq_reflexive xs = xs == xs

prop_quadtree_assoc :: Grid () -> Grid () -> Grid () -> Bool
prop_quadtree_assoc xs ys zs = (xs <> ys) <> zs == xs <> (ys <> zs)

main :: IO ()
main = do
	--verboseCheckWith (stdArgs { maxSize = 2, maxSuccess = 2 }) prop_eq_reflexive
	quickCheck prop_eq_reflexive
	--print $ (a <> b) <> c
	--print $ a <> (b <> c)
	--print $ (a <> b) <> c == a <> (b <> c)
	quickCheck prop_quadtree_assoc
	where
	a,b,c :: Grid ()
	a = Grid {unGrid = () :< VertexF Q4 (() :< LeafF)}
	b = Grid {unGrid = () :< VertexF Q3 (() :< LeafF)}
	c = Grid {unGrid = () :< LeafF}

