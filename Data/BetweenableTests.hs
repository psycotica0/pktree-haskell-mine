module Data.BetweenableTests (test) where

import Test.QuickCheck (arbitrary, suchThat, Property, conjoin, printTestCase, Gen)

import Data.Set (singleton, empty)

import Data.Betweenable

showTestCase label a b = printTestCase (label ++ ": " ++ show a) b

-- Double, Integer, and Int each use the ord_overlap set of functions
-- By testing those, it should be sufficient

test_ord_overlap = conjoin [simple, lower, upper, under, over]
	where
	simple = do
		a <- arbitrary :: Gen Integer
		b <- arbitrary `suchThat` (>= a)
		c <- arbitrary `suchThat` (> b)
		showTestCase "ord_overlap: simple" (a, b, c) $ (ord_overlap a b c == singleton 0)
	lower = do
		a <- arbitrary :: Gen Integer
		c <- arbitrary `suchThat` (> a)
		showTestCase "ord_overlap: lower" (a, a, c) $ (ord_overlap a a c == singleton 0)
	upper = do
		a <- arbitrary :: Gen Integer
		c <- arbitrary `suchThat` (> a)
		showTestCase "ord_overlap: upper" (a, c, c) $ (ord_overlap a c c == empty)
	under = do
		a <- arbitrary :: Gen Integer
		b <- arbitrary `suchThat` (< a)
		c <- arbitrary `suchThat` (> a)
		showTestCase "ord_overlap: under" (a, b, c) $ (ord_overlap a b c == empty)
	over = do
		a <- arbitrary :: Gen Integer
		c <- arbitrary `suchThat` (> a)
		b <- arbitrary `suchThat` (>= c)
		showTestCase "ord_overlap: over" (a, b, c) $ (ord_overlap a b c == empty)

test_ord_overlap_inclusive = conjoin [simple, lower, upper, under, over]
	where
	simple = do
		a <- arbitrary :: Gen Integer
		b <- arbitrary `suchThat` (>= a)
		c <- arbitrary `suchThat` (>= b)
		showTestCase "ord_overlap_inclusive: simple" (a, b, c) $ (ord_overlap_inclusive a b c == singleton 0)
	lower = do
		a <- arbitrary :: Gen Integer
		c <- arbitrary `suchThat` (>= a)
		showTestCase "ord_overlap_inclusive: lower" (a, a, c) $ (ord_overlap_inclusive a a c == singleton 0)
	upper = do
		a <- arbitrary :: Gen Integer
		c <- arbitrary `suchThat` (>= a)
		showTestCase "ord_overlap_inclusive: upper" (a, c, c) $ (ord_overlap_inclusive a c c == singleton 0)
	under = do
		a <- arbitrary :: Gen Integer
		b <- arbitrary `suchThat` (< a)
		c <- arbitrary `suchThat` (> a)
		showTestCase "ord_overlap_inclusive: under" (a, b, c) $ (ord_overlap_inclusive a b c == empty)
	over = do
		a <- arbitrary :: Gen Integer
		c <- arbitrary `suchThat` (> a)
		b <- arbitrary `suchThat` (> c)
		showTestCase "ord_overlap_inclusive: over" (a, b, c) $ (ord_overlap_inclusive a b c == empty)

test_ord_overlap_exclusive = conjoin [simple, lower, upper, under, over]
	where
	simple = do
		a <- arbitrary :: Gen Integer
		b <- arbitrary `suchThat` (> a)
		c <- arbitrary `suchThat` (> b)
		showTestCase "ord_overlap_exclusive: simple" (a, b, c) $ (ord_overlap_exclusive a b c == singleton 0)
	lower = do
		a <- arbitrary :: Gen Integer
		c <- arbitrary `suchThat` (> a)
		showTestCase "ord_overlap_exclusive: lower" (a, a, c) $ (ord_overlap_exclusive a a c == empty)
	upper = do
		a <- arbitrary :: Gen Integer
		c <- arbitrary `suchThat` (> a)
		showTestCase "ord_overlap_exclusive: upper" (a, c, c) $ (ord_overlap_exclusive a c c == empty)
	under = do
		a <- arbitrary :: Gen Integer
		b <- arbitrary `suchThat` (<= a)
		c <- arbitrary `suchThat` (> a)
		showTestCase "ord_overlap_exclusive: under" (a, b, c) $ (ord_overlap_exclusive a b c == empty)
	over = do
		a <- arbitrary :: Gen Integer
		c <- arbitrary `suchThat` (> a)
		b <- arbitrary `suchThat` (>= c)
		showTestCase "ord_overlap_exclusive: over" (a, b, c) $ (ord_overlap_exclusive a b c == empty)

-- TODO: Test Tuples

test = conjoin [test_ord_overlap, test_ord_overlap_inclusive, test_ord_overlap_exclusive]
