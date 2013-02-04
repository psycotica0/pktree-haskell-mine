module Data.Betweenable where

import Data.Set (Set, union, empty, singleton, mapMonotonic, size)
import Data.Bool.HT (if')

-- This one doesn't work for all Ord, only single dimensional instances of Ord
ord_overlap :: (Ord a) => a -> a -> a -> Set Int
ord_overlap a b c = if' (and [a <= b, b < c]) (singleton 0) empty

ord_overlap_inclusive :: (Ord a) => a -> a -> a -> Set Int
ord_overlap_inclusive a b c = if' (and [a <= b, b <= c]) (singleton 0) empty

ord_overlap_exclusive :: (Ord a) => a -> a -> a -> Set Int
ord_overlap_exclusive a b c = if' (and [a < b, b < c]) (singleton 0) empty

-- Returns true if first <= second < third
within :: (Betweenable a) => a -> a -> a -> Bool
within a b c = (==) (dimensions a) $ size $ overlap a b c

-- Returns true if first <= second <= third
contained_by :: (Betweenable a) => a -> a -> a -> Bool
contained_by a b c = (==) (dimensions a) $ size $ overlap_inclusive a b c

-- This is the new one I need to handle both subsets and intersections
class Betweenable a where
	-- This returns the number of dimensions this datatype represents
	-- This is sort of a hack, I'm assuming in a few places that for any given a, there is exactly one return value
	dimensions :: a -> Int
	-- This function returns a set containing the dimenions (between 0 and the result of `dimensions`) that overlaps in the second value in the range between the first and third
	-- In this one has an exclusive upper bound, but an inclusive lower bound
	overlap :: a -> a -> a -> Set Int
	-- This does the same as the above, but has an inclusive upper bound as well
	overlap_inclusive :: a -> a -> a -> Set Int
	-- This does the same, but has an exclusive lower and upper bound
	overlap_exclusive :: a -> a -> a -> Set Int

instance Betweenable Double where
	dimensions _ = 1
	overlap = ord_overlap
	overlap_inclusive = ord_overlap_inclusive
	overlap_exclusive = ord_overlap_exclusive

instance Betweenable Integer where
	dimensions _ = 1
	overlap = ord_overlap
	overlap_inclusive = ord_overlap_inclusive
	overlap_exclusive = ord_overlap_exclusive

instance Betweenable Int where
	dimensions _ = 1
	overlap = ord_overlap
	overlap_inclusive = ord_overlap_inclusive
	overlap_exclusive = ord_overlap_exclusive

instance (Betweenable a, Betweenable b) => Betweenable (a, b) where
	dimensions _ = 2
	-- Need to compute overlap in each dimension, making the second dimension independent of the first
	overlap (a, b) (c, d) (e, f) = union (overlap a c e) $ mapMonotonic (+ dimensions a) $ overlap b d f
	overlap_inclusive (a, b) (c, d) (e, f) = union (overlap_inclusive a c e) $ mapMonotonic (+ dimensions a) $ overlap_inclusive b d f
	overlap_exclusive (a, b) (c, d) (e, f) = union (overlap_exclusive a c e) $ mapMonotonic (+ dimensions a) $ overlap_exclusive b d f
