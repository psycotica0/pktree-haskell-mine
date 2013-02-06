module Data.Zone (Zone(Zone), divide_zone_by, contains, subset, intersect) where

import Data.Divisible (Divisible, divide_into_lower, divide_into_upper)
import Data.Offsetable (Offsetable, (*+*), (*-*))
import Data.Betweenable (Betweenable, overlap, within, contained_by, overlap_exclusive, dimensions)

import Data.Set (size, unions)

data Zone a = Zone a a deriving (Show, Read, Eq)

divide_zone_by :: (Divisible a, Offsetable a) => Zone a -> a -> [Zone a]
divide_zone_by (Zone low high) divisor = zipWith Zone lowers uppers
	where
	lowers = division divide_into_lower
	uppers = division divide_into_upper
	division func = map (*+* low) $ func (high *-* low) divisor

contains :: (Betweenable a, Eq a) => Zone a -> a -> Bool
-- I've had to add a special case so that (Zone 3 3) is seen to contain 3.
contains (Zone low high) point | low == high && high == point = True
contains (Zone low high) point = within low point high

-- Subset uses within for the lower bound and contained_by for the upper bound
-- This is to get the desired properties on a few different cases:
-- 1) Zones are subsets of themselves. Low <= Low && High >= High
--    This requires an inclusive lower bound and and inclusive upper bound
-- 2) Zones contain things equal to the lower end.
--    Low <= Low && Low <= High
-- 3) Zone don't contain things equal to the higher end.
--    High >= Low && High < High
-- That explanation didn't really work out as well outside my head...
-- Anyway, this is what I want.
subset :: (Betweenable a, Eq a) => (Zone a) -> (Zone a) -> Bool
-- I encode equality specifically both to make it obvious, but also because (Zone 3 4) (Zone 3 4) works without it, but (Zone 3 3) (Zone 3 3) did not
subset big_zone small_zone | big_zone == small_zone = True
subset (Zone bl bu) (Zone sl su) = and [within bl sl bu, contained_by bl su bu]

-- This function returns True if two zones intersect
-- Two zones are considered to intersect iff every dimension has overlap
-- In this case, we use overlap_exclusive when comparing high points to ranges, and overlap when comparing low points.
-- The reason is that we want (overlap low low high) to be true, and (overlap low high high) to be false
-- That's because zones in this system are in general inclusive in the lower end and exclusive in the upper
intersect :: (Betweenable a, Eq a) => (Zone a) -> (Zone a) -> Bool
-- See the comments for subset's equality case
intersect zone1 zone2 | zone1 == zone2 = True
intersect (Zone lower1 upper1) (Zone lower2 upper2) = (dimensions lower1) == (size dims)
	where
	dims = unions [overlap lower1 lower2 upper1, overlap lower2 lower1 upper2, overlap_exclusive lower1 upper2 upper1, overlap_exclusive lower2 upper1 upper2]
