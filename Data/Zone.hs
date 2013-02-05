module Data.Zone where
import Data.Divisible
import Data.Offsetable
import Data.Betweenable

import Data.Set (size, unions)

data Zone a = Zone {lower_bound :: a, upper_bound :: a} deriving (Show, Read, Eq)

divide_zone_by :: (Divisible a, Offsetable a) => Zone a -> a -> [Zone a]
zone `divide_zone_by` a = zipWith Zone lowers uppers
	where
	lowers = offset_func ((flip divide_into_lower) a)
	uppers = offset_func ((flip divide_into_upper) a)
	offset_func f = map (*+* (lower_bound zone)) $ f ((upper_bound zone) *-* (lower_bound zone))

contains :: (Betweenable a, Eq a) => Zone a -> a -> Bool
-- I've had to add a special case so that (Zone 3 3) is seen to contain 3.
(Zone a b) `contains` c | a == b && b == c = True
zone `contains` value = within (lower_bound zone) value (upper_bound zone)

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
big_zone `subset` small_zone | big_zone == small_zone = True
big_zone `subset` small_zone = (within (lower_bound big_zone) (lower_bound small_zone) (upper_bound big_zone)) && (contained_by (lower_bound big_zone) (upper_bound small_zone) (upper_bound big_zone))

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
