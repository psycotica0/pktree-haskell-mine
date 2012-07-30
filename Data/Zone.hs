module Data.Zone where
import Data.Divisible
import Data.Offsetable
import Data.Betweenable

data Zone a = Zone {lower_bound :: a, upper_bound :: a} deriving (Show, Read, Eq)

divide_zone_by :: (Divisible a, Offsetable a) => Zone a -> a -> [Zone a]
zone `divide_zone_by` a = zipWith Zone lowers uppers
	where
	lowers = offset_func ((flip divide_into_lower) a)
	uppers = offset_func ((flip divide_into_upper) a)
	offset_func f = map (*+* (lower_bound zone)) $ f ((upper_bound zone) *-* (lower_bound zone))

contains :: (Betweenable a) => Zone a -> a -> Bool
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
subset :: (Betweenable a) => (Zone a) -> (Zone a) -> Bool
big_zone `subset` small_zone = (within (lower_bound big_zone) (lower_bound small_zone) (upper_bound big_zone)) && (contained_by (lower_bound big_zone) (upper_bound small_zone) (upper_bound big_zone))
