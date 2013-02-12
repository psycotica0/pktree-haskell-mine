module Data.Zone (
	Zone(Zone), contains, subset, intersect,
	Divisible, divide_zone_by,
	integral_divide_zone_by, fractional_divide_zone_by, enum_divide_zone_by,
	) where

import Data.Betweenable (Betweenable, overlap, within, contained_by, overlap_exclusive, dimensions)

import Data.Set (size, unions)
import Data.Traversable (mapAccumL)
import Data.List (scanl)
import Control.Applicative ((<$>), (<*>))

data Zone a = Zone a a deriving (Show, Read, Eq)

contains :: (Betweenable a, Eq a) => Zone a -> a -> Bool
-- I've had to add a special case so that (Zone 3 3) is seen to contain 3.
contains (Zone low high) point = or [and [low == high, high == point], within low point high]

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
subset bz@(Zone bl bu) sz@(Zone sl su) = or [bz == sz, and [within bl sl bu, contained_by bl su bu]]

-- This function returns True if two zones intersect
-- Two zones are considered to intersect iff every dimension has overlap
-- In this case, we use overlap_exclusive when comparing high points to ranges, and overlap when comparing low points.
-- The reason is that we want (overlap low low high) to be true, and (overlap low high high) to be false
-- That's because zones in this system are in general inclusive in the lower end and exclusive in the upper
intersect :: (Betweenable a, Eq a) => (Zone a) -> (Zone a) -> Bool
-- See the comments for subset's equality case
intersect zone1@(Zone lower1 upper1) zone2@(Zone lower2 upper2) = or [zone1 == zone2, (dimensions lower1) == (size dims)]
	where
	dims = unions [overlap lower1 lower2 upper1, overlap lower2 lower1 upper2, overlap_exclusive lower1 upper2 upper1, overlap_exclusive lower2 upper1 upper2]

class Divisible a where
	divide_zone_by :: Zone a -> a -> [Zone a]

fractional_divide_zone_by :: (RealFrac a, Fractional a) => Zone a -> a -> [Zone a]
fractional_divide_zone_by (Zone l u) d = take d' $ fmap (\l' -> Zone l' $ l' + block_size) $ scanl (+) l $ repeat block_size
	where
	d' = truncate d
	block_size = (u - l) / (fromIntegral d')

instance Divisible Double where
	divide_zone_by = fractional_divide_zone_by

integral_divide_zone_by :: (Integral a) => Zone a -> a -> [Zone a]
integral_divide_zone_by (Zone l u) d = snd $ mapAccumL iterator l [d,(d-1)..1]
	where
	iterator l' d' = let u' = ((u - l') `div` d') + l' in (u', Zone l' u')

instance Divisible Int where
	divide_zone_by = integral_divide_zone_by

instance Divisible Integer where
	divide_zone_by = integral_divide_zone_by

instance (Divisible a, Divisible b) => Divisible (a, b) where
	divide_zone_by (Zone (la, lb) (ua, ub)) (da, db) = combine <$> (divide_zone_by (Zone la ua) da) <*> (divide_zone_by (Zone lb ub) db)
		where
		combine (Zone l1 u1) (Zone l2 u2) = Zone (l1, l2) (u1, u2)

-- So... this is probably the most efficient, but seems kinda dirty...
enum_divide_zone_by :: (Enum a) => Zone a -> a -> [Zone a]
enum_divide_zone_by (Zone l u) d = fmap (\(Zone l' u') -> Zone (toEnum l') (toEnum u')) $ integral_divide_zone_by (Zone (fromEnum l) (fromEnum u)) (fromEnum d)
