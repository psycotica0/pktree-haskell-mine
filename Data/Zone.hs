module Data.Zone where
import Data.Divisible
import Data.Offsetable

data Zone a = Zone {lower_bound :: a, upper_bound :: a} deriving (Show, Read, Eq)

divide_zone_by :: (Divisible a, Offsetable a) => Zone a -> a -> [Zone a]
zone `divide_zone_by` a = zipWith Zone lowers uppers
	where
	lowers = offset_func ((flip divide_into_lower) a)
	uppers = offset_func ((flip divide_into_upper) a)
	offset_func f = map (*+* (lower_bound zone)) $ f ((upper_bound zone) *-* (lower_bound zone))
