module Data.Betweenable where

-- Like Ord, but more generic I guess, determines if one value is between two others
class Betweenable a where
	-- Returns true if first < second < third
	between :: a -> a -> a -> Bool
	-- Returns true if first <= second < third
	within :: a -> a -> a -> Bool
	-- Returns true if first <= second <= third
	contained_by :: a -> a -> a -> Bool

instance Betweenable Double where
	between a b c = (a < b) && (b < c)
	within a b c = (a <= b) && (b < c)
	contained_by a b c = (a <= b) && (b <= c)

instance Betweenable Integer where
	between a b c = (a < b) && (b < c)
	within a b c = (a <= b) && (b < c)
	contained_by a b c = (a <= b) && (b <= c)

instance Betweenable Int where
	between a b c = (a < b) && (b < c)
	within a b c = (a <= b) && (b < c)
	contained_by a b c = (a <= b) && (b <= c)

instance (Betweenable a, Betweenable b) => Betweenable (a, b) where
	between (a, b) (c, d) (e, f) = (between a c e) && (between b d f)
	within (a, b) (c, d) (e, f) = (within a c e) && (within b d f)
	contained_by (a, b) (c, d) (e, f) = (contained_by a c e) && (contained_by b d f)
