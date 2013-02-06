module Data.Divisible (
	Divisible,
	divide_into_upper, divide_into_lower,
	upper_from_generator, lower_from_generator,
	fractional_divide_into_upper, fractional_divide_into_lower,
	integral_divide_into_upper, integral_divide_into_lower,
	) where

import Control.Applicative ((<*>), (<$>))
import Data.Traversable (mapAccumL)

lower_from_generator :: (Num a) => (a -> Int -> [a]) -> a -> Int -> [a]
lower_from_generator generator a b = take b $ scanl (+) 0 $ generator a b

upper_from_generator :: (Num a) => (a -> Int -> [a]) -> a -> Int -> [a]
upper_from_generator generator a b = take b $ scanl1 (+) $ generator a b

fractional_generator :: (Fractional a, RealFrac a) => a -> Int -> [a]
fractional_generator a b = repeat $ a / (fromIntegral b)

fractional_divide_into_lower :: (Fractional a, RealFrac a) => a -> a -> [a]
fractional_divide_into_lower a b = lower_from_generator fractional_generator a (truncate b)

fractional_divide_into_upper :: (Fractional a, RealFrac a) => a -> a -> [a]
fractional_divide_into_upper a b = upper_from_generator fractional_generator a (truncate b)

integral_generator :: (Integral a) => a -> Int -> [a]
integral_generator a b = snd $ mapAccumL iterator a $ reverse [1..(fromIntegral b)]
	where
	iterator acc v = let d = div acc v in (acc - d, d)

integral_divide_into_lower :: (Integral a) => a -> a -> [a]
integral_divide_into_lower a b = lower_from_generator integral_generator a (fromIntegral b)

integral_divide_into_upper :: (Integral a) => a -> a -> [a]
integral_divide_into_upper a b = upper_from_generator integral_generator a (fromIntegral b)

class Divisible a where
	-- This spits out a list of "break points" if you divide left side by right
	-- For example, 8 `divide_into_upper` 4 => [2, 4, 6, 8], representing the 4 "blocks" of 8.
	-- For example, 8 `divide_into_lower` 4 => [0, 2, 4, 6], as the lower sides of those blocks
	-- These are obviously not the sizes of the blocks, but rather their position wrt 8.
	-- For a different example: (4, 2) `divide_into_upper` (2, 2) => [(2,1), (2,2), (4,1), (4,2)]
	divide_into_upper :: a -> a -> [a]
	divide_into_lower :: a -> a -> [a]

instance Divisible Double where
	divide_into_lower = fractional_divide_into_lower
	divide_into_upper = fractional_divide_into_upper

instance Divisible Integer where
	divide_into_lower = integral_divide_into_lower
	divide_into_upper = integral_divide_into_upper

instance Divisible Int where
	divide_into_lower = integral_divide_into_lower
	divide_into_upper = integral_divide_into_upper

instance (Divisible a, Divisible b) => Divisible (a, b) where
	divide_into_upper (a, b) (c, d) = (,) <$> (divide_into_upper a c) <*> (divide_into_upper b d)
	divide_into_lower (a, b) (c, d) = (,) <$> (divide_into_lower a c) <*> (divide_into_lower b d)
