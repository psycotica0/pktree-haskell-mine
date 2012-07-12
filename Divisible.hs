module Data.Divisible where
import Control.Applicative

class Divisible a where
	(//) :: a -> a -> a
	divide_into :: a -> Int -> [a]

instance Divisible Double where
	(//) = (/)
	a `divide_into` 0 = []
	a `divide_into` b = take b $ repeat $ a / (fromInteger $ toInteger b)

instance Divisible Integer where
	(//) = div
	a `divide_into` 0 = []
	a `divide_into` b = (a // (toInteger b)) : (divide_into (a - (a // (toInteger b))) (b-1))

instance Divisible Int where
	(//) = div
	a `divide_into` 0 = []
	a `divide_into` b = (a // b) : (divide_into (a - (a // b)) (b-1))

instance (Divisible a, Divisible b) => Divisible (a, b) where
	(a, b) // (c, d) = (a // c, b // d)
	(a, b) `divide_into` c = (,) <$> (a `divide_into` c) <*> (b `divide_into` c)
