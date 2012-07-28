module Data.Divisible where
import Control.Applicative

class Divisible a where
	-- This just abstracts division so 4 // 3 == 1 and 4.0 // 3.0 == 1.33333
	(//) :: a -> a -> a
	-- This divides one number into the other number equal parts.
	-- The return list is the sizes of each part.
	-- For example: 8 `divide_into` 4 => [2, 2, 2, 2]
	divide_into :: a -> a -> [a]
	-- This spits out a list of "break points" if you divide left side by right
	-- For example, 8 `divide_into_upper` 4 => [2, 4, 6, 8], representing the 4 "blocks" of 8.
	-- For example, 8 `divide_into_lower` 4 => [0, 2, 4, 6], as the lower sides of those blocks
	-- These are obviously not the sizes of the blocks, but rather their position wrt 8.
	-- For a different example: (4, 2) `divide_into_upper` (2, 2) => [(2,1), (2,2), (4,1), (4,2)]
	divide_into_upper :: a -> a -> [a]
	divide_into_lower :: a -> a -> [a]

instance Divisible Double where
	(//) = (/)
	a `divide_into` 0 = []
	a `divide_into` b = take (truncate b) $ repeat $ a // (fromInteger $ truncate b)
	a `divide_into_upper` b = tail $ scanl (+) 0 $ a `divide_into` b
	a `divide_into_lower` b = init $ scanl (+) 0 $ a `divide_into` b

instance Divisible Integer where
	(//) = div
	a `divide_into` 0 = []
	a `divide_into` b = (a // b) : (divide_into (a - (a // b)) (b-1))
	a `divide_into_upper` b = tail $ scanl (+) 0 $ a `divide_into` b
	a `divide_into_lower` b = init $ scanl (+) 0 $ a `divide_into` b

instance Divisible Int where
	(//) = div
	a `divide_into` 0 = []
	a `divide_into` b = (a // b) : (divide_into (a - (a // b)) (b-1))
	a `divide_into_upper` b = tail $ scanl (+) 0 $ a `divide_into` b
	a `divide_into_lower` b = init $ scanl (+) 0 $ a `divide_into` b

instance (Divisible a, Divisible b) => Divisible (a, b) where
	(a, b) // (c, d) = (a // c, b // d)
	(a, b) `divide_into` (c, d) = (,) <$> (a `divide_into` c) <*> (b `divide_into` d)
	(a, b) `divide_into_upper` (c, d) = (,) <$> (a `divide_into_upper` c) <*> (b `divide_into_upper` d)
	(a, b) `divide_into_lower` (c, d) = (,) <$> (a `divide_into_lower` c) <*> (b `divide_into_lower` d)
