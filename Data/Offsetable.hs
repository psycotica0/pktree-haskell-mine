module Data.Offsetable where

class Offsetable a where
	-- I don't need all the crap that Num gives us, and overloading things into num feels bad...
	-- So, instead I've made this.
	-- It just takes one thing and moves it up by another, and back down by another.
	-- That's all
	(*+*) :: a -> a -> a
	(*-*) :: a -> a -> a

instance Offsetable Double where
	(*+*) = (+)
	(*-*) = (-)

instance Offsetable Integer where
	(*+*) = (+)
	(*-*) = (-)

instance Offsetable Int where
	(*+*) = (+)
	(*-*) = (-)

instance (Offsetable a, Offsetable b) => Offsetable (a, b) where
	(a, b) *+* (c, d) = (a *+* c, b *+* d)
	(a, b) *-* (c, d) = (a *-* c, b *-* d)
