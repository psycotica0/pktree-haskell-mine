module Data.PkTree (PkTree, empty,
	insert, delete, move,
	lookup_zone, find_nearest,
	) where
import Data.Tree (Tree(Node), rootLabel)
import Data.Zone (Zone(Zone), subset, contains, divide_zone_by)
import Data.Divisible (Divisible)
import Data.Betweenable (Betweenable)
import Data.Offsetable (Offsetable)

import Data.Bool.HT (if')
import Data.List (partition)

unless c = flip $ if' c

data PkNode point payload = Instantiable (Zone point) | NonInstantiable (Zone point) | Leaf point payload deriving (Show, Eq)

pk_zone :: (PkNode a b) -> Zone a
pk_zone (Instantiable zone) = zone
pk_zone (NonInstantiable zone) = zone
pk_zone (Leaf point _) = Zone point point

type PkTree zone payload = Tree (PkNode zone payload)

node_zone = pk_zone.rootLabel

empty :: (Zone a) -> PkTree a b
empty zone = Node (NonInstantiable zone) []

insert :: (Offsetable point, Divisible point, Eq point, Betweenable point) => Int -> point -> point -> payload -> PkTree point payload -> PkTree point payload
insert k r point payload = alter k r (const $ Just payload) point

delete :: (Offsetable point, Divisible point, Eq point, Betweenable point) => Int -> point -> point -> PkTree point payload -> PkTree point payload
delete k r = alter k r (const Nothing)

-- This function is just a convenience around fetching a key, then if it exists removing it, and adding it elsewhere
move :: (Offsetable point, Divisible point, Eq point, Betweenable point) => Int -> point -> point -> PkTree point payload -> PkTree point payload
move = undefined

-- This function returns the payloads of every item in the zone
lookup_zone :: Zone point -> PkTree point payload -> [payload]
lookup_zone = undefined

-- This function finds the nearest points to the given point, using the given function to compute distance
-- It returns a tuple containing the smallest distance, and a list of all the payloads that distance from the point
-- If the type for distance doesn't reliably support Eq, like floating point numbers, then there will likely be only one item in the return list
-- That one will be the one with the smallest distance
-- If, though, the distance function computes manhattan distance or something, and returns an Int, then if more than one has the smallest, then they will all be returned
find_nearest :: (Ord distance) => (point -> Zone point -> distance) -> point -> PkTree point payload -> (distance, [payload])
find_nearest = undefined

-- This function returns the input node as the only item in the list if it's instantiable, or its children if it's not.
-- This is so I can just wrap results in this and do ((check_instantiable result) ++ children) and then I don't have NonInstantiable children.
-- (Assuming all children of this node are instantiable, which I am)
check_instantiable :: (PkTree zone payload) -> [(PkTree zone payload)]
check_instantiable (Node (NonInstantiable _) children) = children
check_instantiable node = [node]

-- This function encapsulates the logic of the PK Tree.
-- It allows one to implement insert or delete or adjust on top of it
-- It works similarly to the alter function in Map.
-- Essentially, given a point, it passes in the value at that point if it exists, or Nothing if it doesn't, to the function provided
-- If that function returns a payload, it is set at that point, if it returns Nothing then it's deleted (Or left empty if it never existed)
alter :: (Offsetable point, Divisible point, Eq point, Betweenable point) => Int -> point -> (Maybe payload -> Maybe payload) -> point -> PkTree point payload -> PkTree point payload
alter k r func point (Node pknode children) = build_node k r (pk_zone pknode) $ do_func $ partition is_contained children
	where
	is_contained child = contains (node_zone child) point
	-- This function handles the result of func
	handle_func children' Nothing = children'
	handle_func children' (Just payload) = (Node (Leaf point payload) []):children'
	-- This case is where none of the this point isn't any of the children
	do_func ([], children') = handle_func children' $ func Nothing
	-- This is the case where one of the children is the one we're looking for
	-- I've made it over explicit because I want it to fail quickly if my assumptions aren't true
	do_func ((Node (Leaf _ payload) []):[], children') = handle_func children' $ func $ Just payload
	-- This is the case where one of the children is the subdivision this point belongs in
	do_func (child:[], children') = (check_instantiable $ alter k r func point child) ++ children'

-- This function takes a label and a list of children and builds a Node out of it
-- That's not all, though.
-- It also takes a k and an r and creates subdivisions and tests them, to see if they are instantiable
-- It also checks itself, and either returns an Instantiable node or a NonInstantiable as necessary
-- While this node itself may be Instantiable or not, with a steady-state it should be the case that every node under this one should be either Instantiable or Leaf.
build_node :: (Offsetable point, Divisible point, Eq point, Betweenable point) => Int -> point -> Zone point -> [PkTree point a] -> PkTree point a
build_node k r zone children = unless (length children < k) check_subdivisions $ Node (NonInstantiable zone) children
	where
	check_subdivisions = Node ((if' (length children' < k) NonInstantiable Instantiable) zone) children'
	children' = concatMap check_instantiable $ map (\sub -> build_node k r sub $ filter ((subset sub).node_zone) children) $ zone `divide_zone_by` r

