module Data.PkTree where
import Data.Tree
import Data.Zone
import Data.Divisible
import Data.Betweenable

data PkNode zone payload = Instantiable {pk_zone :: (Zone zone)} | NonInstantiable {pk_zone :: (Zone zone)} | Leaf {pk_zone :: (Zone zone), payload :: payload} deriving (Show, Eq)

type PkTree zone payload = Tree (PkNode zone payload)

data PkConfiguration zone = PkConfiguration {config_k :: Int, config_r :: zone} deriving (Show, Eq)

empty_tree :: (Zone a) -> PkTree a Int
empty_tree zone = Node (NonInstantiable zone) []

insert :: (Divisible zone, Betweenable zone) => (PkTree zone payload) -> (PkConfiguration zone) -> (Zone zone) -> payload -> (PkTree zone payload)
insert tree config zone payload = insert_node tree config (Node (Leaf zone payload) [])

insert_node :: (Divisible zone, Betweenable zone) => (PkTree zone payload) -> (PkConfiguration zone) -> (PkTree zone payload) -> (PkTree zone payload)
insert_node tree config node =

-- If it's contained by one of my children, then recursively insert it there
-- Elseif it's instantiable, insert it as one of my children
-- Else insert its children as my children, not checking containment
	find (\point -> ((pk_zone.rootLabel) tree) `subset` point) (map (pk_zone.rootLabel) (subForest tree))
	-- This gives us either the child that contains it, or Nothing.
	-- In Nothing we insert it into the subForest
	-- Otherwise we insert_node it into the one we return.
	-- I still want the child, though...
	-- I'm thinking I might do this instead:
	any (\point -> ((pk_zone.rootLabel) tree) `subset` point) (map (pk_zone.rootLabel) (subForest tree))
	-- Which returns True if it's in any of the subchildren, or False otherwise
	-- Then:
	let maybe_insert big_node small_node | (((pk_zone.rootLabel) big_node) `subset` ((pk_zone.rootLabel) small_node)) = insert_node big_node config small_node;
		maybe_insert big_node _ = big_node; in
	result True point = map (\n -> maybe_insert n point) (subForest tree)
	result False (Node (NonInstantiable _) children) = (subForest tree) ++ children
	result False node = node:(subForest tree)
	-- So, I'm doing the subset check twice, but I like the algorithm cleanness behind it...
	-- The issue is that otherwise I need to replace children....
	-----------------------------------------------------------------------------------------
	-- Oh!
	-- Back to the first way, using the find:
	let replace x f z | x == z = f z;
		replace x y z = z;
		in
	result (Just child) point = map (replace child (\x -> insert_node x config point)) (subForest tree)
	result Nothing (Node (NonInstantiable _) children) = (subForest tree) ++ children
	result Nothing node = node:(subForest tree)
	-- So, if there is a match, then it goes through the list of children and when it finds one that's the one we're looking for we insert into there first
	-- The rest of them we just pass through
	-- If there isn't a match, but it's a NonInstantiable node, then we include its children as our children
	-- If there isn't a match and it's some other node, then we include it as our child.
	-- This one's not necessarily better, but I think I like it better than running any and inserting on the one we've got.
	-- I seem to recall from another thing that running == on trees blows because it recurses.
	-- If that's the case, then I should make replace into something that needs a PkTree and does == on the zone it holds.
	-- That's really what I want anyway...
-- If I have fewer than K children, return a NonInstantiable node and be done
	fewer_than_k tree k | (length $ subForest tree) < k = Node (NonInstantiable (pk_zone tree)) (subForest tree)
	fewer_than_k tree _ = the_rest_of_the_stuff tree
-- Make a list of my subdivisions as PkTrees
	map (\zone -> Node (NonInstantiable zone) []) $ ((pk_zone.rootLabel) tree) `divide_zone_by` (config_r config)
-- Insert each of my children into one of the subdivisions using insert_node as a new list of children.
	-- Looks like this could use the find a match and replace it think I had up there. Or, the maybe_insert.
	-- Which ever method I used to either insert something into the list if the point contained it should go here too.
	-- I'll put it in a fold, me thinks, and interate over all children, maintaining this list of subdivision children.
-- Go through each of these children and, for any NonInstantiable child, add its children as my children
	-- I'm thinking here I can just have a list of subdivisions, which I get out of the last step
	-- Then I fold over them again with something like this:
	only_instantiable (Node (NonInstantiable _) children) acc = children ++ acc
	only_instantiable node acc = node:acc
-- If I have fewer than K chikldren, return a NonInstantiable node and be done
	fewer_than_k_again tree k | (length $ subForest tree) < k = Node (NonInstantiable (pk_zone tree)) (subForest tree)
	fewer_than_k_again tree _ = Node (Instantiable (pk_zone tree)) (subForest tree)
