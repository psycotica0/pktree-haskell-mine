module Data.PkTree where
import Data.Tree
import Data.Zone
import Data.Divisible
import Data.Betweenable
import qualified Data.List as List -- List.find

data PkNode zone payload = Instantiable {pk_zone :: (Zone zone)} | NonInstantiable {pk_zone :: (Zone zone)} | Leaf {pk_zone :: (Zone zone), payload :: payload} deriving (Show, Eq)

type PkTree zone payload = Tree (PkNode zone payload)

data PkConfiguration zone = PkConfiguration {config_k :: Int, config_r :: zone} deriving (Show, Eq)

node_zone = pk_zone.rootLabel

empty_tree :: (Zone a) -> PkTree a Int
empty_tree zone = Node (NonInstantiable zone) []

insert :: (Divisible zone, Betweenable zone, Eq zone) => (PkTree zone payload) -> (PkConfiguration zone) -> (Zone zone) -> payload -> (PkTree zone payload)
insert tree config zone payload = insert_node tree config (Node (Leaf zone payload) [])

-- This function returns the input node as the only item in the list if it's instantiable, or its children if it's not.
-- This is so I can just wrap results in this and do ((check_instantiable result) ++ children) and then I don't have NonInstantiable children.
-- (Assuming all children of this node are instantiable, which I am)
check_instantiable :: (PkTree zone payload) -> [(PkTree zone payload)]
check_instantiable (Node (NonInstantiable _) children) = children
check_instantiable node = [node]

-- This function takes in a list of nodes (children) and a new input node
-- Then if one of the children contains input, then it returns the list of children with input insert_node'd into that child
-- If none of the children contain it, then it returns the children as is with the input node as one of them
-- Also, it takes a function that goes from (a -> [a]) to run on the result of insert_node.
-- That can be check_instantiable if you want to make sure that the result is instantiable, or return if you don't
insert_into_children :: (Divisible zone, Betweenable zone, Eq zone) => ((PkTree zone payload) -> [(PkTree zone payload)]) -> (PkConfiguration zone) -> [(PkTree zone payload)] -> (PkTree zone payload) -> [(PkTree zone payload)]
insert_into_children func config children input = insert_or_not $ List.find (\child -> subset (node_zone child) (node_zone input)) children
	where
	insert_or_not Nothing = input:children
	insert_or_not (Just child) = (func $ insert_node child config input) ++ filter (\i -> (node_zone i) /= (node_zone child)) children


insert_node :: (Divisible zone, Betweenable zone, Eq zone) => (PkTree zone payload) -> (PkConfiguration zone) -> (PkTree zone payload) -> (PkTree zone payload)
insert_node (Node pknode children) config node = Node pknode $ insert_into_children check_instantiable config children node

---- If it's contained by one of my children, then recursively insert it there
---- Elseif it's instantiable, insert it as one of my children
---- Else insert its children as my children, not checking containment
--	find (\point -> ((pk_zone.rootLabel) tree) `subset` point) (map (pk_zone.rootLabel) (subForest tree))
--	-- This gives us either the child that contains it, or Nothing.
--	-- In Nothing we insert it into the subForest
--	-- Otherwise we insert_node it into the one we return.
--	-- I still want the child, though...
--	-- I'm thinking I might do this instead:
--	any (\point -> ((pk_zone.rootLabel) tree) `subset` point) (map (pk_zone.rootLabel) (subForest tree))
--	-- Which returns True if it's in any of the subchildren, or False otherwise
--	-- Then:
--	let maybe_insert big_node small_node | (((pk_zone.rootLabel) big_node) `subset` ((pk_zone.rootLabel) small_node)) = insert_node big_node config small_node;
--		maybe_insert big_node _ = big_node; in
--	result True point = map (\n -> maybe_insert n point) (subForest tree)
--	result False (Node (NonInstantiable _) children) = (subForest tree) ++ children
--	result False node = node:(subForest tree)
--	-- So, I'm doing the subset check twice, but I like the algorithm cleanness behind it...
--	-- The issue is that otherwise I need to replace children....
--	-----------------------------------------------------------------------------------------
--	-- Oh!
--	-- Back to the first way, using the find:
--	let replace x f z | x == z = f z;
--		replace x y z = z;
--		in
--	result (Just child) point = map (replace child (\x -> insert_node x config point)) (subForest tree)
--	result Nothing (Node (NonInstantiable _) children) = (subForest tree) ++ children
--	result Nothing node = node:(subForest tree)
--	-- So, if there is a match, then it goes through the list of children and when it finds one that's the one we're looking for we insert into there first
--	-- The rest of them we just pass through
--	-- If there isn't a match, but it's a NonInstantiable node, then we include its children as our children
--	-- If there isn't a match and it's some other node, then we include it as our child.
--	-- This one's not necessarily better, but I think I like it better than running any and inserting on the one we've got.
--	-- I seem to recall from another thing that running == on trees blows because it recurses.
--	-- If that's the case, then I should make replace into something that needs a PkTree and does == on the zone it holds.
--	-- That's really what I want anyway...
---- If I have fewer than K children, return a NonInstantiable node and be done
--	fewer_than_k tree k | (length $ subForest tree) < k = Node (NonInstantiable (pk_zone tree)) (subForest tree)
--	fewer_than_k tree _ = the_rest_of_the_stuff tree
---- Make a list of my subdivisions as PkTrees
--	map (\zone -> Node (NonInstantiable zone) []) $ ((pk_zone.rootLabel) tree) `divide_zone_by` (config_r config)
---- Insert each of my children into one of the subdivisions using insert_node as a new list of children.
--	-- Looks like this could use the find a match and replace it think I had up there. Or, the maybe_insert.
--	-- Which ever method I used to either insert something into the list if the point contained it should go here too.
--	-- I'll put it in a fold, me thinks, and interate over all children, maintaining this list of subdivision children.
---- Go through each of these children and, for any NonInstantiable child, add its children as my children
--	-- I'm thinking here I can just have a list of subdivisions, which I get out of the last step
--	-- Then I fold over them again with something like this:
--	only_instantiable (Node (NonInstantiable _) children) acc = children ++ acc
--	only_instantiable node acc = node:acc
---- If I have fewer than K chikldren, return a NonInstantiable node and be done
--	fewer_than_k_again tree k | (length $ subForest tree) < k = Node (NonInstantiable (pk_zone tree)) (subForest tree)
--	fewer_than_k_again tree _ = Node (Instantiable (pk_zone tree)) (subForest tree)
