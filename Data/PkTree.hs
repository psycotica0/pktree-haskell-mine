module Data.PkTree where
import Data.Tree
import Data.Zone
import Data.Divisible
import Data.Betweenable
import Data.Offsetable
import qualified Data.List as List (find)

data PkNode zone payload = Instantiable {pk_zone :: (Zone zone)} | NonInstantiable {pk_zone :: (Zone zone)} | Leaf {pk_zone :: (Zone zone), payload :: payload} deriving (Show, Eq)

type PkTree zone payload = Tree (PkNode zone payload)

data PkConfiguration zone = PkConfiguration {config_k :: Int, config_r :: zone} deriving (Show, Eq)

node_zone = pk_zone.rootLabel

empty_tree :: (Zone a) -> PkTree a b
empty_tree zone = Node (NonInstantiable zone) []

insert :: (Divisible point, Betweenable point, Eq point, Offsetable point) => (PkTree point payload) -> (PkConfiguration point) -> point -> payload -> (PkTree point payload)
insert tree config point payload = insert_zone tree config (Zone point point) payload

insert_zone :: (Divisible zone, Betweenable zone, Eq zone, Offsetable zone) => (PkTree zone payload) -> (PkConfiguration zone) -> (Zone zone) -> payload -> (PkTree zone payload)
insert_zone tree config zone payload = insert_node tree config (Node (Leaf zone payload) [])

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
insert_into_children :: (Divisible zone, Betweenable zone, Eq zone, Offsetable zone) => ((PkTree zone payload) -> [(PkTree zone payload)]) -> (PkConfiguration zone) -> [(PkTree zone payload)] -> (PkTree zone payload) -> [(PkTree zone payload)]
insert_into_children func config children input = insert_or_not $ List.find (\child -> subset (node_zone child) (node_zone input)) children
	where
	insert_or_not Nothing = input:children
	insert_or_not (Just child) = (func $ insert_node child config input) ++ filter (\i -> (node_zone i) /= (node_zone child)) children


insert_node :: (Divisible zone, Betweenable zone, Eq zone, Offsetable zone) => (PkTree zone payload) -> (PkConfiguration zone) -> (PkTree zone payload) -> (PkTree zone payload)
insert_node (Node pknode children) config node = are_we_done $ insert_into_children check_instantiable config children node
	where
	are_we_done children | (length children) < (config_k config) = Node (NonInstantiable (pk_zone pknode)) children
	are_we_done children = what_are_we $ check_subdivisions children
	check_subdivisions children = concatMap check_instantiable $ populate_subdivisions children
	populate_subdivisions = foldr (flip $ insert_into_children return config) $ map build_node_from_subdivision $ (pk_zone pknode) `divide_zone_by` (config_r config)
	build_node_from_subdivision zone = Node (NonInstantiable zone) []
	what_are_we children | (length children) < (config_k config) = Node (NonInstantiable (pk_zone pknode)) children
	what_are_we children = Node (Instantiable (pk_zone pknode)) children
