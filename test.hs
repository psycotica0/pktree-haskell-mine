import Data.PkTree
import Data.Zone
import Data.Tree as Tree

main = putStrLn $ Tree.drawTree $ fmap show result

config = PkConfiguration 3 (2,2)
root :: PkTree (Int,Int) [Char]
root = empty_tree (Zone (0,0) (100, 100))

points = [((2,2), "Hello!"), ((3,3), "Yo!"), ((4,4), "Dude!")]

result = foldr (\(pos, payload) acc -> insert acc config pos payload) root points
