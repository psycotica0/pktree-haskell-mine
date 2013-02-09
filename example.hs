import Data.PkTree (PkTree, empty, insert, lookup_point, move, delete, lookup_zone)
import Data.Zone (Zone(Zone))
import Data.Tree (drawTree)

print_tree = putStrLn.drawTree.(fmap show)

k = 3
r = (2,2)

main = do
	putStrLn "Tree:"
	print_tree result
	putStrLn "Value at (4,4)"
	print $ lookup_point (4,4) result
	putStrLn "Moving (3,3) to (30,30)"
	print_tree $ move k r (3,3) (30,30) result
	putStrLn "Deleting (3,3)"
	print_tree $ delete k r (3,3) result
	putStrLn "Finding all in zone (4,4) (15,15)"
	print $ lookup_zone (Zone (4,4) (15,15)) result

root :: PkTree (Int,Int) [Char]
root = empty (Zone (0,0) (100, 100))

points = [((2,2), "Hello!"), ((3,3), "Yo!"), ((4,4), "Dude!"), ((10, 7), "I'm an outcast!")]

result = foldr (uncurry $ insert k r) root points
