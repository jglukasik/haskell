-- Homework 4

convert :: (Double, [Char]) -> (Double, [Char])
convert (n, unit) 
	| unit == "m"  = (n * 1.09361, "yd")
	| unit == "yd" = (n / 1.09361, "m")
	| unit == "L"   = (n * 0.264172, "gal")
	| unit == "gal" = (n / 0.264172, "L")
	| unit == "kg" = (n * 2.20462, "lb")
	| unit == "lb" = (n / 2.20462, "kg")
-- wow, i thought i was doing this all wrong, but i guess it was just a pattern matching/guard excercise. add a catchall though!
	| otherwise = error "invalid units"


-- Homework 5

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

singleton :: a -> Tree a
singleton n = Node n (EmptyTree) (EmptyTree)

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right) 
  | x == y =  Node x left right
  | x < y = Node y (treeInsert x left) right
  | x > y = Node y left (treeInsert x right)

add :: Num a => Tree a -> a
add EmptyTree = 0
add (Node n left right) = n + (add left) + (add right)
