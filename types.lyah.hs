-- binary tree implementation
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x < a = Node a (treeInsert x left) right
	| x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
	| x == a = True 
	| x < a = treeElem x left
	| x > a = treeElem x right
--Hey thats pretty nifty!

instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)



data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
	Red == Red = True
	Green == Green = True
	Yellow == Yellow = True
	_ == _ = False	

instance Show TrafficLight where
	show Red = "Red light"
	show Yellow = "Yellow light"
	show Green = "Green light"
